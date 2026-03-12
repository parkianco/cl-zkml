;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Tensor operations for zkML

(in-package #:cl-zkml)

;;; ============================================================================
;;; Tensor Structure
;;; ============================================================================

(defstruct (zkml-tensor (:constructor %make-zkml-tensor))
  "A multi-dimensional tensor in the field."
  (shape nil :type list)
  (data #() :type vector)
  (strides nil :type list))

(defun compute-strides (shape)
  "Compute strides for row-major ordering."
  (let ((strides nil)
        (stride 1))
    (loop for dim in (reverse shape) do
      (push stride strides)
      (setf stride (* stride dim)))
    strides))

(defun make-zkml-tensor (shape &key initial-element initial-contents)
  "Create a tensor with given shape."
  (let* ((size (reduce #'* shape :initial-value 1))
         (data (if initial-contents
                   (coerce (if (listp initial-contents)
                               (flatten-nested initial-contents)
                               initial-contents)
                           'vector)
                   (make-array size :initial-element (or initial-element 0)))))
    (when (/= (length data) size)
      (error 'zkml-error :message "Data size mismatch with shape"))
    (%make-zkml-tensor
     :shape shape
     :data data
     :strides (compute-strides shape))))

(defun flatten-nested (list)
  "Flatten a nested list."
  (cond ((null list) nil)
        ((atom list) (list list))
        (t (append (flatten-nested (car list))
                   (flatten-nested (cdr list))))))

(defun zkml-tensor-size (tensor)
  "Return total number of elements."
  (reduce #'* (zkml-tensor-shape tensor) :initial-value 1))

(defun compute-linear-index (indices strides)
  "Compute linear index from multi-dimensional indices."
  (loop for idx in indices
        for stride in strides
        sum (* idx stride)))

(defun zkml-tensor-get (tensor &rest indices)
  "Get element at indices."
  (let ((idx (compute-linear-index indices (zkml-tensor-strides tensor))))
    (aref (zkml-tensor-data tensor) idx)))

(defun zkml-tensor-set (tensor value &rest indices)
  "Set element at indices."
  (let ((idx (compute-linear-index indices (zkml-tensor-strides tensor))))
    (setf (aref (zkml-tensor-data tensor) idx)
          (mod value +zkml-field-prime+))))

(defun zkml-tensor-flatten (tensor)
  "Return flattened 1D tensor."
  (make-zkml-tensor (list (zkml-tensor-size tensor))
                    :initial-contents (zkml-tensor-data tensor)))

(defun zkml-tensor-reshape (tensor new-shape)
  "Reshape tensor to new shape."
  (let ((new-size (reduce #'* new-shape :initial-value 1))
        (old-size (zkml-tensor-size tensor)))
    (unless (= new-size old-size)
      (error 'zkml-error :message "Cannot reshape: size mismatch"))
    (%make-zkml-tensor
     :shape new-shape
     :data (zkml-tensor-data tensor)
     :strides (compute-strides new-shape))))

;;; ============================================================================
;;; Tensor Operations
;;; ============================================================================

(defun zkml-matmul (a b)
  "Matrix multiplication. A: (m x k), B: (k x n) -> (m x n)"
  (let* ((shape-a (zkml-tensor-shape a))
         (shape-b (zkml-tensor-shape b))
         (m (first shape-a))
         (k1 (second shape-a))
         (k2 (first shape-b))
         (n (second shape-b)))
    (unless (= k1 k2)
      (error 'zkml-error :message "Matrix dimension mismatch"))
    (let ((result (make-zkml-tensor (list m n))))
      (loop for i from 0 below m do
        (loop for j from 0 below n do
          (let ((sum 0))
            (loop for l from 0 below k1 do
              (setf sum (zkml-field-add
                         sum
                         (zkml-fixed-mul
                          (zkml-tensor-get a i l)
                          (zkml-tensor-get b l j)))))
            (zkml-tensor-set result sum i j))))
      result)))

(defun zkml-add-tensors (a b)
  "Element-wise addition of tensors."
  (let* ((shape (zkml-tensor-shape a))
         (size (zkml-tensor-size a))
         (data-a (zkml-tensor-data a))
         (data-b (zkml-tensor-data b))
         (result-data (make-array size)))
    (loop for i from 0 below size do
      (setf (aref result-data i)
            (zkml-field-add (aref data-a i) (aref data-b i))))
    (%make-zkml-tensor
     :shape shape
     :data result-data
     :strides (zkml-tensor-strides a))))

(defun zkml-hadamard (a b)
  "Element-wise multiplication (Hadamard product)."
  (let* ((shape (zkml-tensor-shape a))
         (size (zkml-tensor-size a))
         (data-a (zkml-tensor-data a))
         (data-b (zkml-tensor-data b))
         (result-data (make-array size)))
    (loop for i from 0 below size do
      (setf (aref result-data i)
            (zkml-fixed-mul (aref data-a i) (aref data-b i))))
    (%make-zkml-tensor
     :shape shape
     :data result-data
     :strides (zkml-tensor-strides a))))

(defun zkml-transpose (tensor)
  "Transpose a 2D tensor."
  (let* ((shape (zkml-tensor-shape tensor))
         (rows (first shape))
         (cols (second shape))
         (result (make-zkml-tensor (list cols rows))))
    (loop for i from 0 below rows do
      (loop for j from 0 below cols do
        (zkml-tensor-set result (zkml-tensor-get tensor i j) j i)))
    result))

(defun zkml-broadcast (tensor target-shape)
  "Broadcast tensor to target shape."
  (let* ((src-shape (zkml-tensor-shape tensor))
         (target-size (reduce #'* target-shape :initial-value 1))
         (result-data (make-array target-size)))
    ;; Simple broadcast for bias addition (1D to 2D row broadcast)
    (cond
      ((and (= (length src-shape) 1)
            (= (length target-shape) 2)
            (= (first src-shape) (second target-shape)))
       ;; Broadcast 1D to rows of 2D
       (let ((rows (first target-shape))
             (cols (second target-shape))
             (src-data (zkml-tensor-data tensor)))
         (loop for i from 0 below rows do
           (loop for j from 0 below cols do
             (setf (aref result-data (+ (* i cols) j))
                   (aref src-data j))))))
      (t
       ;; Default: just copy (no actual broadcast needed)
       (loop for i from 0 below (min target-size (zkml-tensor-size tensor)) do
         (setf (aref result-data i)
               (aref (zkml-tensor-data tensor) (mod i (zkml-tensor-size tensor)))))))
    (%make-zkml-tensor
     :shape target-shape
     :data result-data
     :strides (compute-strides target-shape))))
