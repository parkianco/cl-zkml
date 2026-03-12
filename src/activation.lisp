;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Activation functions for zkML

(in-package #:cl-zkml)

;;; ============================================================================
;;; ReLU
;;; ============================================================================

(defun zkml-relu (tensor)
  "Apply ReLU activation: max(0, x).
   In field arithmetic, we check if x is in the positive half."
  (let* ((size (zkml-tensor-size tensor))
         (data (zkml-tensor-data tensor))
         (half-prime (floor +zkml-field-prime+ 2))
         (result-data (make-array size)))
    (loop for i from 0 below size
          for val = (aref data i) do
      ;; Positive if in lower half of field
      (setf (aref result-data i)
            (if (<= val half-prime) val 0)))
    (%make-zkml-tensor
     :shape (zkml-tensor-shape tensor)
     :data result-data
     :strides (zkml-tensor-strides tensor))))

;;; ============================================================================
;;; Sigmoid Approximation
;;; ============================================================================

(defun zkml-sigmoid (tensor)
  "Apply sigmoid approximation using piecewise linear function.
   sigmoid(x) ~ 0.25*x + 0.5 for x in [-2, 2], clamped otherwise."
  (let* ((size (zkml-tensor-size tensor))
         (data (zkml-tensor-data tensor))
         (result-data (make-array size))
         (half (zkml-to-fixed 0.5))
         (quarter (zkml-to-fixed 0.25))
         (one (zkml-to-fixed 1.0))
         (two-fixed (zkml-to-fixed 2.0))
         (neg-two-fixed (zkml-to-fixed -2.0))
         (half-prime (floor +zkml-field-prime+ 2)))
    (loop for i from 0 below size
          for val = (aref data i)
          for signed-val = (if (> val half-prime)
                               (- val +zkml-field-prime+)
                               val)
          do
      (setf (aref result-data i)
            (cond
              ;; x >= 2: output 1
              ((>= signed-val (zkml-from-fixed two-fixed)) one)
              ;; x <= -2: output 0
              ((<= signed-val (zkml-from-fixed neg-two-fixed)) 0)
              ;; Linear region: 0.25*x + 0.5
              (t (zkml-field-add (zkml-fixed-mul val quarter) half)))))
    (%make-zkml-tensor
     :shape (zkml-tensor-shape tensor)
     :data result-data
     :strides (zkml-tensor-strides tensor))))

;;; ============================================================================
;;; Tanh Approximation
;;; ============================================================================

(defun zkml-tanh-approx (tensor)
  "Apply tanh approximation using piecewise linear function."
  (let* ((size (zkml-tensor-size tensor))
         (data (zkml-tensor-data tensor))
         (result-data (make-array size))
         (one (zkml-to-fixed 1.0))
         (neg-one (zkml-to-fixed -1.0)))
    (loop for i from 0 below size
          for val = (aref data i)
          for float-val = (zkml-from-fixed val) do
      (setf (aref result-data i)
            (cond
              ((>= float-val 1.0) one)
              ((<= float-val -1.0) neg-one)
              ;; Linear region
              (t val))))
    (%make-zkml-tensor
     :shape (zkml-tensor-shape tensor)
     :data result-data
     :strides (zkml-tensor-strides tensor))))

;;; ============================================================================
;;; Softmax
;;; ============================================================================

(defun zkml-softmax (tensor)
  "Apply softmax normalization.
   Uses simplified exp approximation for ZK circuits."
  (let* ((shape (zkml-tensor-shape tensor))
         (rows (if (= (length shape) 2) (first shape) 1))
         (cols (if (= (length shape) 2) (second shape) (first shape)))
         (data (zkml-tensor-data tensor))
         (result-data (make-array (zkml-tensor-size tensor))))
    ;; Process each row
    (loop for row from 0 below rows do
      (let ((row-start (* row cols))
            (exp-vals (make-array cols))
            (sum 0))
        ;; Compute exp approximation for each element
        (loop for j from 0 below cols
              for val = (aref data (+ row-start j))
              for exp-approx = (zkml-to-fixed (exp (zkml-from-fixed val)))
              do
          (setf (aref exp-vals j) exp-approx)
          (setf sum (zkml-field-add sum exp-approx)))
        ;; Normalize
        (loop for j from 0 below cols do
          (setf (aref result-data (+ row-start j))
                (if (zerop sum)
                    (zkml-to-fixed (/ 1.0 cols))
                    (zkml-fixed-div (aref exp-vals j) sum))))))
    (%make-zkml-tensor
     :shape shape
     :data result-data
     :strides (zkml-tensor-strides tensor))))

;;; ============================================================================
;;; GELU Approximation
;;; ============================================================================

(defun zkml-gelu-approx (tensor)
  "Apply GELU approximation: 0.5 * x * (1 + tanh(0.797885 * (x + 0.044715 * x^3))).
   Simplified to piecewise linear for ZK efficiency."
  (let* ((size (zkml-tensor-size tensor))
         (data (zkml-tensor-data tensor))
         (result-data (make-array size))
         (half (zkml-to-fixed 0.5))
         (half-prime (floor +zkml-field-prime+ 2)))
    (loop for i from 0 below size
          for val = (aref data i)
          for float-val = (zkml-from-fixed val) do
      (setf (aref result-data i)
            (cond
              ;; Approximate: x * sigmoid(1.702 * x)
              ((> val half-prime)
               ;; Negative: near zero
               (zkml-fixed-mul val (zkml-to-fixed 0.1)))
              ((< float-val 0.5)
               ;; Small positive: approximately x/2
               (zkml-fixed-mul val half))
              (t
               ;; Large positive: approximately x
               val))))
    (%make-zkml-tensor
     :shape (zkml-tensor-shape tensor)
     :data result-data
     :strides (zkml-tensor-strides tensor))))
