;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Field arithmetic for zkML

(in-package #:cl-zkml)

;;; ============================================================================
;;; Error Condition
;;; ============================================================================

(define-condition zkml-error (error)
  ((message :initarg :message :reader zkml-error-message))
  (:report (lambda (c s) (format s "zkML Error: ~A" (zkml-error-message c)))))

;;; ============================================================================
;;; Field Constants
;;; ============================================================================

;; BN254 scalar field prime
(defconstant +zkml-field-prime+
  21888242871839275222246405745257275088548364400416034343698204186575808495617
  "The prime field modulus for zkML (BN254 scalar field).")

;;; ============================================================================
;;; Field Arithmetic
;;; ============================================================================

(defun zkml-field-add (a b)
  "Add two field elements."
  (mod (+ a b) +zkml-field-prime+))

(defun zkml-field-sub (a b)
  "Subtract two field elements."
  (mod (- a b) +zkml-field-prime+))

(defun zkml-field-mul (a b)
  "Multiply two field elements."
  (mod (* a b) +zkml-field-prime+))

(defun zkml-field-neg (a)
  "Negate a field element."
  (mod (- +zkml-field-prime+ a) +zkml-field-prime+))

(defun zkml-field-pow (base exp)
  "Exponentiation using square-and-multiply."
  (let* ((result 1)
         (b (mod base +zkml-field-prime+))
         (e exp))
    (loop while (> e 0) do
      (when (oddp e)
        (setf result (mod (* result b) +zkml-field-prime+)))
      (setf b (mod (* b b) +zkml-field-prime+))
      (setf e (ash e -1)))
    result))

(defun zkml-field-inv (a)
  "Compute multiplicative inverse using Fermat's little theorem."
  (when (zerop a)
    (error 'zkml-error :message "Cannot invert zero"))
  (zkml-field-pow a (- +zkml-field-prime+ 2)))

(defun zkml-field-div (a b)
  "Divide two field elements."
  (zkml-field-mul a (zkml-field-inv b)))

;;; ============================================================================
;;; Fixed-Point Arithmetic
;;; ============================================================================

;; 18-bit fractional part for sufficient precision
(defconstant +zkml-scale+ (ash 1 18)
  "Scale factor for fixed-point representation.")

(defun zkml-to-fixed (x)
  "Convert a floating-point number to fixed-point field element."
  (let ((scaled (round (* x +zkml-scale+))))
    (if (< scaled 0)
        (mod scaled +zkml-field-prime+)
        (mod scaled +zkml-field-prime+))))

(defun zkml-from-fixed (x)
  "Convert a fixed-point field element to floating-point."
  (let ((val (if (> x (/ +zkml-field-prime+ 2))
                 (- x +zkml-field-prime+)
                 x)))
    (/ val (float +zkml-scale+))))

(defun zkml-fixed-mul (a b)
  "Multiply two fixed-point numbers and rescale."
  (let ((product (zkml-field-mul a b)))
    ;; Divide by scale to maintain fixed-point format
    (zkml-field-div product +zkml-scale+)))

(defun zkml-fixed-div (a b)
  "Divide two fixed-point numbers."
  (let ((scaled-a (zkml-field-mul a +zkml-scale+)))
    (zkml-field-div scaled-a b)))
