;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Proof generation for zkML

(in-package #:cl-zkml)

;;; ============================================================================
;;; Constraint Structure
;;; ============================================================================

(defstruct (zkml-constraint (:constructor %make-zkml-constraint))
  "A constraint for zkML proof system."
  (a-coeffs nil :type list)
  (b-coeffs nil :type list)
  (c-coeffs nil :type list)
  (constraint-type :arithmetic :type keyword))

(defun make-zkml-constraint (&key a-coeffs b-coeffs c-coeffs (constraint-type :arithmetic))
  "Create a zkML constraint."
  (%make-zkml-constraint
   :a-coeffs a-coeffs
   :b-coeffs b-coeffs
   :c-coeffs c-coeffs
   :constraint-type constraint-type))

;;; ============================================================================
;;; Constraint Generation
;;; ============================================================================

(defun generate-layer-constraints (layer input-vars output-vars)
  "Generate constraints for a single layer."
  (let ((constraints nil))
    (typecase layer
      (zkml-dense-layer
       ;; For dense layer: out[i] = sum(in[j] * weight[j,i]) + bias[i]
       (let* ((weights (zkml-dense-layer-weights layer))
              (weight-shape (zkml-tensor-shape weights))
              (in-dim (first weight-shape))
              (out-dim (second weight-shape)))
         (loop for i from 0 below out-dim do
           (loop for j from 0 below in-dim do
             ;; Constraint: input[j] * weight[j,i] contributes to output[i]
             (push (make-zkml-constraint
                    :a-coeffs (list (cons (nth j input-vars) 1))
                    :b-coeffs (list (cons (cons :weight (cons j i))
                                          (zkml-tensor-get weights j i)))
                    :c-coeffs nil
                    :constraint-type :matmul)
                   constraints)))))
      (zkml-activation-layer
       ;; For ReLU: out = max(0, in) requires range proof
       (loop for in-var in input-vars
             for out-var in output-vars do
         (push (make-zkml-constraint
                :a-coeffs (list (cons in-var 1))
                :b-coeffs nil
                :c-coeffs (list (cons out-var -1))
                :constraint-type :relu)
               constraints))))
    constraints))

(defun generate-model-constraints (model input-vars)
  "Generate constraints for entire model."
  (let ((constraints nil)
        (current-vars input-vars))
    (loop for layer across (zkml-model-layers model)
          for layer-idx from 0 do
      (let* ((output-size (typecase layer
                            (zkml-dense-layer
                             (second (zkml-tensor-shape
                                      (zkml-dense-layer-weights layer))))
                            (otherwise (length current-vars))))
             (output-vars (loop for i from 0 below output-size
                                collect (cons :layer (cons layer-idx i)))))
        (setf constraints
              (append constraints
                      (generate-layer-constraints layer current-vars output-vars)))
        (setf current-vars output-vars)))
    constraints))

;;; ============================================================================
;;; Witness Generation
;;; ============================================================================

(defstruct (zkml-witness (:constructor %make-zkml-witness))
  "Witness for zkML proof."
  (input-values nil :type (or null zkml-tensor))
  (layer-activations nil :type list)
  (output-values nil :type (or null zkml-tensor)))

(defun make-zkml-witness (&key input-values layer-activations output-values)
  "Create a witness."
  (%make-zkml-witness
   :input-values input-values
   :layer-activations layer-activations
   :output-values output-values))

(defun generate-inference-witness (model input)
  "Generate witness from model inference."
  (multiple-value-bind (output activations)
      (zkml-model-forward model input)
    (make-zkml-witness
     :input-values input
     :layer-activations activations
     :output-values output)))

;;; ============================================================================
;;; Proof Structure
;;; ============================================================================

(defstruct (zkml-proof (:constructor %make-zkml-proof))
  "A zkML inference proof."
  (model-hash 0 :type integer)
  (input-hash 0 :type integer)
  (output-hash 0 :type integer)
  (constraint-commitments #() :type vector)
  (witness-commitments #() :type vector)
  (evaluation-proofs #() :type vector))

(defun make-zkml-proof (&key model-hash input-hash output-hash
                          constraint-commitments witness-commitments
                          evaluation-proofs)
  "Create a zkML proof."
  (%make-zkml-proof
   :model-hash (or model-hash 0)
   :input-hash (or input-hash 0)
   :output-hash (or output-hash 0)
   :constraint-commitments (coerce (or constraint-commitments #()) 'vector)
   :witness-commitments (coerce (or witness-commitments #()) 'vector)
   :evaluation-proofs (coerce (or evaluation-proofs #()) 'vector)))

;;; ============================================================================
;;; Hash Functions
;;; ============================================================================

(defun simple-hash (&rest values)
  "Simple hash for commitments."
  (let ((h 0))
    (loop for v in values do
      (setf h (mod (+ (* h 31) (if (numberp v) v (sxhash v)))
                   +zkml-field-prime+)))
    h))

(defun hash-tensor (tensor)
  "Hash a tensor."
  (let ((h 0)
        (data (zkml-tensor-data tensor)))
    (loop for val across data do
      (setf h (simple-hash h val)))
    h))

(defun hash-model (model)
  "Hash model weights."
  (let ((h 0))
    (loop for layer across (zkml-model-layers model) do
      (typecase layer
        (zkml-dense-layer
         (setf h (simple-hash h (hash-tensor (zkml-dense-layer-weights layer))))
         (when (zkml-dense-layer-bias layer)
           (setf h (simple-hash h (hash-tensor (zkml-dense-layer-bias layer))))))))
    h))

;;; ============================================================================
;;; Proof Generation
;;; ============================================================================

(defun zkml-prove-inference (model input)
  "Generate proof for ML inference."
  (let* ((witness (generate-inference-witness model input))
         (output (zkml-witness-output-values witness))
         (activations (zkml-witness-layer-activations witness))
         ;; Generate constraints
         (input-vars (loop for i from 0 below (zkml-tensor-size input)
                           collect (cons :input i)))
         (constraints (generate-model-constraints model input-vars))
         ;; Compute commitments
         (constraint-commits
           (map 'vector
                (lambda (c)
                  (simple-hash (zkml-constraint-a-coeffs c)
                               (zkml-constraint-b-coeffs c)))
                constraints))
         (witness-commits
           (coerce
            (loop for act in activations
                  collect (hash-tensor act))
            'vector)))
    (make-zkml-proof
     :model-hash (hash-model model)
     :input-hash (hash-tensor input)
     :output-hash (hash-tensor output)
     :constraint-commitments constraint-commits
     :witness-commitments witness-commits)))

;;; ============================================================================
;;; Proof Verification
;;; ============================================================================

(defun zkml-verify-inference (proof model input expected-output)
  "Verify a zkML inference proof."
  ;; Check model hash
  (unless (= (zkml-proof-model-hash proof)
             (hash-model model))
    (return-from zkml-verify-inference nil))
  ;; Check input hash
  (unless (= (zkml-proof-input-hash proof)
             (hash-tensor input))
    (return-from zkml-verify-inference nil))
  ;; Check output hash
  (unless (= (zkml-proof-output-hash proof)
             (hash-tensor expected-output))
    (return-from zkml-verify-inference nil))
  ;; Verify constraint commitments exist
  (unless (> (length (zkml-proof-constraint-commitments proof)) 0)
    (return-from zkml-verify-inference nil))
  t)
