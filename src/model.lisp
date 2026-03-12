;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Neural network model for zkML

(in-package #:cl-zkml)

;;; ============================================================================
;;; Model Structure
;;; ============================================================================

(defstruct (zkml-model (:constructor %make-zkml-model))
  "A neural network model composed of layers."
  (layers #() :type vector)
  (name "model" :type string))

(defun make-zkml-model (layers &key (name "model"))
  "Create a model from a list of layers."
  (%make-zkml-model
   :layers (coerce layers 'vector)
   :name name))

;;; ============================================================================
;;; Model Forward Pass
;;; ============================================================================

(defun zkml-model-forward (model input)
  "Run forward pass through all layers, returning intermediate activations."
  (let ((activations (list input))
        (current input))
    (loop for layer across (zkml-model-layers model) do
      (setf current (zkml-layer-forward layer current))
      (push current activations))
    (values current (nreverse activations))))

(defun zkml-model-predict (model input)
  "Run inference and return only the final output."
  (zkml-model-forward model input))

;;; ============================================================================
;;; Quantization
;;; ============================================================================

(defstruct (zkml-quantization-params (:constructor %make-zkml-quantization-params))
  "Quantization parameters for a tensor."
  (scale 1.0 :type number)
  (zero-point 0 :type integer)
  (bits 8 :type integer))

(defun make-zkml-quantization-params (&key (scale 1.0) (zero-point 0) (bits 8))
  "Create quantization parameters."
  (%make-zkml-quantization-params
   :scale scale
   :zero-point zero-point
   :bits bits))

(defun zkml-quantize-weights (tensor &key (bits 8))
  "Quantize tensor weights to fixed-point representation."
  (let* ((size (zkml-tensor-size tensor))
         (data (zkml-tensor-data tensor))
         (max-val 0)
         (result-data (make-array size)))
    ;; Find max absolute value
    (loop for i from 0 below size do
      (let ((abs-val (abs (zkml-from-fixed (aref data i)))))
        (when (> abs-val max-val)
          (setf max-val abs-val))))
    ;; Compute scale
    (let* ((qmax (1- (ash 1 (1- bits))))
           (scale (if (zerop max-val) 1.0 (/ max-val qmax))))
      ;; Quantize
      (loop for i from 0 below size do
        (let* ((float-val (zkml-from-fixed (aref data i)))
               (q-val (round (/ float-val scale))))
          (setf (aref result-data i)
                (zkml-to-fixed (* q-val scale)))))
      (values (%make-zkml-tensor
               :shape (zkml-tensor-shape tensor)
               :data result-data
               :strides (zkml-tensor-strides tensor))
              (make-zkml-quantization-params :scale scale :bits bits)))))

(defun zkml-dequantize (tensor params)
  "Dequantize tensor using parameters."
  (declare (ignore params))
  ;; In fixed-point representation, data is already in usable form
  tensor)

;;; ============================================================================
;;; Model Weight I/O
;;; ============================================================================

(defun load-model-weights (model weight-list)
  "Load weights from a list of tensors into model layers."
  (let ((weight-idx 0))
    (loop for layer across (zkml-model-layers model) do
      (typecase layer
        (zkml-dense-layer
         (when (< weight-idx (length weight-list))
           (setf (zkml-dense-layer-weights layer)
                 (nth weight-idx weight-list))
           (incf weight-idx))
         (when (and (zkml-dense-layer-bias layer)
                    (< weight-idx (length weight-list)))
           (setf (zkml-dense-layer-bias layer)
                 (nth weight-idx weight-list))
           (incf weight-idx)))
        (zkml-conv2d-layer
         (when (< weight-idx (length weight-list))
           (setf (zkml-conv2d-layer-kernel layer)
                 (nth weight-idx weight-list))
           (incf weight-idx))
         (when (and (zkml-conv2d-layer-bias layer)
                    (< weight-idx (length weight-list)))
           (setf (zkml-conv2d-layer-bias layer)
                 (nth weight-idx weight-list))
           (incf weight-idx))))))
  model)

(defun export-model-weights (model)
  "Export all model weights as a list of tensors."
  (let ((weights nil))
    (loop for layer across (zkml-model-layers model) do
      (typecase layer
        (zkml-dense-layer
         (push (zkml-dense-layer-weights layer) weights)
         (when (zkml-dense-layer-bias layer)
           (push (zkml-dense-layer-bias layer) weights)))
        (zkml-conv2d-layer
         (push (zkml-conv2d-layer-kernel layer) weights)
         (when (zkml-conv2d-layer-bias layer)
           (push (zkml-conv2d-layer-bias layer) weights)))))
    (nreverse weights)))
