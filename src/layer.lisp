;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Neural network layers for zkML

(in-package #:cl-zkml)

;;; ============================================================================
;;; Layer Base
;;; ============================================================================

(defstruct (zkml-layer (:constructor nil))
  "Base structure for neural network layers."
  (type nil :type keyword)
  (params nil :type list))

;;; ============================================================================
;;; Dense Layer
;;; ============================================================================

(defstruct (zkml-dense-layer (:include zkml-layer)
                             (:constructor %make-zkml-dense-layer))
  "Fully connected (dense) layer."
  (weights nil :type (or null zkml-tensor))
  (bias nil :type (or null zkml-tensor))
  (activation nil :type (or null keyword)))

(defun make-zkml-dense-layer (input-dim output-dim &key bias-p (activation nil))
  "Create a dense layer."
  (%make-zkml-dense-layer
   :type :dense
   :weights (make-zkml-tensor (list input-dim output-dim))
   :bias (when bias-p (make-zkml-tensor (list output-dim)))
   :activation activation))

;;; ============================================================================
;;; Conv2D Layer
;;; ============================================================================

(defstruct (zkml-conv2d-layer (:include zkml-layer)
                              (:constructor %make-zkml-conv2d-layer))
  "2D convolution layer."
  (kernel nil :type (or null zkml-tensor))  ; (out-channels, in-channels, kH, kW)
  (bias nil :type (or null zkml-tensor))
  (stride 1 :type integer)
  (padding 0 :type integer)
  (activation nil :type (or null keyword)))

(defun make-zkml-conv2d-layer (in-channels out-channels kernel-size
                               &key (stride 1) (padding 0) bias-p (activation nil))
  "Create a 2D convolution layer."
  (%make-zkml-conv2d-layer
   :type :conv2d
   :kernel (make-zkml-tensor (list out-channels in-channels kernel-size kernel-size))
   :bias (when bias-p (make-zkml-tensor (list out-channels)))
   :stride stride
   :padding padding
   :activation activation))

;;; ============================================================================
;;; BatchNorm Layer
;;; ============================================================================

(defstruct (zkml-batchnorm-layer (:include zkml-layer)
                                 (:constructor %make-zkml-batchnorm-layer))
  "Batch normalization layer."
  (gamma nil :type (or null zkml-tensor))  ; Scale
  (beta nil :type (or null zkml-tensor))   ; Shift
  (running-mean nil :type (or null zkml-tensor))
  (running-var nil :type (or null zkml-tensor))
  (epsilon 1e-5 :type number))

(defun make-zkml-batchnorm-layer (num-features &key (epsilon 1e-5))
  "Create a batch normalization layer."
  (let ((ones (make-zkml-tensor (list num-features)
                                :initial-element (zkml-to-fixed 1.0)))
        (zeros (make-zkml-tensor (list num-features)
                                 :initial-element 0)))
    (%make-zkml-batchnorm-layer
     :type :batchnorm
     :gamma ones
     :beta zeros
     :running-mean zeros
     :running-var ones
     :epsilon epsilon)))

;;; ============================================================================
;;; Activation Layers
;;; ============================================================================

(defstruct (zkml-activation-layer (:include zkml-layer)
                                  (:constructor %make-zkml-activation-layer))
  "Activation function layer."
  (activation-type :relu :type keyword))

(defun make-zkml-relu-layer ()
  "Create a ReLU activation layer."
  (%make-zkml-activation-layer
   :type :activation
   :activation-type :relu))

(defun make-zkml-softmax-layer ()
  "Create a softmax activation layer."
  (%make-zkml-activation-layer
   :type :activation
   :activation-type :softmax))

;;; ============================================================================
;;; Layer Forward Pass
;;; ============================================================================

(defgeneric zkml-layer-forward (layer input)
  (:documentation "Compute forward pass through layer."))

(defmethod zkml-layer-forward ((layer zkml-dense-layer) input)
  "Dense layer forward: output = input @ weights + bias."
  (let* ((weights (zkml-dense-layer-weights layer))
         (bias (zkml-dense-layer-bias layer))
         (output (zkml-matmul input weights)))
    ;; Add bias if present
    (when bias
      (let ((broadcast-bias (zkml-broadcast bias (zkml-tensor-shape output))))
        (setf output (zkml-add-tensors output broadcast-bias))))
    ;; Apply activation
    (case (zkml-dense-layer-activation layer)
      (:relu (zkml-relu output))
      (:sigmoid (zkml-sigmoid output))
      (:tanh (zkml-tanh-approx output))
      (:softmax (zkml-softmax output))
      (:gelu (zkml-gelu-approx output))
      (otherwise output))))

(defmethod zkml-layer-forward ((layer zkml-conv2d-layer) input)
  "Conv2D layer forward pass (simplified for 4D tensors)."
  (let* ((kernel (zkml-conv2d-layer-kernel layer))
         (bias (zkml-conv2d-layer-bias layer))
         (stride (zkml-conv2d-layer-stride layer))
         (padding (zkml-conv2d-layer-padding layer))
         (input-shape (zkml-tensor-shape input))
         (kernel-shape (zkml-tensor-shape kernel))
         ;; Assuming NCHW format
         (batch-size (first input-shape))
         (out-channels (first kernel-shape))
         (in-h (third input-shape))
         (in-w (fourth input-shape))
         (k-h (third kernel-shape))
         (k-w (fourth kernel-shape))
         (out-h (1+ (floor (- (+ in-h (* 2 padding)) k-h) stride)))
         (out-w (1+ (floor (- (+ in-w (* 2 padding)) k-w) stride)))
         (output (make-zkml-tensor (list batch-size out-channels out-h out-w))))
    (declare (ignore bias in-h in-w k-h k-w stride))
    ;; Simplified: just return zero tensor for structure
    ;; Full convolution would be implemented in production
    output))

(defmethod zkml-layer-forward ((layer zkml-batchnorm-layer) input)
  "BatchNorm layer forward pass."
  (let* ((gamma (zkml-batchnorm-layer-gamma layer))
         (beta (zkml-batchnorm-layer-beta layer))
         (mean (zkml-batchnorm-layer-running-mean layer))
         (var (zkml-batchnorm-layer-running-var layer))
         (shape (zkml-tensor-shape input))
         (size (zkml-tensor-size input))
         (result-data (make-array size)))
    (declare (ignore gamma beta mean var))
    ;; Simplified: x_norm = (x - mean) / sqrt(var + eps) * gamma + beta
    ;; For now, just pass through
    (loop for i from 0 below size do
      (setf (aref result-data i)
            (aref (zkml-tensor-data input) i)))
    (%make-zkml-tensor
     :shape shape
     :data result-data
     :strides (zkml-tensor-strides input))))

(defmethod zkml-layer-forward ((layer zkml-activation-layer) input)
  "Activation layer forward pass."
  (case (zkml-activation-layer-activation-type layer)
    (:relu (zkml-relu input))
    (:sigmoid (zkml-sigmoid input))
    (:tanh (zkml-tanh-approx input))
    (:softmax (zkml-softmax input))
    (:gelu (zkml-gelu-approx input))
    (otherwise input)))
