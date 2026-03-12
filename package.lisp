;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; Package definition for cl-zkml

(defpackage #:cl-zkml
  (:use #:cl)
  (:export
   ;; Field Arithmetic
   #:+zkml-field-prime+
   #:zkml-field-add
   #:zkml-field-sub
   #:zkml-field-mul
   #:zkml-field-div
   #:zkml-field-inv
   #:zkml-field-neg
   #:zkml-field-pow

   ;; Fixed-Point Arithmetic
   #:+zkml-scale+
   #:zkml-to-fixed
   #:zkml-from-fixed
   #:zkml-fixed-mul
   #:zkml-fixed-div

   ;; Tensors
   #:zkml-tensor
   #:make-zkml-tensor
   #:zkml-tensor-shape
   #:zkml-tensor-data
   #:zkml-tensor-get
   #:zkml-tensor-set
   #:zkml-tensor-size
   #:zkml-tensor-flatten
   #:zkml-tensor-reshape

   ;; Tensor Operations
   #:zkml-matmul
   #:zkml-add-tensors
   #:zkml-hadamard
   #:zkml-transpose
   #:zkml-broadcast

   ;; Activation Functions
   #:zkml-relu
   #:zkml-sigmoid
   #:zkml-tanh-approx
   #:zkml-softmax
   #:zkml-gelu-approx

   ;; Layers
   #:zkml-layer
   #:make-zkml-dense-layer
   #:make-zkml-conv2d-layer
   #:make-zkml-batchnorm-layer
   #:make-zkml-relu-layer
   #:make-zkml-softmax-layer
   #:zkml-layer-forward

   ;; Model
   #:zkml-model
   #:make-zkml-model
   #:zkml-model-layers
   #:zkml-model-forward
   #:zkml-model-predict

   ;; Quantization
   #:zkml-quantize-weights
   #:zkml-dequantize
   #:zkml-quantization-params
   #:make-zkml-quantization-params

   ;; Constraint Generation
   #:zkml-constraint
   #:make-zkml-constraint
   #:generate-layer-constraints
   #:generate-model-constraints

   ;; Proof System
   #:zkml-proof
   #:make-zkml-proof
   #:zkml-prove-inference
   #:zkml-verify-inference

   ;; Witness Generation
   #:zkml-witness
   #:make-zkml-witness
   #:generate-inference-witness

   ;; Model Import
   #:load-model-weights
   #:export-model-weights

   ;; Errors
   #:zkml-error))
