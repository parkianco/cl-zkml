# cl-zkml

Pure Common Lisp zero-knowledge machine learning inference library.

## Features

- **Zero dependencies** - completely standalone
- **Fixed-point arithmetic** - ZK-friendly quantized computations
- **Neural network layers** - Dense, Conv2D, BatchNorm, activations
- **Activation functions** - ReLU, Sigmoid, Tanh, Softmax, GELU
- **Tensor operations** - MatMul, element-wise ops, broadcasting
- **Constraint generation** - Automatic R1CS-style constraints
- **Proof system** - Generate and verify ML inference proofs

## Installation

```bash
cd ~/common-lisp/
git clone https://github.com/parkianco/cl-zkml.git
```

```lisp
(asdf:load-system :cl-zkml)
```

## Quick Start

```lisp
(use-package :cl-zkml)

;; Create a simple neural network
(defvar *model*
  (make-zkml-model
   (list (make-zkml-dense-layer 4 8 :bias-p t :activation :relu)
         (make-zkml-dense-layer 8 3 :bias-p t :activation :softmax))))

;; Create input tensor (batch of 2, 4 features)
(defvar *input*
  (make-zkml-tensor '(2 4)
    :initial-contents '((0.1 0.2 0.3 0.4)
                        (0.5 0.6 0.7 0.8))))

;; Run inference
(defvar *output* (zkml-model-predict *model* *input*))

;; Generate ZK proof of inference
(defvar *proof* (zkml-prove-inference *model* *input*))

;; Verify proof
(zkml-verify-inference *proof* *model* *input* *output*)  ; => T
```

## API Reference

### Fixed-Point Arithmetic

```lisp
;; Convert between float and field representation
(zkml-to-fixed 0.5)       ; => field element
(zkml-from-fixed fe)      ; => 0.5

;; Fixed-point operations (maintains scale)
(zkml-fixed-mul a b)
(zkml-fixed-div a b)
```

### Tensors

```lisp
;; Create tensors
(make-zkml-tensor '(3 4))                    ; 3x4 zeros
(make-zkml-tensor '(2 3) :initial-element 1) ; 2x3 ones
(make-zkml-tensor '(2 2) :initial-contents '((1 2) (3 4)))

;; Access elements
(zkml-tensor-get tensor 0 1)      ; Get element
(zkml-tensor-set tensor val 0 1)  ; Set element

;; Operations
(zkml-matmul a b)          ; Matrix multiplication
(zkml-add-tensors a b)     ; Element-wise addition
(zkml-hadamard a b)        ; Element-wise multiplication
(zkml-transpose tensor)    ; Transpose 2D tensor
```

### Activation Functions

```lisp
(zkml-relu tensor)         ; max(0, x)
(zkml-sigmoid tensor)      ; Piecewise linear approximation
(zkml-tanh-approx tensor)  ; Piecewise linear approximation
(zkml-softmax tensor)      ; Softmax normalization
(zkml-gelu-approx tensor)  ; GELU approximation
```

### Layers

```lisp
;; Dense (fully connected)
(make-zkml-dense-layer input-dim output-dim
  :bias-p t :activation :relu)

;; Convolution
(make-zkml-conv2d-layer in-ch out-ch kernel-size
  :stride 1 :padding 0 :activation :relu)

;; Batch normalization
(make-zkml-batchnorm-layer num-features)

;; Activation only
(make-zkml-relu-layer)
(make-zkml-softmax-layer)
```

### Models

```lisp
;; Build model
(make-zkml-model (list layer1 layer2 layer3))

;; Inference
(zkml-model-predict model input)     ; Final output only
(zkml-model-forward model input)     ; Output + all activations

;; Weight management
(load-model-weights model weight-list)
(export-model-weights model)
```

### Quantization

```lisp
;; Quantize weights for smaller proofs
(zkml-quantize-weights tensor :bits 8)

;; Create quantization params
(make-zkml-quantization-params :scale 0.01 :bits 8)
```

### Proof System

```lisp
;; Generate proof
(zkml-prove-inference model input)

;; Verify proof
(zkml-verify-inference proof model input expected-output)

;; Generate constraints manually
(generate-model-constraints model input-vars)
(generate-layer-constraints layer input-vars output-vars)
```

## Fixed-Point Representation

All computations use 18-bit fractional precision:
- Scale factor: 2^18 = 262144
- Range: approximately [-8M, 8M] with ~0.000004 precision
- Field: BN254 scalar field (254-bit prime)

## ZK Compatibility

This library generates constraints compatible with:
- R1CS (Rank-1 Constraint System)
- Groth16 / PLONK / STARK backends

Constraints are automatically generated for:
- Matrix multiplication
- Element-wise operations
- ReLU (range proofs)
- Softmax normalization

## License

BSD-3-Clause. See [LICENSE](LICENSE).

## Author

Parkian Company LLC
