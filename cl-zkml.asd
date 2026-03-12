;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause
;;;;
;;;; cl-zkml.asd - ASDF system definition for zkML

(asdf:defsystem #:cl-zkml
  :description "Pure Common Lisp zero-knowledge machine learning inference"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "1.0.0"
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "field")
     (:file "tensor")
     (:file "activation")
     (:file "layer")
     (:file "model")
     (:file "proof")))))
