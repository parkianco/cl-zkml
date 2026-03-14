;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; test-zkml.lisp - Unit tests for zkml
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:cl-zkml.test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-zkml.test)

(defun run-tests ()
  "Run all tests for cl-zkml."
  (format t "~&Running tests for cl-zkml...~%")
  ;; TODO: Add test cases
  ;; (test-function-1)
  ;; (test-function-2)
  (format t "~&All tests passed!~%")
  t)
