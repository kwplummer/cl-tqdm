(in-package #:cl-user)

(asdf:defsystem :cl-tqdm
  :name "cl-tqdm"
  :description "Simple And Fast Progress Bar Library for Common Lisp"
  :author "kwplummer" ; Forked from hikettei. Not sure what to do here...
  :version "v1.0.1"
  :license "MIT"
  :source-control (:git "git@github.com:kwplummer/cl-tqdm.git")
  :serial t
  :components ((:file "cl-tqdm")))

