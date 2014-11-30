(in-package :asdf-user)
(defsystem "static-noise"
  :description "A simple static site generator. It's very basic..."
  :version "0.0.1"
  :author "Bryan Baraoidan"
  :license "Public Domain"
  :depends-on (:cl-ppcre
               :cl-who
               :djula
               :cl-fad
               :cl-markdown
               :hunchentoot
               :local-time)
  :components ((:file "main")))
