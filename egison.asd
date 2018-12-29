(asdf:defsystem "egison"
  :version "0.0.0"
  :licence "MIT"
  :description "This package introduces Egison's non-linear pattern-matching to Common Lisp."
  :author "Yuito Murase <yuito.murase@gmail.com>"
  :depends-on ("optima")
  :components ((:module "src"
                        :serial t
                        :components ((:file "util")
                                     (:file "egison")
                                     (:file "poker"))))
  :in-order-to ((test-op (test-op "egison.test"))))
