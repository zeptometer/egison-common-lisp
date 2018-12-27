(asdf:defsystem :egison.test
  :depends-on (:fiveam :egison)
  :components ((:module "test"
                        :components ((:file "egison")
                                     (:file "poker"))))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :egison-test :egison.test))
                         (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :egison-poker-test :egison.poker.test))))
