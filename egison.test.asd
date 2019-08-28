(asdf:defsystem :egison.test
  :depends-on (:fiveam :mockingbird :egison)
  :components ((:module "test"
                        :components ((:file "util")
                                     (:file "egison")
                                     (:file "poker"))))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :egison-util-test :egison.util.test))
                         (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :egison-test :egison.test))
                         (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :egison-poker-test :egison.poker.test))))
