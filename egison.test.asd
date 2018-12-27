(asdf:defsystem :egison.test
  :depends-on (:fiveam :egison)
  :components ((:file "test"))
  :perform (asdf:test-op (o s) (uiop:symbol-call :fiveam :run! 'egison.test:egison-test)))
