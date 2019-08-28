# Common Lisp Macros for Egison Pattern Matching
This library is ported from [egison-scheme](https://github.com/egison/egison-scheme).

## Install
egison-common-lisp is not provided by quicklisp currently. You need to [configure ASDF manually](https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html) to use egison-common-lisp. You can use [local-project mechanism](http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html) if you use Quicklisp.

```
    (asdf:load-system :egison) ;; Load system
    (asdf:test-system :egison) ;; Run unit tests

    (use-package :egison)
    ...
```

## Usage
TODO: Add document for egison-stype pattern matching (Sorry!)

For now, please read [egison-scheme's README](https://github.com/egison/egison-scheme/blob/master/README.md) for general background and usage. Also read /test directory to see how egison-style pattern match looks like with egison-common-lisp.
