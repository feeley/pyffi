#!/usr/bin/env gsi-script

(define-library (dis)

  (import (..))
  (import (gambit))

  (begin

    (define-macro (with-pyenv pyenv . things)
      `(begin
         (%python-environment% pyenv)
         ,@things))

    (define pyenv (start-python))

    (with-pyenv
     pyenv
     (PyRun_SimpleString "def f(x): return x+2")
     (define f (PyObject_GetAttrString
                (*python-environment*-__main__ (%python-environment%))
                "f"))
     (define bytes (pyrun "f.__code__.co_code.hex()"))
     (pydis bytes))

     (stop-python)

     ))
