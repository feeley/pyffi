#!/usr/bin/env gsi-script

(define-library (demo)

  (import (..))
  (import (gambit))

  (begin

    (Py_Initialize)

    (PyRun_SimpleString "print('result=' + repr(10*20));")

    (define __main__ (PyImport_AddModuleObject "__main__"))
    (define globals (PyModule_GetDict __main__))
    (define locals (PyDict_New))

    (define (pyrun s) (PyRun_String s Py_eval_input globals locals))
    (define (pyrun* s) (PyRun_String* s Py_eval_input globals locals))

    ;; manual type conversions
    (define pyint (pyrun* "0")) ;; returns a PyObject*
    (pretty-print pyint)
    (pretty-print (PyObject_Str pyint)) ;; calls __str__()
    (pretty-print (PyUnicode->string (PyObject_Str pyint))) ;; to scheme string

    ;; automatic type conversion
    ;; automatic DECREF, see C code
    (define pystring (pyrun "\"Hello, world!\""))
    (pretty-print pystring)

    ;; built-in modules
    (define py/math (PyImport_ImportModuleEx "math" globals globals #f))
    (define pi (PyObject_GetAttr py/math (PyUnicode_FromString "pi")))
    (pretty-print (PyObject->string pi))

    ;; Lists
    (define pylist (list->PyList '(1 2 "hello" "bonjour" #t #f)))
    (pretty-print (PyObject->string pylist))

    ;; DECREFs
    ;; (Py_DECREF globals)
    ;; (Py_DECREF locals)
    ;; (Py_DECREF pyint)
    ;; (Py_DECREF py/math)
    ;; (Py_DECREF pi)
    ;; (Py_DECREF pylist)

    (Py_Finalize)

    ))
