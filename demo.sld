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

    ;; installed modules
    (define py/requests (PyImport_ImportModule "requests"))
    (define py/requests_dict (PyModule_GetDict py/requests))
    (define r (PyRun_String* "get(\"http://httpbin.org/ip\")" Py_eval_input py/requests_dict locals))

    (define (requests/json r)
      (PyObject->string
       (PyObject_CallMethod r "json" "")))

    (pretty-print (requests/json r))

    ;; Lists
    (define pylist (list->PyList '(1 2 "hello" "bonjour")))
    (pretty-print (PyObject->string pylist))

    ;; DECREFs
    (Py_DECREF globals)
    (Py_DECREF locals)
    (Py_DECREF pyint)
    (Py_DECREF py/math)
    (Py_DECREF pi)
    (Py_DECREF py/requests)
    (Py_DECREF py/requests_dict)
    (Py_DECREF r)
    (Py_DECREF pylist)

    (Py_Finalize)

    ))
