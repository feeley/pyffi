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

    ;; (Py_Finalize)

    ))
