#!/usr/bin/env gsi-script

(define-library (requests)

  (import (..))
  (import (gambit))

  (begin

    (Py_Initialize)

    (define __main__ (PyImport_AddModuleObject "__main__"))
    (define globals (PyModule_GetDict __main__))
    (define locals (PyDict_New))

    (define py/requests (PyImport_ImportModule "requests"))
    (define py/requests_dict (PyModule_GetDict py/requests))
    (define r (PyRun_String* "get(\"http://httpbin.org/ip\")" Py_eval_input py/requests_dict locals))

    (define (requests/json r)
      (PyObject->string
       (PyObject_CallMethod r "json" "")))

    (pretty-print (requests/json r))

    (Py_DECREF py/requests)
    (Py_DECREF py/requests_dict)
    (Py_DECREF r)

    (Py_Finalize)

    ))
