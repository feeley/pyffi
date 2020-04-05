#!/usr/bin/env gsi-script

(import (github.com/feeley pyffi))
(import (gambit))

(define (PyImport-AddModuleObject s)
  (PyImport_AddModuleObject
   (string->PyObject*/str s)))

(define (PyImport-ImportModule s)
  (PyImport_ImportModule
   (string->PyObject*/str s)))

(Py_Initialize)

(define __main__ (PyImport-AddModuleObject "__main__"))
(define globals (PyModule_GetDict __main__))
(define locals (PyDict_New))

(define py/requests (PyImport_ImportModule "requests"))
(define py/json     (PyImport_ImportModule "json"))
(define py/requests_dict (PyModule_GetDict py/requests))
(define r (PyRun_String
           "get(\"http://httpbin.org/ip\")"
           Py_eval_input
           py/requests_dict
           locals))

(define r.json_dict (PyObject_CallMethod r "json" ""))
(define r.json_text (PyObject_CallMethod r.json_dict "__str__" ""))

(pretty-print (PyObject*/str->string r.json_text))

(Py_Finalize)
