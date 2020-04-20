#!/usr/bin/env gsi-script

(import (github.com/feeley pyffi))
(import (gambit))

;; enable rich python foreign-object representation
(register-foreign-write-handlers)

(define (PyImport-AddModuleObject s)
  (PyImport_AddModuleObject
   (string->PyObject*/str s)))

(define (PyImport-ImportModule s)
  (PyImport_ImportModule
   (string->PyObject*/str s)))

(define VENV_PATH
  (getenv "VENV_PATH" #f))

(if (not VENV_PATH)
  (begin
    (display "VENV_PATH not set") (newline)
    (exit #f)))

;; Assumes a proper virtualenv, created with virtualenv, not python -m venv
(define (venv-path->PYTHONPATH p)
  (string-append
   (string-append p "/bin/python")
   ":" (string-append p "/lib/python37.zip")
   ":" (string-append p "/lib/python3.7")
   ":" (string-append p "/lib/python3.7/lib-dynload")
   ":" (string-append p "/lib/python3.7/site-packages")))

(define PYTHONPATH
  (venv-path->PYTHONPATH VENV_PATH))

(Py_SetPath PYTHONPATH)
(Py_SetPythonHome VENV_PATH)
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

(pp r.json_text)
(pp (PyObject*/str->string r.json_text))

(Py_Finalize)
