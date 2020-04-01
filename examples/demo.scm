#!/usr/bin/env gsi-script

(import (github.com/feeley pyffi))
(import (gambit))

(Py_Initialize)

(PyRun_SimpleString "print('result=' + repr(10*20));")

(define __main__
  (PyImport_AddModuleObject
   (string->PyObject*/str "__main__")))
(define globals (PyModule_GetDict __main__))

(define (pyrun s)
  (PyRun_String s Py_eval_input globals globals))

;; manual type conversions
(define pystr (pyrun "\"hello, world!\"")) ;; returns a PyObject*/str
(pretty-print pystr) ;; PyObject*/str
(pretty-print (PyObject*/str->string pystr)) ;; hello, world!

(Py_Finalize)
