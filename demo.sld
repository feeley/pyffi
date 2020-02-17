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

    (pretty-print
     (PyRun_String "len('123456789')" Py_eval_input globals locals))))
