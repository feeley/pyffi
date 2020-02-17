(define-library (pyffi)

  ;; no import needed

  (export

Py_eval_input
Py_file_input
Py_single_input

Py_Initialize
PyImport_AddModuleObject
PyRun_SimpleString
PyRun_String
PyModule_GetDict
PyDict_New

)

  (include "pyffi.scm"))
