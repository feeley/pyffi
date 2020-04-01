(define-library (pyffi)

  ;; no import needed

  (export

   ;; Debug
   _Py_REFCNT

   ;; Constants
   Py_eval_input
   Py_file_input
   Py_single_input

   ;; Initialization, Finalization, and Threads
   Py_Initialize
   Py_Finalize

   ;; PyUnicode_*
   PyUnicode_FromString

   ;; PyRun_*
   PyRun_SimpleString
   PyRun_String

   ;; PyImport_*
   PyImport_AddModuleObject

   ;; PyModule_*
   PyModule_GetDict

   ;; Converters
   PyObject*/str->string
   string->PyObject*/str

   )

  (include "pyffi.scm"))
