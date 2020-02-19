(define-library (pyffi)

  ;; no import needed

  (export

   ;; constants
   Py_eval_input
   Py_file_input
   Py_single_input

   Py_Initialize
   Py_Finalize

   ;; PyImport
   PyImport_AddModuleObject
   PyImport_ImportModule
   PyImport_ImportModuleEx

   ;; PyRun
   PyRun_SimpleString
   PyRun_String
   PyRun_String*

   ;; PyModule
   PyModule_GetDict

   ;; PyDict
   PyDict_New

   ;; PyObject
   PyObject_Str
   PyObject_CallMethod
   PyObject_GetAttr

   PyObject->string

   ;; PyUnicode
   PyUnicode->string
   PyUnicode_FromString

   ;; Helpers
   PyVersion

   )

  (include "pyffi.scm"))
