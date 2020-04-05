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

   ;; PyRun_*
   PyRun_SimpleString
   PyRun_String

   ;; PyImport_*
   PyImport_AddModuleObject

   ;; PyModule_*
   PyModule_GetDict

   ;; PyBool_*
   PyBool_FromLong

   ;; PyLong_*
   PyLong_FromUnicodeObject

   ;; PyUnicode_*
   PyUnicode_FromString

   ;; Converters
   PyObject*/None->void
   void->PyObject*/None
   PyObject*/bool->boolean
   boolean->PyObject*/bool
   PyObject*/int->exact-integer
   exact-integer->PyObject*/int
   PyObject*/str->string
   string->PyObject*/str
   PyObject*/tuple->vector
   vector->PyObject*/tuple
   PyObject*/list->vector
   vector->PyObject*/list
   PyObject*->object
   object->PyObject*

   )

  (include "pyffi.scm"))
