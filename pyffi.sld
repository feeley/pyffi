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
   Py_SetPath
   Py_SetPythonHome

   ;; PyRun_*
   PyRun_SimpleString
   PyRun_String

   ;; PyImport_*
   PyImport_AddModuleObject
   PyImport_AddModule
   PyImport_ImportModule
   PyImport_ImportModuleEx

   ;; PyModule_*
   PyModule_GetDict

   ;; PyDict_*
   PyDict_New
   PyDict_SetItemString

   ;; PyList_*
   PyList_New

   ;; PyTuple_*
   PyTuple_GetItem

   ;; PyBool_*
   PyBool_FromLong

   ;; PyLong_*
   PyLong_FromUnicodeObject

   ;; PyUnicode_*
   PyUnicode_FromString

   ;; PyObject_*
   PyObject_CallMethod
   PyObject_GetAttrString
   PyObject_Length
   PyObject_Repr
   PyObject*-type
   PyObject*-type-name

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

   ;; Misc
   register-foreign-write-handlers
   pip-install
   pip
   py
   py-import
   current-python-interpreter
   six.infix
   convert
   python

   )

   (include "pyffi.scm")
   (include "six.scm")
   )
