(define-library (pyffi)

  ;; no import needed

  (export

   ;; Constants
   Py_eval_input
   Py_file_input
   Py_single_input
   PY_VERSION_HEX
   PY_MAJOR_VERSION
   PY_MINOR_VERSION
   PY_MICRO_VERSION

   ;; Initialization, Finalization, and Threads
   Py_Initialize
   Py_Finalize

   ;; Reference Counting
   Py_INCREF
   Py_XINCREF
   Py_DECREF
   Py_XDECREF
   Py_CLEAR

   ;; PyImport_*
   PyImport_AddModuleObject
   PyImport_ImportModule
   PyImport_ImportModuleEx

   ;; PyRun_*
   PyRun_SimpleString
   PyRun_String
   PyRun_String*

   ;; PyModule_*
   PyModule_GetDict

   ;; PyDict_*
   PyDict_New

   ;; PyObject_*
   PyObject_HasAttr
   PyObject_HasAttrString
   PyObject_GetAttr
   PyObject_GetAttrString
   PyObject_Repr
   PyObject_Str
   PyObject_Bytes
   PyObject_Call
   PyObject_CallObject
   PyObject_CallMethod
   PyObject_IsTrue
   PyObject_Not
   PyObject_Type
   PyObject_GetItem
   PyObject_SetItem
   PyObject_DelItem

   ;; PyList_*
   PyList_Check
   PyList_CheckExact
   PyList_New
   PyList_Size
   PyList_GetItem
   PyList_SetItem
   PyList_Insert
   PyList_Append
   PyList_AsTuple

   ;; PyUnicode_*
   PyUnicode_FromString

   ;; Other
   PyObject->string
   PyUnicode->string

   )

  (include "pyffi.scm"))
