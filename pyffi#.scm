;;;============================================================================

;;; File: "pyffi#.scm"

;;; Copyright (c) 2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(##namespace ("github.com/feeley/pyffi#"

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
              PyImport_ImportModule

              ;; PyModule_*
              PyModule_GetDict

              ;; PyDict_*
              PyDict_New

              ;; PyBool_*
              PyBool_FromLong

              ;; PyLong_*
              PyLong_FromUnicodeObject

              ;; PyUnicode_*
              PyUnicode_FromString

              ;; PyObject_*
              PyObject_CallMethod

              ;; Converters
              PyObject*/None->void
              void->PyObject*/None
              PyObject*/bool->boolean
              boolean->PyObject*/bool
              PyObject*/int->exact-integer
              exact-integer->PyObject*/int
              PyObject*/str->string
              string->PyObject*/str

              ))

;;;============================================================================
