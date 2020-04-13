#!/usr/bin/env gsi-script

(import (github.com/feeley pyffi))
(import (gambit))

(define VENV_PATH
  (getenv "VENV_PATH" #f))

(if (not VENV_PATH)
    (begin
      (display "VENV_PATH not set") (newline)
      (exit #f)))

(define (PyImport-AddModuleObject s)
  (PyImport_AddModuleObject
   (string->PyObject*/str s)))

(define (PyImport-ImportModule s)
  (PyImport_ImportModule
   (string->PyObject*/str s)))

;; Generate PYTHONPATH from a virtualenv path
;; Assumes Python 3.7!
(define (venv-path->PYTHONPATH p)
  (string-append
   (string-append p "/lib/python37.zip")
   ":" (string-append p "/lib/python3.7")
   ":" (string-append p "/lib/python3.7/lib-dynload")
   ":" (string-append p "/lib/python3.7/site-packages")))

(define PYTHONPATH
  (venv-path->PYTHONPATH VENV_PATH))

(Py_SetPath PYTHONPATH)
(Py_Initialize)

(define __main__ (PyImport-AddModuleObject "__main__"))
(define globals (PyModule_GetDict __main__))
(define locals (PyDict_New))

(define numpy (PyImport_ImportModule "numpy"))
(define numpy_dict (PyModule_GetDict numpy))

(define arr (PyRun_String
             "arange(15).reshape(3, 5)"
             Py_eval_input
             numpy_dict
             locals))
(define arr.shape (PyObject_GetAttrString arr "shape"))
(define arr.type (PyObject_GetAttrString arr "__class__"))

;; Python objects
(pp arr)
(pp arr.shape)
(pp arr.type)

;; convert tuple to scheme list
(define arr.shape* (PyObject*->object arr.shape))
(pp arr.shape*)

(define py-type-object (PyObject*-type arr))
(pp py-type-object)

(define type-name (PyObject*-type-name arr))
(pp type-name)

(define (py/type o)
  (let ((t (PyObject*-type-name o)))
    (string->symbol t)))

(pp (py/type arr))

(Py_Finalize)


;; Make sure to set VENV_PATH and LD_PRELOAD.
;; e.g.:

;; VENV_PATH=~/.virtualenvs/gambit LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libpython3.7m.so gsid virtualenv.scm
;; #<PyObject* #2 0x7f87b64ea3a0>
;; #<PyObject*/tuple #3 0x7f87a9ff29c8>
;; #<PyObject* #4 0x7f87b63175c0>
;; (3 5)
;; #<PyTypeObject* #5 0x7f87b63175c0>
;; "numpy.ndarray"
;; numpy.ndarray
