(import (github.com/feeley pyffi))
(import (gambit))

(define (PyImport-AddModuleObject s)
  (PyImport_AddModuleObject
   (string->PyObject*/str s)))

(define VENV_PATH
  (getenv "VENV_PATH" #f))

(if (not VENV_PATH)
    (begin
      (display "VENV_PATH not set") (newline)
      (exit #f)))

;; Assumes a proper virtualenv, created with virtualenv, not python -m venv
(define (venv-path->PYTHONPATH p)
  (string-append
   (string-append p "/lib/python37.zip")
   ":" (string-append p "/lib/python3.7")
   ":" (string-append p "/lib/python3.7/lib-dynload")
   ":" (string-append p "/lib/python3.7/site-packages")))

(define python-subtypes
  '(PyObject*
    PyObject*/None
    PyObject*/bool
    PyObject*/int
    PyObject*/float
    PyObject*/complex
    PyObject*/bytes
    PyObject*/bytearray
    PyObject*/str
    PyObject*/list
    PyObject*/dict
    PyObject*/frozenset
    PyObject*/set
    PyObject*/tuple
    PyObject*/module
    ))

(define (PyObject*-register-foreign-write-handler t)
  (##readtable-foreign-write-handler-register!
   ##main-readtable
   t
   (lambda (we obj)
     (##wr-sn* we obj t PyObject*-wr-str))))


(define (PyObject*-wr-str we obj)
  (let* ((repr (PyObject_Repr obj))
         (s (PyObject*/str->string repr)))
    (if (> (string-length s) 50)
        (##wr-str we (string-append "\n" s))
        (##wr-str we (string-append " " s)))))

(for-each PyObject*-register-foreign-write-handler python-subtypes)

(define PYTHONPATH
  (venv-path->PYTHONPATH VENV_PATH))

(Py_SetPath PYTHONPATH)
(Py_Initialize)

(define __main__ (PyImport-AddModuleObject "__main__"))
(define globals (PyModule_GetDict __main__))
(define locals (PyDict_New))

(define numpy (PyImport_ImportModuleEx "numpy" globals locals (PyList_New 0)))
(define numpy_dict (PyModule_GetDict numpy))

(define (pyrun s #!optional (d globals))
  (PyRun_String s Py_eval_input d locals))

(define None (pyrun "None"))
(define bool (pyrun "True"))
(define int (pyrun "2+2"))
(define float (pyrun "3.14159"))
(define complex (pyrun "3+2j"))
(define bytes (pyrun "b'ABC123'"))
(define bytearray (pyrun "bytearray('ABC123', 'utf-8')"))
(define str (pyrun "\"Hello, world!\""))
(define pylist (pyrun "[1, 2, 3]"))
(define dict (pyrun "{'a': 1}"))
(define frozenset (pyrun "frozenset(('a', 'b', 'c'))"))
(define set (pyrun "set([1, 1, 2, 2, 3, 3])"))
(define tuple (pyrun "(1, 2)"))
(define arr (pyrun "arange(15).reshape(3, 5)" numpy_dict))

(pp None)
(pp bool)
(pp int)
(pp float)
(pp complex)
(pp bytes)
(pp bytearray)
(pp str)
(pp pylist)
(pp dict)
(pp frozenset)
(pp set)
(pp tuple)
(pp __main__)
(pp arr)

(Py_Finalize)
