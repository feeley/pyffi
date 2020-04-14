
;; Legacy load of pyintf
(import (github.com/feeley pyffi))

;; Tests.
(Py_Initialize)

(define __main__ (PyImport_AddModuleObject (string->PyObject*/str "__main__")))
(define __main__dict (PyModule_GetDict __main__))

;; Run an expression
(define (pyrun str) (PyRun_String str Py_eval_input __main__dict __main__dict))
;; Run a statement
(define (pyrun* str) (PyRun_String str Py_single_input __main__dict __main__dict))

;; check conversion of various subtypes
(begin
(pp (pyrun "__builtins__"))
(pp (pyrun "None"))
(pp (pyrun "1>2"))
(pp (pyrun "1<2"))
(pp (pyrun "1+2*3"))
(pp (pyrun "3.1415"))
(pp (pyrun "1+2j"))
(pp (pyrun "b'abc'"))
(pp (pyrun "bytearray(b'abc')"))
(pp (pyrun "'abc'"))
(pp (pyrun "[1,2,3]"))
(pp (pyrun "{}"))
(pp (pyrun "frozenset()"))
(pp (pyrun "set()"))
(pp (pyrun "(1,2,3)"))
)

;; check roundtrip of strings
(begin
(pp (PyObject*/str->string (string->PyObject*/str "hello")))

(pp (PyObject*/str->string (pyrun "'ab'+'cd'")))

(with-exception-catcher
 (lambda (exc) (print "exception: ") (display-exception exc))
 (lambda () (pp (PyObject*/str->string (pyrun "'ab'+1")))))
)

;(Py_Finalize)

;;;----------------------------------------------------------------------------
