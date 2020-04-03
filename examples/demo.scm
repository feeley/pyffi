#!/usr/bin/env gsi-script

(import (github.com/feeley pyffi))
(import (gambit))

(Py_Initialize)

(PyRun_SimpleString "print('result=' + repr(10*20));")

(define __main__
  (PyImport_AddModuleObject
   (string->PyObject*/str "__main__")))
(define globals (PyModule_GetDict __main__))

(define (pyrun s)
  (PyRun_String s Py_eval_input globals globals))

;; manual type conversions
(define pystr (pyrun "\"hello, world!\"")) ;; returns a PyObject*/str
(pretty-print pystr) ;; PyObject*/str
(pretty-print (PyObject*/str->string pystr)) ;; hello, world!

(define (check-roundtrip py2scm scm2py vals)
  (for-each
   (lambda (x)
     (let* ((py (scm2py x))
            (scm (py2scm py)))
       (if (not (equal? scm x))
           (pretty-print (list 'error x '-> py '-> scm))
           '(pretty-print (list 'ok '-> x py '-> scm)))))
   vals))

(define (checks)

  (check-roundtrip PyObject*/None->void
                   void->PyObject*/None
                   (list (void)))

  (check-roundtrip PyObject*/bool->boolean
                   boolean->PyObject*/bool
                   (list #f #t))

  (check-roundtrip PyObject*/int->exact-integer
                   exact-integer->PyObject*/int
                   (list 42
                         2305843009213693951 ;; max fixnum
#;                         2305843009213693952
#;                         9223372036854775807
#;                         18446744073709551616))

  (check-roundtrip PyObject*/str->string
                   string->PyObject*/str
                   (list ""
                         "hello!\n"))
  )

(checks)

#;
(let loop ((n 100000000))
  (if (> n 0)
      (begin
        (checks)
        (loop (- n 1)))))

;(pretty-print (PyObject*/int->exact-integer py-None))
;(pretty-print (PyObject*/int->exact-integer py-False))
;(pretty-print (PyObject*/int->exact-integer py-True))
;(pretty-print (PyObject*/int->exact-integer py-tiny-int))
;(pretty-print (PyObject*/int->exact-integer py-max-fix))
;(pretty-print (PyObject*/int->exact-integer py-max-ll))
;;;;;;(pretty-print (PyObject*/int->exact-integer py-2^64))

(Py_Finalize)
