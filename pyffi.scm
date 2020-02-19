;;;============================================================================

;;; File: "pyffi.scm"

;;; Copyright (c) 2020 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Python FFI.

(##supply-module github.com/feeley/pyffi)

(##namespace ("github.com/feeley/pyffi#"))  ;; in github.com/feeley/pyffi#
(##include "~~lib/_prim#.scm")              ;; map fx+ to ##fx+, etc
(##include "~~lib/_gambit#.scm")            ;; for macro-check-procedure,
                                            ;; macro-absent-obj, etc

(##include "pyffi#.scm")                    ;; correctly map pyffi ops

(declare (extended-bindings)) ;; ##fx+ is bound to fixnum addition, etc
(declare (not safe))          ;; claim code has no type errors
(declare (block))             ;; claim no global is assigned

;;;----------------------------------------------------------------------------

;; Locate python libs.

(define-macro (gen-meta-info)
  (define (version-to-int v)
    (string->number
     (list->string
      (map (lambda (c) (if (eq? c #\.) #\0 c))
           (take (drop (string->list v) 7) 5)))))

  (define python3-version
    (let ((v (shell-command "python3 --version" #t)))
      (if (= 0 (car v))
        (version-to-int (cdr v))
        (error "can't find python3"))))

  (define python3-config-embed (make-parameter #f))

  (define (python3-config-cmd f #!optional (embed (python3-config-embed)))
    (let ((cmd (string-append "python3-config " f)))
      (if embed
        (string-append cmd " --embed")
        cmd)))

  ;; The --embed flag only applies to Python >= 3.8
  (if (>= python3-version 30800)
    (python3-config-embed #t))

  (let* ((cflags  (shell-command (python3-config-cmd "--cflags") #t))
         (ldflags (shell-command (python3-config-cmd "--ldflags") #t)))
    (if (and (= 0 (car cflags))
             (= 0 (car ldflags)))
        `(begin
           (##meta-info
            cc-options
            ,(call-with-input-string (cdr cflags) read-line))
           (##meta-info
            ld-options
            ,(call-with-input-string (cdr ldflags) read-line)))
        (error "can't execute python3-config to find the python library"))))

(gen-meta-info)

;;;----------------------------------------------------------------------------

(c-declare "#include \"../pyffi.c\"")

;;;----------------------------------------------------------------------------

(c-define-type PyObject "PyObject")
(c-define-type PyObject*
               (pointer PyObject (PyObject*)))

(c-define-type PyObject*/release
               (pointer PyObject (PyObject*) "release_PyObjectPtr"))

(c-define-type PyScm "PyScm" "PYSCM_to_SCMOBJ" "SCMOBJ_to_PYSCM" #t)

(define Py_eval_input   ((c-lambda () int "___return(Py_eval_input);")))
(define Py_file_input   ((c-lambda () int "___return(Py_file_input);")))
(define Py_single_input ((c-lambda () int "___return(Py_single_input);")))

(define Py_Initialize
  (c-lambda () void "Py_Initialize"))

(define Py_Finalize
  (c-lambda () void "Py_Finalize"))

(define PyImport_AddModuleObject
  (c-lambda (PyScm) PyObject* "PyImport_AddModuleObject"))

(define PyRun_SimpleString
  (c-lambda (UTF-8-string) int "PyRun_SimpleString"))

(define PyRun_String
  (c-lambda (UTF-8-string int PyObject* PyObject*) PyScm "PyRun_String"))

(define PyRun_String*
  (c-lambda (UTF-8-string int PyObject* PyObject*) PyObject* "PyRun_String"))

(define PyModule_GetDict
  (c-lambda (PyObject*) PyObject* "PyModule_GetDict"))

(define PyDict_New
  (c-lambda () PyObject*/release "PyDict_New"))

(define PyObject_Str
  (c-lambda (PyObject*) PyObject* "PyObject_Str"))

(define PyObject_Bytes
  (c-lambda (PyObject*) PyObject* "PyObject_Bytes"))

(define PyVersion
  (c-lambda () int "PyVersion"))

(define PyUnicode->string
  (c-lambda (PyObject*) scheme-object "PyUnicode_string"))

(define (PyObject->string o)
  (PyUnicode->string (PyObject_Str o)))

;; PyObject* PyObject_CallMethod(PyObject *obj, const char *name, const char *format, ...)
(define PyObject_CallMethod
  (c-lambda (PyObject* nonnull-char-string nonnull-char-string) PyObject* "PyObject_CallMethod"))

;; PyObject* PyImport_ImportModuleEx(const char *name, PyObject *globals, PyObject *locals, PyObject *fromlist)
(define PyImport_ImportModuleEx
  (c-lambda (nonnull-char-string PyObject* PyObject* PyObject*) PyObject* "PyImport_ImportModuleEx"))

(define PyImport_ImportModule
  (c-lambda (nonnull-char-string) PyObject* "PyImport_ImportModule"))

(define PyObject_GetAttr
  (c-lambda (PyObject* PyObject*) PyObject* "PyObject_GetAttr"))

(define PyUnicode_FromString
  (c-lambda (nonnull-char-string) PyObject* "PyUnicode_FromString"))

;;;============================================================================
