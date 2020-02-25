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
(##include "pydis#.scm")

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

  (define python3-config-embed (##make-parameter #f))

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

(c-declare "#include \"../pyffi-intf.c\"")

;;;----------------------------------------------------------------------------

(define-macro (map-macro macro lst)
  (if (not (null? (car lst)))
      (let lp ((lst (cdr lst))
               (acc (cons `(,macro ,@(car lst)) '())))
        (if (null? lst)
            `(begin ,@acc)
            (lp (cdr lst) (cons `(,macro ,@(car lst)) acc))))))

(define-macro (PyAPI id args ret #!optional (f #f))
  (let ((sid (if f f (symbol->string id))))
    `(define ,id
       (c-lambda ,args ,ret ,sid))))

(define-macro (with-PyAPI . args)
  `(map-macro PyAPI ,args))


;; Types
(c-define-type PyObject "PyObject")
(c-define-type PyObject*
               (pointer PyObject (PyObject*)))

(c-define-type PyObject*/release
               (pointer PyObject (PyObject*) "release_PyObjectPtr"))

(c-define-type PyScm "PyScm" "PYSCM_to_SCMOBJ" "SCMOBJ_to_PYSCM" #t)

(c-define-type Py_ssize_t ssize_t)


;; Constants
(define Py_eval_input    ((c-lambda () int "___return(Py_eval_input);")))
(define Py_file_input    ((c-lambda () int "___return(Py_file_input);")))
(define Py_single_input  ((c-lambda () int "___return(Py_single_input);")))
(define PY_VERSION_HEX   ((c-lambda () int "___return(PY_VERSION_HEX);")))
(define PY_MAJOR_VERSION ((c-lambda () int "___return(PY_MAJOR_VERSION);")))
(define PY_MINOR_VERSION ((c-lambda () int "___return(PY_MINOR_VERSION);")))
(define PY_MICRO_VERSION ((c-lambda () int "___return(PY_MICRO_VERSION);")))


(with-PyAPI
 ;; Initialization, Finalization, and Threads
 (Py_Initialize () void)
 (Py_Finalize   () void)
 (Py_SetProgramName (wchar_t-string) void)

 ;; Reference Counting
 (Py_INCREF  (PyObject*) void)
 (Py_XINCREF (PyObject*) void)
 (Py_DECREF  (PyObject*) void)
 (Py_XDECREF (PyObject*) void)
 ;; (Py_CLEAR   (PyObject*) void)

 ;; PyImport_*
 (PyImport_AddModuleObject (PyScm) PyObject*)
 (PyImport_ImportModule    (nonnull-char-string) PyObject*)
 (PyImport_ImportModuleEx
  (nonnull-char-string PyObject* PyObject* PyObject*) PyObject*)

 ;; PyModule_*
 (PyModule_GetDict (PyObject*) PyObject*)

 ;; PyRun_*
 (PyRun_SimpleString (UTF-8-string) int)
 (PyRun_String       (UTF-8-string int PyObject* PyObject*) PyScm)
 (PyRun_String*
  (UTF-8-string int PyObject* PyObject*) PyObject* "PyRun_String")

 ;; PyDict_*
 (PyDict_New () PyObject*)

 ;; PyObject_*
 (PyObject_HasAttr       (PyObject* PyObject*) int)
 (PyObject_HasAttrString (PyObject* nonnull-char-string) int)
 (PyObject_GetAttr       (PyObject* PyObject*) PyObject*) ;; New
 (PyObject_GetAttrString (PyObject* nonnull-char-string) PyObject*) ;; New
 (PyObject_Repr          (PyObject*) PyObject*) ;; New
 (PyObject_Str           (PyObject*) PyObject*) ;; New
 (PyObject_Bytes         (PyObject*) PyObject*) ;; New
 (PyObject_Call          (PyObject* PyObject* PyObject*) PyObject*) ;; New
 (PyObject_CallObject    (PyObject* PyObject*) PyObject*) ;; New
 (PyObject_CallMethod
  (PyObject* nonnull-char-string nonnull-char-string) PyObject*) ;; New
 (PyObject_IsTrue  (PyObject*) int)
 (PyObject_Not     (PyObject*) int)
 (PyObject_Type    (PyObject*) PyObject*) ;; New
 (PyObject_GetItem (PyObject* PyObject*) PyObject*) ;; New
 (PyObject_SetItem (PyObject* PyObject* PyObject*) int)
 (PyObject_DelItem (PyObject* PyObject*) int)
 (PyObject_Dir     (PyObject*) PyObject*) ;; New

 ;; PyList_*
 (PyList_Check       (PyObject*) int)
 (PyList_CheckExact  (PyObject*) int)
 (PyList_New         (Py_ssize_t) PyObject*) ;; New
 (PyList_Size        (PyObject*) Py_ssize_t)
 (PyList_GetItem     (PyObject* Py_ssize_t) PyObject*) ;; Borrowed
 (PyList_SetItem     (PyObject* Py_ssize_t PyObject*) int) ;; Stolen
 (PyList_Insert      (PyObject* Py_ssize_t PyObject*) int)
 (PyList_Append      (PyObject* PyObject*) int)
 (PyList_AsTuple     (PyObject*) PyObject*) ;; New

 ;; PyUnicode_*
 (PyUnicode_FromString (nonnull-char-string) PyObject*)

 ;; Reflection
 (PyEval_GetGlobals () PyObject*)

 )


;; Utils
(define PyUnicode->string
  (c-lambda (PyObject*) scheme-object "PyUnicode_string"))

(define (PyObject->string o)
  (PyUnicode->string (PyObject_Str o)))

(define (Scm_list_length l)
  ((c-lambda (scheme-object) Py_ssize_t "Scm_list_length") l))

(define (SCMOBJ_to_PyObject obj)
  ((c-lambda (scheme-object) PyObject* "SCMOBJ_to_PyObject") obj))

(define (list->PyList l)
  ((c-lambda (scheme-object) PyObject* "SCMOBJ_to_PyList") l))

(define (list->PyList* l)
  (let* ((len (length l))
         (pylist (PyList_New len)))
    (let lp ((l* l) (i 0))
      (if (and (not (null? l*)) (<= i len))
        (begin
          (PyList_SetItem pylist i (SCMOBJ_to_PyObject (car l*)))
          (lp (cdr l*) (+ i 1)))
        pylist))))

(define-structure *python-environment* __main__ __main__dict)

(define %python-environment% (##make-parameter #f))

(define (start-python)
  (Py_Initialize)
  (let* ((__main__ (PyImport_AddModuleObject "__main__"))
         (__main__dict (PyModule_GetDict __main__))
         (pyenv (make-*python-environment* __main__ __main__dict)))
    ;; (%python-environment% pyenv)
    pyenv))

(define (stop-python #!optional (program-name "pygambit"))
  (Py_Finalize))

(define (pyrun s #!key (pyenv (%python-environment%)) (locals #f))
  (if (not locals)
    (PyRun_String s Py_eval_input
                  (*python-environment*-__main__dict pyenv)
                  (*python-environment*-__main__dict pyenv))
    (PyRun_String s Py_eval_input
                  (*python-environment*-__main__dict pyenv)
                  locals)))

(define (pyrun* s #!key (pyenv (%python-environment%)) (locals #f))
  (if (not locals)
    (PyRun_String* s Py_eval_input
                   (*python-environment*-__main__dict pyenv)
                   (*python-environment*-__main__dict pyenv))
    (PyRun_String* s Py_eval_input
                   (*python-environment*-__main__dict pyenv)
                   locals)))

(define-macro (with-pyenv pyenv . things)
  `(begin
     (%python-environment% pyenv)
     ,@things))

;; f1.__code__.co_code.hex()

(##include "pydis.scm")

;;;============================================================================
