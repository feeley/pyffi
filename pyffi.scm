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

;; Generate meta information to link to Python libs.

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

;; Get Python C API.

(c-declare #<<end-of-c-declare

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef PyObject *PyObjectPtr;

___SCMOBJ release_PyObjectPtr(void *obj) {
#ifdef ___DEBUG_PYTHON_REFCNT
    printf("Py_DECREF(%p) => %ld\n", obj, Py_REFCNT(obj));
    fflush(stdout);
#endif

    if (Py_IsInitialized())
      Py_DECREF(___CAST(PyObjectPtr, obj));
    return ___FIX(___NO_ERR);
}

end-of-c-declare
)

;;;----------------------------------------------------------------------------

;; Define PyObject* foreign type.

(c-define-type PyObject "PyObject")

(c-define-type _PyObject* (nonnull-pointer
                          PyObject
                          (PyObject*
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
                           PyObject*/module)))

(c-define-type PyObject* "void*"
                         "PYOBJECTPTR_to_SCMOBJ"
                         "SCMOBJ_to_PYOBJECTPTR"
                         #t)

;;;----------------------------------------------------------------------------

;; Define PyObject* subtypes.

(define-macro (define-python-subtype-type subtype)
  (define name (string->symbol (string-append "PyObject*/" subtype)))
  (define _name (string->symbol (string-append "_PyObject*/" subtype)))
  (define type (string-append "PyObjectPtr_" subtype))
  (define to-scmobj (string-append (string-upcase type) "_to_SCMOBJ"))
  (define from-scmobj (string-append "SCMOBJ_to_" (string-upcase type)))
  `(begin
     (c-declare ,(string-append "typedef PyObjectPtr " type ";"))
     (c-define-type ,_name (nonnull-pointer PyObject ,name))
     (c-define-type ,name "void*" ,to-scmobj ,from-scmobj #t)))

(define-python-subtype-type "None")
(define-python-subtype-type "bool")
(define-python-subtype-type "int")
(define-python-subtype-type "float")
(define-python-subtype-type "complex")
(define-python-subtype-type "bytes")
(define-python-subtype-type "bytearray")
(define-python-subtype-type "str")
(define-python-subtype-type "list")
(define-python-subtype-type "dict")
(define-python-subtype-type "frozenset")
(define-python-subtype-type "set")
(define-python-subtype-type "tuple")
(define-python-subtype-type "module")

;;;----------------------------------------------------------------------------

;; Generator of converter macros.

(define-macro (define-python-converter-macros _SUBTYPE)
  `(c-declare ,(string-append "

#define ___BEGIN_CFUN_SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src,dst,i) \
  if ((___err = SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src,dst,i) }

#define ___BEGIN_CFUN_PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(src,dst) \
  if ((___err = PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(src,dst) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(src,dst,i) \
  if ((___err = PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(src,dst,i) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src,dst) \
  if ((___err = SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src,dst) }
")))

;;;----------------------------------------------------------------------------

;; Converter for Python* type that detects the subtype.

(c-declare #<<end-of-c-declare

___SCMOBJ PYOBJECTPTR_to_SCMOBJ(PyObjectPtr src, ___SCMOBJ *dst, int arg_num) {

  ___SCMOBJ tag;

  if ( src == NULL )
    {
      *dst = ___VOID;
      return ___FIX(___NO_ERR);
    }

#ifdef ___C_TAG_PyObject_2a__2f_None
  if (src==Py_None)
    tag = ___C_TAG_PyObject_2a__2f_None;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bool
  if (src==Py_False || src==Py_True)
    tag = ___C_TAG_PyObject_2a__2f_bool;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_int
  if (PyLong_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_int;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_float
  if (PyFloat_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_float;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_complex
  if (PyComplex_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_complex;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytes
  if (PyBytes_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_bytes;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytearray
  if (PyByteArray_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_bytearray;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_str
  if (PyUnicode_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_str;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_list
  if (PyList_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_list;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_dict
  if (PyDict_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_dict;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_frozenset
  if (PyFrozenSet_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_frozenset;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_set
  if (PyAnySet_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_set;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_tuple
  if (PyTuple_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_tuple;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_module
  if (PyModule_CheckExact(src))
    tag = ___C_TAG_PyObject_2a__2f_module;
  else
#endif

  tag = ___C_TAG_PyObject_2a_;

  Py_INCREF(src);

#ifdef ___DEBUG_PYTHON_REFCNT
  printf("PYOBJECTPTR_to_SCMOBJ Py_INCREF(%p) => %ld\n", src, Py_REFCNT(src));
  fflush(stdout);
#endif

  return ___NONNULLPOINTER_to_SCMOBJ(___PSTATE,
                                     src,
                                     tag,
                                     release_PyObjectPtr,
                                     dst,
                                     arg_num);
}

___SCMOBJ SCMOBJ_to_PYOBJECTPTR(___SCMOBJ src, void **dst, int arg_num) {

  ___PSGET

#define CONVERT_TO_NONNULLPOINTER(tag) \
  ___SCMOBJ_to_NONNULLPOINTER(___PSP src, dst, tag, arg_num)

#define TRY_CONVERT_TO_NONNULLPOINTER(tag) \
  if (CONVERT_TO_NONNULLPOINTER(tag) == ___FIX(___NO_ERR)) \
    return ___FIX(___NO_ERR)

#ifdef ___C_TAG_PyObject_2a__2f_None
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_None);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bool
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_bool);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_int
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_int);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_float
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_float);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_complex
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_complex);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytes
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_bytes);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bytearray
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_bytearray);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_str
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_str);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_list
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_list);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_dict
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_dict);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_frozenset
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_frozenset);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_set
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_set);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_tuple
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_tuple);
#endif

#ifdef ___C_TAG_PyObject_2a__2f_module
  TRY_CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a__2f_module);
#endif

  return CONVERT_TO_NONNULLPOINTER(___C_TAG_PyObject_2a_);
}

end-of-c-declare
)

(define-python-converter-macros "")

;;;----------------------------------------------------------------------------

;; Converters for Python* subtypes.

(define-macro (define-python-subtype-converters subtype check)
  (define _SUBTYPE (string-append "_" (string-upcase subtype)))
  (define tag (string-append "___C_TAG_PyObject_2a__2f_" subtype))
  `(begin
     (c-declare
       ,(string-append "

#ifdef " tag "

___SCMOBJ PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(PyObjectPtr_" subtype " src, ___SCMOBJ *dst, int arg_num) {

  if (!(" check "))
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

  Py_INCREF(src);

#ifdef ___DEBUG_PYTHON_REFCNT
  printf(\"PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ Py_INCREF(" _SUBTYPE " %p) => %ld\\n\", src, Py_REFCNT(src));
  fflush(stdout);
#endif

  return ___NONNULLPOINTER_to_SCMOBJ(___PSTATE,
                                     src,
                                     " tag ",
                                     release_PyObjectPtr,
                                     dst,
                                     arg_num);
}

___SCMOBJ SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(___SCMOBJ src, void **dst, int arg_num) {

  return ___SCMOBJ_to_NONNULLPOINTER(___PSA(___PSTATE)
                                     src,
                                     dst,
                                     " tag ",
                                     arg_num);
}

#endif

"))
     (define-python-converter-macros ,_SUBTYPE)))

(define-python-subtype-converters "None"      "src==Py_None")
(define-python-subtype-converters "bool"      "src==Py_False || src==Py_True")
(define-python-subtype-converters "int"       "PyLong_CheckExact(src)")
(define-python-subtype-converters "float"     "PyFloat_CheckExact(src)")
(define-python-subtype-converters "complex"   "PyComplex_CheckExact(src)")
(define-python-subtype-converters "bytes"     "PyBytes_CheckExact(src)")
(define-python-subtype-converters "bytearray" "PyByteArray_CheckExact(src)")
(define-python-subtype-converters "str"       "PyUnicode_CheckExact(src)")
(define-python-subtype-converters "list"      "PyList_CheckExact(src)")
(define-python-subtype-converters "dict"      "PyDict_CheckExact(src)")
(define-python-subtype-converters "frozenset" "PyFrozenSet_CheckExact(src)")
(define-python-subtype-converters "set"       "PyAnySet_CheckExact(src) && !PyFrozenSet_CheckExact(src)")
(define-python-subtype-converters "tuple"     "PyTuple_CheckExact(src)")
(define-python-subtype-converters "module"    "PyModule_CheckExact(src)")

;;;----------------------------------------------------------------------------

;; Use for debugging
(define _Py_REFCNT
  (c-lambda (PyObject*) ssize_t
    "___return(Py_REFCNT(___arg1));"))

;; Interface to Python API.

(define Py_eval_input   ((c-lambda () int "___return(Py_eval_input);")))
(define Py_file_input   ((c-lambda () int "___return(Py_file_input);")))
(define Py_single_input ((c-lambda () int "___return(Py_single_input);")))

(define Py_Initialize
  (c-lambda () void
    "Py_Initialize"))

(define Py_Finalize
  (c-lambda () void
    "Py_Finalize"))

(define PyUnicode_FromString
  (c-lambda (UTF-8-string) PyObject*/str
    "PyUnicode_FromString"))

(define PyRun_SimpleString
  (c-lambda (UTF-8-string) int "PyRun_SimpleString"))

(define PyRun_String
  (c-lambda (UTF-8-string int PyObject*/dict PyObject*/dict) PyObject*
    "PyRun_String"))


(define PyImport_AddModuleObject
  (c-lambda (PyObject*/str) PyObject*/module
    "PyImport_AddModuleObject"))

(define PyModule_GetDict
  (c-lambda (PyObject*/module) PyObject*/dict
    "PyModule_GetDict"))

;;;----------------------------------------------------------------------------

;; Converters between Scheme and subtypes of Python* foreign objects.

;; TODO: check for errors and implement conversion of other subtypes...

(define PyObject*/str->string
  (c-lambda (PyObject*/str) scheme-object
    "
     ___SCMOBJ obj = ___VOID;

     if (!PyUnicode_READY(___arg1)) { /* convert to canonical representation */

       Py_ssize_t len = PyUnicode_GET_LENGTH(___arg1);

       obj = ___alloc_scmobj(___PSTATE, ___sSTRING, len << ___LCS);

       if (!___FIXNUMP(obj))
         switch (PyUnicode_KIND(___arg1)) {
           case PyUnicode_1BYTE_KIND:
             {
               Py_UCS1 *data = PyUnicode_1BYTE_DATA(___arg1);
               while (len-- > 0)
                 ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
               break;
             }
           case PyUnicode_2BYTE_KIND:
             {
               Py_UCS2 *data = PyUnicode_2BYTE_DATA(___arg1);
               while (len-- > 0)
                 ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
               break;
             }
           case PyUnicode_4BYTE_KIND:
             {
               Py_UCS4 *data = PyUnicode_4BYTE_DATA(___arg1);
               while (len-- > 0)
                 ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
               break;
             }
         }
     }

     ___return(obj);
    "))

(define string->PyObject*/str
  (c-lambda (scheme-object) PyObject*/str
    "
     PyObject* obj =
       PyUnicode_FromKindAndData(___CS_SELECT(PyUnicode_1BYTE_KIND,
                                              PyUnicode_2BYTE_KIND,
                                              PyUnicode_4BYTE_KIND),
                                 ___CAST(void*,
                                         ___BODY_AS(___arg1,___tSUBTYPED)),
                                 ___INT(___STRINGLENGTH(___arg1)));
     ___return(obj);
    "))

;;;----------------------------------------------------------------------------

;; TODO: get rid of this by improving Gambit C interface.

(define dummy
  (list
   (c-lambda () _PyObject* "___return(NULL);")
   (c-lambda () _PyObject*/None "___return(NULL);")
   (c-lambda () _PyObject*/bool "___return(NULL);")
   (c-lambda () _PyObject*/int "___return(NULL);")
   (c-lambda () _PyObject*/float "___return(NULL);")
   (c-lambda () _PyObject*/complex "___return(NULL);")
   (c-lambda () _PyObject*/bytes "___return(NULL);")
   (c-lambda () _PyObject*/bytearray "___return(NULL);")
   (c-lambda () _PyObject*/str "___return(NULL);")
   (c-lambda () _PyObject*/list "___return(NULL);")
   (c-lambda () _PyObject*/dict "___return(NULL);")
   (c-lambda () _PyObject*/frozenset "___return(NULL);")
   (c-lambda () _PyObject*/set "___return(NULL);")
   (c-lambda () _PyObject*/tuple "___return(NULL);")
   (c-lambda () _PyObject*/module "___return(NULL);")))

;;;----------------------------------------------------------------------------
