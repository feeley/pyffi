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

  ;; TODO: improve to actually find the appropriate directories

  (let ((os (cdr (shell-command "uname" #t))))

    (cond ((equal? os "Darwin\n")
           (let ((path "/usr/local/Cellar/python/3.7.6_1/Frameworks/Python.framework/Versions/3.7/"))
             `(begin
                (##meta-info
                 cc-options
                 ,(string-append "-I" path "include/python3.7m"))
                (##meta-info
                 ld-options
                 ,(string-append path "lib/libpython3.7m.dylib")))))

          ((equal? os "Linux\n")
           `(begin
              (##meta-info
               cc-options
               "-I/usr/include/python3.5"
               (##meta-info
                ld-options
                "/usr/lib/python3.5/config-3.5m-x86_64-linux-gnu/libpython3.5m.a"))))

          (else
           (error "OS is not supported, so can't find the python libraries")))))

(gen-meta-info)

;;;----------------------------------------------------------------------------

(c-declare "

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef PyObject *PyObjectPtr;
typedef PyObjectPtr PyScm;

___SCMOBJ release_PyObjectPtr(void *obj) {
  Py_DECREF(___CAST(PyObjectPtr,obj));
  return ___FIX(___NO_ERR);
}

___SCMOBJ SCMOBJ_to_PyScm(___SCMOBJ src, PyScm *dst, int arg_num) {

  PyScm obj;
  ___SCMOBJ ___temp; /* some macros need this */

  if (src == ___VOID) {

    obj = Py_None;
    Py_INCREF(obj);

  } else if (src == ___FAL) {

    obj = Py_False;
    Py_INCREF(obj);

  } else if (src == ___TRU) {

    obj = Py_True;
    Py_INCREF(obj);

  } else if (___FIXNUMP(src)) {

    if ((obj = PyLong_FromLongLong(___INT(src))) == NULL)
      return ___FIX(___STOC_HEAP_OVERFLOW_ERR+arg_num);

  } else if (___FLONUMP(src)) {

    if ((obj = PyFloat_FromDouble(___FLONUM_VAL(src))) == NULL)
      return ___FIX(___STOC_HEAP_OVERFLOW_ERR+arg_num);
   
  } else if (___CPXNUMP(src)) {

    /* TODO: handle complex */
   
  } else if (___STRINGP(src)) {

    if ((obj = PyUnicode_FromKindAndData(___CS_SELECT(PyUnicode_1BYTE_KIND,
                                                      PyUnicode_2BYTE_KIND,
                                                      PyUnicode_4BYTE_KIND),
                                         ___CAST(void*,
                                                 ___BODY_AS(src,___tSUBTYPED)),
                                         ___INT(___STRINGLENGTH(src))))
        == NULL)
      return ___FIX(___STOC_HEAP_OVERFLOW_ERR+arg_num);

  } else {

    return ___FIX(___STOC_TYPE_ERR+arg_num);

  }

  *dst = obj;

  return ___FIX(___NO_ERR);
}

___SCMOBJ PyScm_to_SCMOBJ(PyScm src, ___SCMOBJ *dst, int arg_num) {

  ___SCMOBJ obj;

  if (src == Py_None) {

    obj = ___VOID;

  } else if (src == Py_False) {

    obj = ___FAL;

  } else if (src == Py_True) {

    obj = ___TRU;

  } else if (PyLong_CheckExact(src)) {

    int overflow;
    ___LONGLONG val = PyLong_AsLongLongAndOverflow(src, &overflow);

    if (overflow) {
      return ___FIX(___CTOS_TYPE_ERR+arg_num); /* TODO: handle bignums */
    } else {
      return ___EXT(___LONGLONG_to_SCMOBJ)(___PSTATE, val, dst, arg_num);
    }

  } else if (PyFloat_CheckExact(src)) {

    double val = PyFloat_AS_DOUBLE(src);
    return ___EXT(___DOUBLE_to_SCMOBJ)(___PSTATE, val, dst, arg_num);

  } else if (PyComplex_CheckExact(src)) {

    double real_val = PyComplex_RealAsDouble(src);
    double imag_val = PyComplex_ImagAsDouble(src);
    /* TODO: handle complex */
    return ___FIX(0);

  } else if (PyUnicode_CheckExact(src)) {

    Py_ssize_t len;

    if (PyUnicode_READY(src)) /* convert to canonical representation */
      return ___FIX(___CTOS_HEAP_OVERFLOW_ERR+arg_num);

    len = PyUnicode_GET_LENGTH(src);

    obj = ___alloc_scmobj(___PSTATE, ___sSTRING, len<<___LCS);

    if (___FIXNUMP(obj))
      return ___FIX(___CTOS_HEAP_OVERFLOW_ERR+arg_num);

    switch (PyUnicode_KIND(src)) {
      case PyUnicode_1BYTE_KIND: {
        Py_UCS1 *data = PyUnicode_1BYTE_DATA(src);
        while (len-- > 0) ___STRINGSET(obj,___FIX(len),___CHR(data[len]));
        break;
      }
      case PyUnicode_2BYTE_KIND: {
        Py_UCS2 *data = PyUnicode_2BYTE_DATA(src);
        while (len-- > 0) ___STRINGSET(obj,___FIX(len),___CHR(data[len]));
        break;
      }
      case PyUnicode_4BYTE_KIND: {
        Py_UCS4 *data = PyUnicode_4BYTE_DATA(src);
        while (len-- > 0) ___STRINGSET(obj,___FIX(len),___CHR(data[len]));
        break;
      }
    }

  } else {

    return ___FIX(___CTOS_TYPE_ERR+arg_num);

  }

  *dst = obj;

  return ___FIX(___NO_ERR);
}

#define ___BEGIN_CFUN_SCMOBJ_to_PYSCM(src,dst,i) \
if ((___err = SCMOBJ_to_PyScm(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_PYSCM(src,dst,i) Py_DECREF(dst); }

#define ___BEGIN_CFUN_PYSCM_to_SCMOBJ(src,dst) \
if ((___err = PyScm_to_SCMOBJ(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_PYSCM_to_SCMOBJ(src,dst) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_PYSCM_to_SCMOBJ(src,dst,i) \
if ((___err = PyScm_to_SCMOBJ(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_PYSCM_to_SCMOBJ(src,dst,i) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_SCMOBJ_to_PYSCM(src,dst) \
if ((___err = SCMOBJ_to_PyScm(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_SCMOBJ_to_PYSCM(src,dst) Py_DECREF(dst); }

")

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

(define PyImport_AddModuleObject
  (c-lambda (PyScm) PyObject* "PyImport_AddModuleObject"))

(define PyRun_SimpleString
  (c-lambda (UTF-8-string) int "PyRun_SimpleString"))

(define PyRun_String
  (c-lambda (UTF-8-string int PyObject* PyObject*) PyScm "PyRun_String"))

(define PyModule_GetDict
  (c-lambda (PyObject*) PyObject* "PyModule_GetDict"))

(define PyDict_New
  (c-lambda () PyObject*/release "PyDict_New"))

;;;============================================================================
