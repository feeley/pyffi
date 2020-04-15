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

#define DEBUG_PYTHON_REFCNT_not

#ifdef DEBUG_PYTHON_REFCNT

#define PYOBJECTPTR_INCREF(obj, where) \
do { \
  Py_INCREF(obj); \
  printf(where " REFCNT(%p)=%ld after INCREF\n", obj, Py_REFCNT(obj)); \
  fflush(stdout); \
} while (0)

#define PYOBJECTPTR_DECREF(obj, where) \
do { \
  printf(where " REFCNT(%p)=%ld before DECREF\n", obj, Py_REFCNT(obj)); \
  fflush(stdout); \
  Py_DECREF(obj); \
} while (0)

#define PYOBJECTPTR_REFCNT_SHOW(obj, where) \
do { \
  printf(where " REFCNT(%p)=%ld\n", obj, Py_REFCNT(obj)); \
  fflush(stdout); \
} while (0)

#else

#define PYOBJECTPTR_INCREF(obj, where) Py_INCREF(obj)
#define PYOBJECTPTR_DECREF(obj, where) Py_DECREF(obj)
#define PYOBJECTPTR_REFCNT_SHOW(obj, where)

#endif

___SCMOBJ release_PyObjectPtr(void *obj) {

  if (Py_IsInitialized()) // Avoid mem management after Python is shutdown
    PYOBJECTPTR_DECREF(___CAST(PyObjectPtr, obj), "release_PyObjectPtr");

  return ___FIX(___NO_ERR);
}

end-of-c-declare
)

;;;----------------------------------------------------------------------------

;; Define PyObject* foreign type.

(c-define-type PyObject "PyObject")

(c-define-type _PyObject*
               (nonnull-pointer
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
                 PyObject*/module
                 )))

(c-define-type PyObject*
               "void*"
               "PYOBJECTPTR_to_SCMOBJ"
               "SCMOBJ_to_PYOBJECTPTR"
               #t)

(c-define-type PyObject*!own
               "void*"
               "PYOBJECTPTR_OWN_to_SCMOBJ"
               "SCMOBJ_to_PYOBJECTPTR_OWN"
               #t)

;;;----------------------------------------------------------------------------

;; Define PyObject* subtypes.

(define-macro (define-python-subtype-type subtype)
  (define type (string-append "PyObjectPtr_" subtype))
  (define _name (string->symbol (string-append "_PyObject*/" subtype)))
  (define name (string->symbol (string-append "PyObject*/" subtype)))
  (define name-own (string->symbol (string-append "PyObject*!own/" subtype)))
  (define TYPE (string-append "PYOBJECTPTR_" (string-upcase subtype)))
  (define TYPE-OWN (string-append "PYOBJECTPTR_OWN_" (string-upcase subtype)))
  (define to-scmobj (string-append TYPE "_to_SCMOBJ"))
  (define from-scmobj (string-append "SCMOBJ_to_" TYPE))
  (define to-scmobj-own (string-append TYPE-OWN "_to_SCMOBJ"))
  (define from-scmobj-own (string-append "SCMOBJ_to_" TYPE-OWN))
  `(begin
     (c-declare ,(string-append "typedef PyObjectPtr " type ";"))
     (c-define-type ,_name (nonnull-pointer PyObject ,name))
     (c-define-type ,name "void*" ,to-scmobj ,from-scmobj #t)
     (c-define-type ,name-own "void*" ,to-scmobj-own ,from-scmobj-own #t)))

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
(define-python-subtype-type "type")

;;;----------------------------------------------------------------------------

;; Define PyTypeObject* foreign type.

;; NOTE: Not sure yet if we want to use raw PyTypeObjects.

(c-define-type PyTypeObject "PyTypeObject")

(c-define-type PyTypeObject*
               (nonnull-pointer PyTypeObject (PyTypeObject*)))

;;;----------------------------------------------------------------------------

;; Generator of converter macros.

(define-macro (define-converter-macros _SUBTYPE _OWN release)
  `(c-declare ,(string-append "

#define ___BEGIN_CFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst,i) \
  if ((___err = SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst,i) " release "}

#define ___BEGIN_CFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst) \
  if ((___err = PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst,i) \
  if ((___err = PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_PYOBJECTPTR" _OWN _SUBTYPE "_to_SCMOBJ(src,dst,i) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst) \
  if ((___err = SCMOBJ_to_PYOBJECTPTR" _SUBTYPE "(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_SCMOBJ_to_PYOBJECTPTR" _OWN _SUBTYPE "(src,dst) " release "}
")))

;;;----------------------------------------------------------------------------

;; Converter for Python* type that detects the subtype.

(c-declare #<<end-of-c-declare

___SCMOBJ PYOBJECTPTR_to_SCMOBJ(PyObjectPtr src, ___SCMOBJ *dst, int arg_num) {

  ___SCMOBJ tag;

  if (src == NULL)
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

#ifdef ___C_TAG_PyObject_2a__2f_None
  if (src == Py_None)
    tag = ___C_TAG_PyObject_2a__2f_None;
  else
#endif

#ifdef ___C_TAG_PyObject_2a__2f_bool
  if (src == Py_False || src == Py_True)
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

  PYOBJECTPTR_REFCNT_SHOW(src, "PYOBJECTPTR_to_SCMOBJ");

  return ___NONNULLPOINTER_to_SCMOBJ(___PSTATE,
                                     src,
                                     tag,
                                     release_PyObjectPtr,
                                     dst,
                                     arg_num);
}

___SCMOBJ PYOBJECTPTR_OWN_to_SCMOBJ(PyObjectPtr src, ___SCMOBJ *dst, int arg_num) {
  if (src == NULL)
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);
  PYOBJECTPTR_INCREF(src, "PYOBJECTPTR_OWN_to_SCMOBJ");
  return PYOBJECTPTR_to_SCMOBJ(src, dst, arg_num);
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

(define-converter-macros "" "" "")
(define-converter-macros "" "_OWN" "___EXT(___release_foreign) (src); ")

;;;----------------------------------------------------------------------------

;; Converters for Python* subtypes.

(define-macro (define-subtype-converters subtype check)
  (define _SUBTYPE (string-append "_" (string-upcase subtype)))
  (define tag (string-append "___C_TAG_PyObject_2a__2f_" subtype))
  `(begin
     (c-declare
       ,(string-append "

#ifdef " tag "

___SCMOBJ PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ(PyObjectPtr_" subtype " src, ___SCMOBJ *dst, int arg_num) {

  if (src == NULL || !(" check "))
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

  PYOBJECTPTR_REFCNT_SHOW(src, \"PYOBJECTPTR" _SUBTYPE "_to_SCMOBJ\");

  return ___NONNULLPOINTER_to_SCMOBJ(___PSTATE,
                                     src,
                                     " tag ",
                                     release_PyObjectPtr,
                                     dst,
                                     arg_num);
}

___SCMOBJ PYOBJECTPTR_OWN" _SUBTYPE "_to_SCMOBJ(PyObjectPtr_" subtype " src, ___SCMOBJ *dst, int arg_num) {

  if (src == NULL || !(" check "))
    return ___FIX(___CTOS_NONNULLPOINTER_ERR+arg_num);

  PYOBJECTPTR_INCREF(src, \"PYOBJECTPTR_OWN" _SUBTYPE "_to_SCMOBJ\");

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
     (define-converter-macros ,_SUBTYPE "" "")
     (define-converter-macros ,_SUBTYPE "_OWN" "___EXT(___release_foreign) (src); ")))

(define-subtype-converters "None"      "src == Py_None")
(define-subtype-converters "bool"      "src == Py_False || src == Py_True")
(define-subtype-converters "int"       "PyLong_CheckExact(src)")
(define-subtype-converters "float"     "PyFloat_CheckExact(src)")
(define-subtype-converters "complex"   "PyComplex_CheckExact(src)")
(define-subtype-converters "bytes"     "PyBytes_CheckExact(src)")
(define-subtype-converters "bytearray" "PyByteArray_CheckExact(src)")
(define-subtype-converters "str"       "PyUnicode_CheckExact(src)")
(define-subtype-converters "list"      "PyList_CheckExact(src)")
(define-subtype-converters "dict"      "PyDict_CheckExact(src)")
(define-subtype-converters "frozenset" "PyFrozenSet_CheckExact(src)")
(define-subtype-converters "set"       "PyAnySet_CheckExact(src) && !PyFrozenSet_CheckExact(src)")
(define-subtype-converters "tuple"     "PyTuple_CheckExact(src)")
(define-subtype-converters "module"    "PyModule_CheckExact(src)")

;;;----------------------------------------------------------------------------

(c-declare #<<end-of-c-declare

PyObjectPtr check_PyObjectPtr(PyObjectPtr result, ___SCMOBJ *err, ___SCMOBJ *errdata, ___SCMOBJ *errhandler) {
  if (result == NULL) {
    PyObjectPtr tuple;
    PyObjectPtr type;
    PyObjectPtr value;
    PyObjectPtr traceback;
    tuple = PyTuple_New(2);
    PyErr_Fetch(&type, &value, &traceback);
    PyErr_NormalizeException(&type, &value, &traceback);
    PyTuple_SET_ITEM(tuple, 0, value);
    PyTuple_SET_ITEM(tuple, 1, traceback);
    *err = PYOBJECTPTR_to_SCMOBJ(tuple, errdata, ___RETURN_POS);
    *errhandler = ___GLO_github_2e_com_2f_feeley_2f_pyffi_23_pyffi_2d_error_2d_handler;
  }
  return result;
}

#define call_with_check_PyObjectPtr(call) \
___return(check_PyObjectPtr(call, &___err, &___errdata, &___errhandler));

end-of-c-declare
)

(define-type python-exception
  id: A9EC1C11-A6D8-4357-99E6-655B75ADC09E
  type-exhibitor: python-exception-type
  data
  proc
  args)

(##structure-display-exception-handler-register!
 (##type-id (python-exception-type))
 (lambda (exc port)
   (if port
       (display (string-append "Python error: "
                               (PyObject*/str->string
                                (PyObject_Repr (python-exception-data exc)))
                               "\n")
                port)
       (cons (python-exception-proc exc)
             (python-exception-args exc)))))

(define (pyffi-error-handler code data proc . args)
  (raise (make-python-exception data proc args)))

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

(define PyBool_FromLong
  (c-lambda (long) PyObject*/bool
    "call_with_check_PyObjectPtr(PyBool_FromLong(___arg1));"))

(define PyLong_FromUnicodeObject
  (c-lambda (PyObject*/str int) PyObject*/int
    "call_with_check_PyObjectPtr(PyLong_FromUnicodeObject(___arg1,___arg2));"))

(define PyUnicode_FromString
  (c-lambda (nonnull-UTF-8-string) PyObject*/str
    "call_with_check_PyObjectPtr(PyUnicode_FromString(___arg1));"))

(define PyRun_SimpleString
  (c-lambda (nonnull-UTF-8-string) int
    "PyRun_SimpleString"))

(define PyRun_String
  (c-lambda (nonnull-UTF-8-string int PyObject*/dict PyObject*/dict) PyObject*
    "call_with_check_PyObjectPtr(PyRun_String(___arg1,___arg2,___arg3,___arg4));"))

(define PyImport_AddModuleObject
  (c-lambda (PyObject*/str) PyObject*/module
    "call_with_check_PyObjectPtr(PyImport_AddModuleObject(___arg1));"))

;; Fails when called with call_with_check_PyObjectPtr
;; (define PyImport_ImportModule
;;   (c-lambda (nonnull-UTF-8-string) PyObject*/module
;;     "call_with_check_PyObjectPtr(PyImport_AddModule(___arg1));"))
(define PyImport_ImportModule
  (c-lambda (nonnull-UTF-8-string) PyObject*/module
    "PyImport_ImportModule"))

(define PyImport_ImportModuleEx
  (c-lambda (nonnull-UTF-8-string PyObject*/dict PyObject*/dict PyObject*/list)
            PyObject*/module
            "
call_with_check_PyObjectPtr(PyImport_ImportModuleEx(___arg1,___arg2,___arg3,___arg4));
"))

(define PyModule_GetDict
  (c-lambda (PyObject*/module) PyObject*/dict
    "call_with_check_PyObjectPtr(PyModule_GetDict(___arg1));"))

(define PyDict_New
  (c-lambda () PyObject*/dict
    "call_with_check_PyObjectPtr(PyDict_New());"))

(define PyList_New
  (c-lambda (int) PyObject*/list
            "call_with_check_PyObjectPtr(PyList_New(___arg1));"))

(define PyObject_CallMethod
  (c-lambda (PyObject* nonnull-UTF-8-string nonnull-UTF-8-string) PyObject*
    "call_with_check_PyObjectPtr(PyObject_CallMethod(___arg1,___arg2,___arg3));"))

(define PyObject_GetAttrString
  (c-lambda (PyObject* nonnull-UTF-8-string) PyObject*
    "call_with_check_PyObjectPtr(PyObject_GetAttrString(___arg1,___arg2));"))

(define PyObject_Length
  (c-lambda (PyObject*) ssize_t
    "PyObject_Length"))

(define PyObject_Repr
  (c-lambda (PyObject*) PyObject*/str
    "call_with_check_PyObjectPtr(PyObject_Repr(___arg1));"))

;; Get object type from struct field, no new reference.
(define PyObject*-type
  (c-lambda (_PyObject*) PyTypeObject*
    "___return(___arg1->ob_type);"))

(define PyObject*-type-name
  (c-lambda (_PyObject*) nonnull-UTF-8-string
    "___return(___arg1->ob_type->tp_name);"))

(define Py_SetPath
  (c-lambda (nonnull-wchar_t-string) void
    "Py_SetPath"))

(define Py_SetPythonHome
  (c-lambda (nonnull-wchar_t-string) void
    "Py_SetPythonHome"))

;;;----------------------------------------------------------------------------

;; Converters between Scheme and subtypes of Python* foreign objects.

;; TODO: check for errors and implement conversion of other subtypes...

(define PyObject*/None->void
  (c-lambda (PyObject*/None) scheme-object "

___return(___VOID);

"))

(define void->PyObject*/None
  (c-lambda (scheme-object) PyObject*/None "

___SCMOBJ src = ___arg1;
PyObjectPtr dst = NULL;

if (___EQP(src, ___VOID)) {
  dst = Py_None;
  PYOBJECTPTR_INCREF(dst, \"void->PyObject*/None\");
}

___return(dst);

"))

(define PyObject*/bool->boolean
  (c-lambda (PyObject*/bool) scheme-object "

___return(___BOOLEAN(___arg1 != Py_False));

"))

(define boolean->PyObject*/bool
  (c-lambda (scheme-object) PyObject*/bool "

___SCMOBJ src = ___arg1;
PyObjectPtr dst = NULL;

if (___BOOLEANP(src)) {
  dst = ___FALSEP(src) ? Py_False : Py_True;
  PYOBJECTPTR_INCREF(dst, \"boolean->PyObject*/bool\");
}

___return(dst);

"))

(define (PyObject*/int->exact-integer src)
  (let ((dst
         ((c-lambda (PyObject*/int) scheme-object "

PyObjectPtr src = ___arg1;
___SCMOBJ dst = ___VOID;

int overflow;
___LONGLONG val = PyLong_AsLongLongAndOverflow(src, &overflow);

if (!overflow)
  {
    if (___EXT(___LONGLONG_to_SCMOBJ)(___PSTATE,
                                      val,
                                      &dst,
                                      ___RETURN_POS)
        != ___FIX(___NO_ERR))
      dst = ___VOID;
  }

___return(___EXT(___release_scmobj) (dst));

")
          src)))
    (if (eq? dst (void))
        (error "PyObject*/int->exact-integer conversion error")
        dst)))

(define exact-integer->PyObject*/int
  (c-lambda (scheme-object) PyObject*/int "

___SCMOBJ src = ___arg1;
PyObjectPtr dst = NULL;

if (___FIXNUMP(src)) {
  dst = PyLong_FromLongLong(___INT(src));
  PYOBJECTPTR_REFCNT_SHOW(dst, \"exact-integer->PyObject*/int\");
}

___return(dst);

"))

(define (PyObject*/str->string src)
  (let ((dst
         ((c-lambda (PyObject*/str) scheme-object "

PyObjectPtr src = ___arg1;
___SCMOBJ dst = ___VOID;

if (!PyUnicode_READY(src)) { /* convert to canonical representation */

  Py_ssize_t len = PyUnicode_GET_LENGTH(src);

  dst = ___EXT(___alloc_scmobj) (___PSTATE, ___sSTRING, len << ___LCS);

  if (___FIXNUMP(dst))
    dst = ___VOID;
  else
    switch (PyUnicode_KIND(src)) {
      case PyUnicode_1BYTE_KIND:
        {
          Py_UCS1 *data = PyUnicode_1BYTE_DATA(src);
          while (len-- > 0)
            ___STRINGSET(dst, ___FIX(len), ___CHR(data[len]));
          break;
        }
      case PyUnicode_2BYTE_KIND:
        {
          Py_UCS2 *data = PyUnicode_2BYTE_DATA(src);
          while (len-- > 0)
            ___STRINGSET(dst, ___FIX(len), ___CHR(data[len]));
          break;
        }
      case PyUnicode_4BYTE_KIND:
        {
          Py_UCS4 *data = PyUnicode_4BYTE_DATA(src);
          while (len-- > 0)
            ___STRINGSET(dst, ___FIX(len), ___CHR(data[len]));
          break;
        }
    }
}

___return(___EXT(___release_scmobj) (dst));

")
          src)))
    (if (eq? dst (void))
        (error "PyObject*/str->string conversion error")
        dst)))

(define string->PyObject*/str
  (c-lambda (scheme-object) PyObject*/str "

___SCMOBJ src = ___arg1;

if (!___STRINGP(src)) {
  ___return(NULL);
} else {
  PyObjectPtr dst = PyUnicode_FromKindAndData(___CS_SELECT(
                                                PyUnicode_1BYTE_KIND,
                                                PyUnicode_2BYTE_KIND,
                                                PyUnicode_4BYTE_KIND),
                                              ___CAST(void*,
                                                ___BODY_AS(src,___tSUBTYPED)),
                                              ___INT(___STRINGLENGTH(src)));
  PYOBJECTPTR_REFCNT_SHOW(dst, \"string->PyObject*/str\");
  ___return(dst);
}

"))

(define (PyObject*/tuple->vector src)
  (let ((dst
         ((c-lambda (PyObject*/tuple) scheme-object "

PyObjectPtr src = ___arg1;
Py_ssize_t len = PyTuple_GET_SIZE(src);
___SCMOBJ dst = ___EXT(___make_vector) (___PSTATE, len, ___FIX(0));

if (___FIXNUMP(dst)) {
  ___return(___VOID);
} else {
  Py_ssize_t i;
  for (i=0; i<len; i++) {
    PyObjectPtr item = PyTuple_GET_ITEM(src, i);
    ___SCMOBJ item_scmobj;
    if (PYOBJECTPTR_OWN_to_SCMOBJ(item, &item_scmobj, ___RETURN_POS)
        == ___FIX(___NO_ERR)) {
      ___VECTORSET(dst, ___FIX(i), ___EXT(___release_scmobj) (item_scmobj))
    } else {
      ___EXT(___release_scmobj) (dst);
      ___return(___VOID);
    }
  }
  ___return(___EXT(___release_scmobj) (dst));
}

")
          src)))
    (if (eq? dst (void))
        (error "PyObject*/tuple->vector conversion error")
        dst)))

(define vector->PyObject*/tuple
  (c-lambda (scheme-object) PyObject*/tuple "

___SCMOBJ src = ___arg1;

if (!___VECTORP(src)) {
  ___return(NULL);
} else {
  Py_ssize_t len = ___INT(___VECTORLENGTH(src));
  PyObjectPtr dst = PyTuple_New(len);
  if (dst == NULL) {
    ___return(NULL);
  } else {
    Py_ssize_t i;
    for (i=0; i<len; i++) {
      ___SCMOBJ item = ___VECTORREF(src,___FIX(i));
      void* item_py;
      if (SCMOBJ_to_PYOBJECTPTR(item, &item_py, ___RETURN_POS)
          == ___FIX(___NO_ERR)) {
        PYOBJECTPTR_INCREF(___CAST(PyObjectPtr,item_py), \"vector->PyObject*/tuple\");
        PyTuple_SET_ITEM(dst, i, ___CAST(PyObjectPtr,item_py));
      } else {
        PYOBJECTPTR_DECREF(dst, \"vector->PyObject*/tuple\");
        ___return(NULL);
      }
    }
    PYOBJECTPTR_REFCNT_SHOW(dst, \"vector->PyObject*/tuple\");
    ___return(dst);
  }
}

"))

(define (PyObject*/list->vector src)
  (let ((dst
         ((c-lambda (PyObject*/list) scheme-object "

PyObjectPtr src = ___arg1;
Py_ssize_t len = PyList_GET_SIZE(src);
___SCMOBJ dst = ___EXT(___make_vector) (___PSTATE, len, ___FIX(0));

if (___FIXNUMP(dst)) {
  ___return(___VOID);
} else {
  Py_ssize_t i;
  for (i=0; i<len; i++) {
    PyObjectPtr item = PyList_GET_ITEM(src, i);
    ___SCMOBJ item_scmobj;
    if (PYOBJECTPTR_OWN_to_SCMOBJ(item, &item_scmobj, ___RETURN_POS)
        == ___FIX(___NO_ERR)) {
      ___VECTORSET(dst, ___FIX(i), ___EXT(___release_scmobj) (item_scmobj))
    } else {
      ___EXT(___release_scmobj) (dst);
      ___return(___VOID);
    }
  }
  ___return(___EXT(___release_scmobj) (dst));
}

")
          src)))
    (if (eq? dst (void))
        (error "PyObject*/list->vector conversion error")
        dst)))

(define vector->PyObject*/list
  (c-lambda (scheme-object) PyObject*/list "

___SCMOBJ src = ___arg1;

if (!___VECTORP(src)) {
  ___return(NULL);
} else {
  Py_ssize_t len = ___INT(___VECTORLENGTH(src));
  PyObjectPtr dst = PyList_New(len);
  if (dst == NULL) {
    ___return(NULL);
  } else {
    Py_ssize_t i;
    for (i=0; i<len; i++) {
      ___SCMOBJ item = ___VECTORREF(src,___FIX(i));
      void* item_py;
      if (SCMOBJ_to_PYOBJECTPTR(item, &item_py, ___RETURN_POS)
          == ___FIX(___NO_ERR)) {
        PYOBJECTPTR_INCREF(___CAST(PyObjectPtr,item_py), \"vector->PyObject*/list\");
        PyList_SET_ITEM(dst, i, ___CAST(PyObjectPtr,item_py));
      } else {
        PYOBJECTPTR_DECREF(dst, \"vector->PyObject*/list\");
        ___return(NULL);
      }
    }
    PYOBJECTPTR_REFCNT_SHOW(dst, \"vector->PyObject*/list\");
    ___return(dst);
  }
}

"))

;;;----------------------------------------------------------------------------

;; Generic converters.

(define (PyObject*->object src)

  (define (conv src)
    (case (car (##foreign-tags src))
      ((PyObject*/None)  (PyObject*/None->void src))
      ((PyObject*/bool)  (PyObject*/bool->boolean src))
      ((PyObject*/int)   (PyObject*/int->exact-integer src))
      ((PyObject*/str)   (PyObject*/str->string src))
      ((PyObject*/tuple) (list-conv src))
      ((PyObject*/list)  (vector-conv src))
      (else              (error "can't convert" src))))

  (define (vector-conv src)
    (let ((vect (PyObject*/list->vector src)))
      (let loop ((i (fx- (vector-length vect) 1)))
        (if (fx< i 0)
            vect
            (begin
              (vector-set! vect i (conv (vector-ref vect i)))
              (loop (fx- i 1)))))))

  (define (list-conv src)
    (let* ((vect (PyObject*/tuple->vector src))
           (len (vector-length vect)))
      (let loop ((i (fx- len 1)) (lst '()))
        (if (fx< i 0)
            lst
            (loop (fx- i 1)
                  (cons (conv (vector-ref vect i))
                        lst))))))

  (if (##foreign? src)
      (conv src)
      src))

(define (object->PyObject* src)

  (define (conv src)
    (cond ((eq? src (void))             (void->PyObject*/None src))
          ((boolean? src)               (boolean->PyObject*/bool src))
          ((exact-integer? src)         (exact-integer->PyObject*/int src))
          ((string? src)                (string->PyObject*/str src))
          ((vector? src)                (vector-conv src))
          ((or (null? src) (pair? src)) (list-conv src))
          (else                         (error "can't convert" src))))

  (define (vector-conv src)
    (let* ((len (vector-length src))
           (vect (make-vector len)))
      (let loop ((i (fx- len 1)))
        (if (fx< i 0)
            (vector->PyObject*/list vect)
            (begin
              (vector-set! vect i (conv (vector-ref src i)))
              (loop (fx- i 1)))))))

  (define (list-conv src)
    (let loop1 ((probe src) (len 0))
      (if (pair? probe)
          (loop1 (cdr probe) (fx+ len 1))
          (let ((vect (make-vector len)))
            (let loop2 ((probe src) (i 0))
              (if (not (and (fx< i len) (pair? probe)))
                  (vector->PyObject*/tuple vect)
                  (begin
                    (vector-set! vect i (conv (car probe)))
                    (loop2 (cdr probe) (fx+ i 1)))))))))

  (conv src))

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
