#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <patchlevel.h>

typedef PyObject *PyObjectPtr;
typedef PyObjectPtr PyScm;

___SCMOBJ release_PyObjectPtr(void *obj)
{
    Py_DECREF(___CAST(PyObjectPtr, obj));
    return ___FIX(___NO_ERR);
}

___SCMOBJ SCMOBJ_to_PyScm(___SCMOBJ src, PyScm * dst, int arg_num)
{

    PyScm obj;
    ___SCMOBJ ___temp;		/* some macros need this */

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
	    return ___FIX(___STOC_HEAP_OVERFLOW_ERR + arg_num);

    } else if (___FLONUMP(src)) {

	if ((obj = PyFloat_FromDouble(___FLONUM_VAL(src))) == NULL)
	    return ___FIX(___STOC_HEAP_OVERFLOW_ERR + arg_num);

    } else if (___CPXNUMP(src)) {

	/* TODO: handle complex */

    } else if (___STRINGP(src)) {

	if ((obj =
	     PyUnicode_FromKindAndData(___CS_SELECT
				       (PyUnicode_1BYTE_KIND,
					PyUnicode_2BYTE_KIND,
					PyUnicode_4BYTE_KIND), ___CAST(void *,
								       ___BODY_AS
								       (src,
									___tSUBTYPED)),
				       ___INT(___STRINGLENGTH(src)))) == NULL)
	    return ___FIX(___STOC_HEAP_OVERFLOW_ERR + arg_num);

    } else {

	return ___FIX(___STOC_TYPE_ERR + arg_num);

    }

    *dst = obj;

    return ___FIX(___NO_ERR);
}

___SCMOBJ PyScm_to_SCMOBJ(PyScm src, ___SCMOBJ * dst, int arg_num)
{

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
	    return ___FIX(___CTOS_TYPE_ERR + arg_num);	/* TODO: handle bignums */
	} else {
	    return ___EXT(___LONGLONG_to_SCMOBJ) (___PSTATE, val, dst, arg_num);
	}

    } else if (PyFloat_CheckExact(src)) {

	double val = PyFloat_AS_DOUBLE(src);
	return ___EXT(___DOUBLE_to_SCMOBJ) (___PSTATE, val, dst, arg_num);

    } else if (PyComplex_CheckExact(src)) {

	double real_val = PyComplex_RealAsDouble(src);
	double imag_val = PyComplex_ImagAsDouble(src);
	/* TODO: handle complex */
	return ___FIX(0);

    } else if (PyUnicode_CheckExact(src)) {

	Py_ssize_t len;

	if (PyUnicode_READY(src))	/* convert to canonical representation */
	    return ___FIX(___CTOS_HEAP_OVERFLOW_ERR + arg_num);

	len = PyUnicode_GET_LENGTH(src);

	obj = ___alloc_scmobj(___PSTATE, ___sSTRING, len << ___LCS);

	if (___FIXNUMP(obj))
	    return ___FIX(___CTOS_HEAP_OVERFLOW_ERR + arg_num);

	switch (PyUnicode_KIND(src)) {
	case PyUnicode_1BYTE_KIND:
	    {
		Py_UCS1 *data = PyUnicode_1BYTE_DATA(src);
		while (len-- > 0)
		    ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
		break;
	    }
	case PyUnicode_2BYTE_KIND:
	    {
		Py_UCS2 *data = PyUnicode_2BYTE_DATA(src);
		while (len-- > 0)
		    ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
		break;
	    }
	case PyUnicode_4BYTE_KIND:
	    {
		Py_UCS4 *data = PyUnicode_4BYTE_DATA(src);
		while (len-- > 0)
		    ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
		break;
	    }
	}

    } else {

	return ___FIX(___CTOS_TYPE_ERR + arg_num);

    }

    *dst = obj;

    return ___FIX(___NO_ERR);
}

#define ___BEGIN_CFUN_SCMOBJ_to_PYSCM(src,dst,i)                        \
  if ((___err = SCMOBJ_to_PyScm(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_PYSCM(src,dst,i) Py_DECREF(dst); }

#define ___BEGIN_CFUN_PYSCM_to_SCMOBJ(src,dst)                          \
  if ((___err = PyScm_to_SCMOBJ(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_PYSCM_to_SCMOBJ(src,dst) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_PYSCM_to_SCMOBJ(src,dst,i)                        \
  if ((___err = PyScm_to_SCMOBJ(src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_PYSCM_to_SCMOBJ(src,dst,i) ___EXT(___release_scmobj)(dst); }

#define ___BEGIN_SFUN_SCMOBJ_to_PYSCM(src,dst)                          \
  if ((___err = SCMOBJ_to_PyScm(src, &dst, 0)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_SCMOBJ_to_PYSCM(src,dst) Py_DECREF(dst); }

___SCMOBJ PyUnicode_string(PyObject * src)
{
    ___SCMOBJ obj;

    if (PyUnicode_CheckExact(src)) {

	Py_ssize_t len;

	if (PyUnicode_READY(src))	/* convert to canonical representation */
	    return ___FIX(___CTOS_HEAP_OVERFLOW_ERR);

	len = PyUnicode_GET_LENGTH(src);

	obj = ___alloc_scmobj(___PSTATE, ___sSTRING, len << ___LCS);

	if (___FIXNUMP(obj))
	    return ___FIX(___CTOS_HEAP_OVERFLOW_ERR);

	switch (PyUnicode_KIND(src)) {
	case PyUnicode_1BYTE_KIND:
	    {
		Py_UCS1 *data = PyUnicode_1BYTE_DATA(src);
		while (len-- > 0)
		    ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
		break;
	    }
	case PyUnicode_2BYTE_KIND:
	    {
		Py_UCS2 *data = PyUnicode_2BYTE_DATA(src);
		while (len-- > 0)
		    ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
		break;
	    }
	case PyUnicode_4BYTE_KIND:
	    {
		Py_UCS4 *data = PyUnicode_4BYTE_DATA(src);
		while (len-- > 0)
		    ___STRINGSET(obj, ___FIX(len), ___CHR(data[len]));
		break;
	    }
	}
	return obj;
    } else {
	return ___FIX(___CTOS_TYPE_ERR);
    }
}
