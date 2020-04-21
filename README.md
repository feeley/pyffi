# pyffi
Gambit Scheme library to interface to Python

**This library depends on new Gambit features not yet made into a release.
Please build using Gambit@HEAD.**

**Programmers are encouraged to read the Python C API specification.**

## Build instructions

The library is compatible with Gambit's primitive module system and the R7RS
libraries.

Assuming you
- have compiled Gambit@HEAD,
- are developing Gambit modules in `/home/me/dev/gambit-modules`,
- have Python 3.7 _with development sources_ available,

these instructions should allow you to build `pyffi`:

```
cd /home/me/.gambit_userlib
mkdir github.com
mkdir github.com/feeley
cd /home/me/dev/gambit-modules
git clone https://github.com/feeley/pyffi
cd pyffi
make ln
make
ln -s /home/me/dev/gambit-modules/pyffi ~/.gambit_userlib/github.com/feeley/pyffi
```

## Examples

After building the library, you should be able to run the examples in the
`examples` directory. Some examples require the use of `virtualenv`. Make sure
you have it installed. A `Makefile` is provided to facilitate demonstrations.

`Pyffi` currently only offers a relatively low-level API in sync with the Python
C API. A higher level API is WIP.

### Vanilla python

This example works out of the box requiring only built-in python modules. From
the `pyffi` directory:

```
cd examples
make demo
```

should output

```
gsi demo.scm
result=200
#<PyObject*/str #2 'hello, world!'>
"hello, world!"
---------------------------------------------
#<PyObject*/tuple #3 (None, False, True, [[], (), 42, 'hello'])>
---------------------------------------------
#(#<PyObject*/None #4 None>
  #<PyObject*/bool #5 False>
  #<PyObject*/bool #6 True>
  #<PyObject*/list #7 [[], (), 42, 'hello']>)
---------------------------------------------
(#!void #f #t #(#() () 42 "hello"))
---------------------------------------------
(#!void #f #t #(#() () 42 "hello"))
---------------------------------------------
```

Notice the rich information displayed in the foreign objects. This information
can be displayed or not depending on your needs. Modify
[examples/demo.scm](examples/demo.scm) to `register-foreign-write-handlers` or
not.

### Python + virtualenv

A common way to manage python versions and dependencies is to use a virtual
environment. Here we assume that you have installed `virtualenv` (not `python3
-m venv`) and are able to use it.

A lot of third-party libraries you will install in virtualenvs will require
linking with `libpython`. The makefile sets `LD_PRELOAD` to the default
python3.7 shared library path on Debian 10. You will need to provide your own
`LD_PRELOAD` if that default is not correct for your system.

The makefile also sets the `VENV_PATH` environment variable. This is required to
tell the Python C API where to look for modules. In our case, we want modules to
be searched for in the virtualenv.

#### requests

As a first example using the low-level API, let's use the popular `requests`
package from PyPI. From the `examples` directory:

```
make requests
```

That will set up the correct virtual environment and execute the code in the
proper context:

```
#<PyObject*/str #2 "{'origin': 'x.x.x.x'}">
"{'origin': 'x.x.x.x'}"
```

Again, notice the rich foreign-object information.

### ruamel.yaml

This example showcases another way to evaluate expressions using the low-level
API. Here, in particular, we are concerned with evaluating a block of Python
code without returning a value, using the `Py_file_input` [start
symbol](https://docs.python.org/3/c-api/veryhigh.html). The code showcases file
IO directly from Python. From the `examples` directory:

```
make ruamel.yaml
```

That will set up the correct virtual environment and execute the code in the
proper context:

```
out.yaml:

alpha: &a
  fun: true
  sad: false
  happy: true

entries:
  - <<: *a
    purpose: cure cancer
  - <<: *a
    purpose: cure latency
  - <<: *a
    purpose: cure all the things
```
