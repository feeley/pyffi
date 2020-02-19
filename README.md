# pyffi
Gambit Scheme library to interface to Python

## Build instructions

The library is compatible with Gambit's primitive module system and the R7RS libraries.

Let's say you develop in `/home/me/dev/gambit-modules`. Follow these instructions to get going (with hacks to fix issues with modules):

```
cd /home/me/.gambit_userlib
mkdir github.com
mkdir feeley
cd /home/me/dev/gambit-modules
git clone https://github.com/feeley/pyffi
cd pyffi
gsc -e '(##build-module "pyffi.sld" (quote C) (quote ((module-ref github.com/feeley/pyffi))))'
ln -s `pwd` @
ln -s /home/me/dev/gambit-modules/pyffi ~/.gambit_userlib/github.com/feeley/pyffi
gsi github.com/feeley/pyffi/demo
```

should print something like:

```
result=200
#<PyObject* #2 0x7f34e3918c30>
"Hello, world!"
```

run from whichever directory. The files will be where the module system expects them to...

The file pyffi.scm will have to be adjusted with the correct paths to the python libraries... it currently works on macOS after a `brew install python3`.
