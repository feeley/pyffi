import sys
import sysconfig

# See CPython's Misc/python-config.in for details.

getvar = sysconfig.get_config_var
pyver = getvar('VERSION')

# TODO: Add more compilers and act upon the information.
pycc = getvar('CC')
if "gcc" in pycc:
    pycc = "gcc"
if "clang" in pycc:
    pycc = "clang"

ldflags = ['-lpython' + pyver + sys.abiflags]
ldflags += getvar('LIBS').split()
ldflags += getvar('SYSLIBS').split()
if not getvar('Py_ENABLE_SHARED'):
    ldflags.insert(0, '-L' + getvar('LIBPL'))
if not getvar('PYTHONFRAMEWORK'):
    ldflags.extend(getvar('LINKFORSHARED').split())


cflags = ['-I' + sysconfig.get_path('include'),
          '-I' + sysconfig.get_path('platinclude')]
cflags.extend(getvar('CFLAGS').split())

# Set @rpath on macOS & clang
PYTHONFRAMEWORKPREFIX = getvar('PYTHONFRAMEWORKPREFIX')
if PYTHONFRAMEWORKPREFIX != '' and pycc == 'clang':
    cflags.append(f"-rpath {PYTHONFRAMEWORKPREFIX}")

# The output is parsed by gsc, one line at a time:
print(pyver)
print(pycc)
print(' '.join(ldflags))
print(' '.join(cflags))
