import sys
import sysconfig
import platform
import pathlib

class MSVC:
    def __init__(self):
        self.name = 'cl'
        self.cflags = []
        self.ldflags = ['-link']

    def add_library_path(self, path):
        self.ldflags.append('-LIBPATH:"{}"'.format(path))

    def add_library(self, lib):
        self.ldflags.append(lib+'.lib')

    def add_libraries(self, libs):
        for lib in libs:
            self.add_library(lib)

    def add_include_path(self, path):
        self.cflags.append('-I"{}"'.format(path))

class GnuLikeCompiler:
    def __init__(self, name):
        self.name = name
        self.cflags = []
        self.ldflags = []

    def add_library_path(self, path):
        self.ldflags.append('-L "{}"'.format(path))

    def add_library(self, lib):
        self.ldflags.append('-l{}'.format(lib))

    def add_libraries(self, libs):
        for lib in libs:
            self.add_library(lib)

    def add_include_path(self, path):
        self.cflags.append('-I"{}"'.format(path))


# See CPython's Misc/python-config.in for details.
getvar = sysconfig.get_config_var
def extend_array_with_config_var(array, name):
    value = getvar(name)
    if value is not None:
        array.extend(value.split())

def find_compiler():
    pycc_cmd = getvar('CC')
    if pycc_cmd is None:
        pycompiler = platform.python_compiler().upper()
        if 'MSC' in pycompiler:
            compiler = MSVC()
        elif 'CLANG' in pycompiler:
            compiler = GnuLikeCompiler('clang')
        elif 'GCC' in pycompiler:
            compiler = GnuLikeCompiler('gcc')
        else:
            RuntimeError('Unknown compiler')
    else:
        pycc_cmd = pycc_cmd.split()
        if 'gcc' in pycc_cmd:
            pycc = 'gcc'
        elif 'clang' in pycc_cmd:
            pycc = 'clang'
        else:
            raise RuntimeError('Unknown compiler')
        compiler = GnuLikeCompiler(pycc)
    return compiler


pyver = getvar('VERSION')
current_system = platform.system()
if current_system == 'Windows':
    compiler = find_compiler()

    libdir = getvar('LIBDIR')
    if libdir is None:
        # Assume libpath is %PYTHONPREFIX%\\libs on Windows
        prefix = pathlib.Path(sysconfig.get_config_var('prefix'))
        libs = prefix / 'libs'
        if not libs.exists():
            raise RuntimeError('Unable to find python C libraries')
        libdir = str(libs)
elif current_system == 'Linux' or current_system == 'Darwin':
    compiler = find_compiler()
    libdir = sysconfig.get_config_var('LIBDIR')
else:
    raise RuntimeError('Unsupported system')

compiler.add_library_path(libdir)

# Getting the abiflags
try:
    abiflags = sys.abiflags
except:
    abiflags = getvar('abiflags') or ''

compiler.add_library(f'python{pyver}{abiflags}')
extend_array_with_config_var(compiler.ldflags, 'LIBS')
extend_array_with_config_var(compiler.ldflags, 'SYSLIBS')

if not getvar('Py_ENABLE_SHARED'):
    libpath = getvar('LIBPL')
    if libpath is not None:
        compiler.add_library_path(libpath)

if not getvar('PYTHONFRAMEWORK'):
    extend_array_with_config_var(compiler.ldflags, 'LINKFORSHARED')

# NOTE: flags may be dupplicate
compiler.add_include_path(sysconfig.get_path('include'))
compiler.add_include_path(sysconfig.get_path('platinclude'))
CONFINCLUDEDIR=getvar('CONFINCLUDEDIR')
if CONFINCLUDEDIR is not None:
    pyld = f'python{pyver}{abiflags}'
    compiler.add_include_path(CONFINCLUDEDIR + '/' + pyld)
extend_array_with_config_var(compiler.cflags, 'CFLAGS')

# Set @rpath on macOS & clang
PYTHONFRAMEWORKPREFIX = getvar('PYTHONFRAMEWORKPREFIX')
if PYTHONFRAMEWORKPREFIX != '' and compiler.name == 'clang':
    compiler.cflags.append(f"-rpath {PYTHONFRAMEWORKPREFIX}")

# The output is parsed by gsc, one line at a time:
print(pyver)
print(compiler.name)
print(' '.join(compiler.ldflags))
print(' '.join(compiler.cflags))
