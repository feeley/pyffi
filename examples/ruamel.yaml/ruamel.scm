#!/usr/bin/env gsi-script

(import (github.com/feeley pyffi))
(import (gambit))

;; enable rich python foreign-object representation
(register-foreign-write-handlers)

(define VENV_PATH
  (getenv "VENV_PATH" #f))

(if (not VENV_PATH)
  (begin
    (display "VENV_PATH not set") (newline)
    (exit #f)))

;; Assumes a proper virtualenv, created with virtualenv, not python -m venv
(define (venv-path->PYTHONPATH p)
  (string-append
   (string-append p "/bin/python")
   ":" (string-append p "/lib/python37.zip")
   ":" (string-append p "/lib/python3.7")
   ":" (string-append p "/lib/python3.7/lib-dynload")
   ":" (string-append p "/lib/python3.7/site-packages")))

(define PYTHONPATH
  (venv-path->PYTHONPATH VENV_PATH))

(Py_SetPath PYTHONPATH)
(Py_SetPythonHome VENV_PATH)
(Py_Initialize)

(define __main__ (PyImport_AddModule "__main__"))
(define globals (PyModule_GetDict __main__))
(define locals (PyDict_New))

;; vectors map to lists, strings map to strings:
(define from-list (object->PyObject* #("YAML")))
;; Use PyImport_ImportModuleEx for stronger control.
;; Equivalent to 'from ruamel.yaml import YAML':
(define py/ruamel.yaml (PyImport_ImportModuleEx
                        "ruamel.yaml"
                        globals
                        locals
                        from-list))
(define py/ruamel.yaml_dict (PyModule_GetDict py/ruamel.yaml))

;; Store 'YAML()' from the py/ryamel.yaml_dict context
;; into the variable 'yaml' inside the globals context
(PyRun_String "yaml = YAML()" Py_file_input py/ruamel.yaml_dict globals)

;; You can wrap the operation in a python try/except and print to the console
;; or you can let Gambit handle the exception.
;; Try changing the name of 'test.yaml' to one that does not exist.
(PyRun_String "
with open('./out.yaml', 'w') as outs:
    with open('./test.yaml', 'r') as ins:
        doc = yaml.load(ins)
        yaml.indent(mapping=2, sequence=4, offset=2)
        yaml.dump(doc, outs)
"
              Py_file_input ;; execute, don't return!
              globals
              locals)

(Py_Finalize)

(define out
  (call-with-input-file "out.yaml"
    (lambda (port) (read-line port #f))))

(display "out.yaml:\n\n")
(display out)
