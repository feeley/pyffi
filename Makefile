build:
		gsc -e '(##build-module "pyffi.sld" (quote C) (quote ((module-ref github.com/feeley/pyffi))))'
		python3 -m venv ${HOME}/.gambit_venv

test:
		gsi pyffi-test.scm

ln:
	  ln -s `pwd` "@"
