build:
		gsc -e '(##build-module "pyffi.sld" (quote C) (quote ((module-ref github.com/feeley/pyffi))))'

test:
		gsi pyffi-test.scm

ln:
	  ln -s `pwd` "@"
