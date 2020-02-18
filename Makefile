build:
		gscd -e '(##build-module "pyffi.sld" (quote C) (quote ((module-ref github.com/feeley/pyffi))))'

ln:
	  ln -s `pwd` "@"
