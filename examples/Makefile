.PHONY: requests ruamel.yaml

UNAME := $(shell uname)

getvar=$(shell python3 -c "import sysconfig; print(sysconfig.get_config_var('$(1)'))")

PY_LIBPL=$(call getvar,LIBPL)
PY_LIBRARY = $(call getvar,LIBRARY) # macOS
PY_LDLIBRARY = $(call getvar,LDLIBRARY) # Linux

# TODO: Handle Windows.
ifeq ($(UNAME), Linux)
	PY_SHARED=$(PY_LIBPL)/$(PY_LDLIBRARY)
	PY_PRELOAD=LD_PRELOAD
endif
ifeq ($(UNAME), Darwin)
	PY_SHARED=$(PY_LIBPL)/$(PY_LIBRARY)
	PY_PRELOAD=DYLD_INSERT_LIBRARIES
endif

make-venv = virtualenv --python python3.7 venv && venv/bin/pip install -r requirements.txt

demo:
		$(PY_PRELOAD)=$(PY_SHARED) \
		gsi demo.scm

requests:
		cd requests && \
		if [ ! -d "venv" ]; then $(make-venv); fi; \
		$(PY_PRELOAD)=$(PY_SHARED) \
		VENV_PATH=venv \
		gsi requests.scm

ruamel.yaml:
		cd ruamel.yaml && \
		if [ ! -d "venv" ]; then $(make-venv); fi; \
		$(PY_PRELOAD)=$(PY_SHARED) \
		VENV_PATH=venv \
		gsi ruamel.scm
