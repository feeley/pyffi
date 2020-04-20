.PHONY: requests

ifndef LD_PRELOAD
override LD_PRELOAD = /usr/lib/x86_64-linux-gnu/libpython3.7m.so
endif

make-venv = virtualenv --python python3.7 venv && venv/bin/pip install -r requirements.txt

demo:
		gsi demo.scm

requests:
		cd requests && \
		if [ ! -d "venv" ]; then $(make-venv); fi; \
		LD_PRELOAD=$(LD_PRELOAD) \
		VENV_PATH=venv \
		gsi requests.scm
