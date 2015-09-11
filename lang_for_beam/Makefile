
get-deps:
	git clone https://github.com/habibutsu/pybison.git pybison

build-pybison:
	cd pybison && python ./setup.py build
	cd pybison && python ./setup.py install

venv:
	virtualenv --system-site-packages --prompt="(venv)" .venv