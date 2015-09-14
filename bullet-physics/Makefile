BULLET3_VERSION=2.83.6
BULLET3_PATH=$(shell pwd)/deps/bullet3


get-deps: deps pybulletphysics deps/bullet3
	pip install PyOpenGL PyOpenGL_accelerate PIL

deps:
	mkdir -p deps

venv:
	virtualenv --prompt="(py/bullet-physics)" .venv

### Bullet3

deps/bullet3.tar.gz:
	curl https://codeload.github.com/bulletphysics/bullet3/tar.gz/${BULLET3_VERSION} > deps/bullet3.tar.gz

deps/bullet3-src: deps/bullet3.tar.gz
	mkdir -p deps/bullet3-src
	cd deps && tar -xf bullet3.tar.gz -C bullet3-src --strip-components 1

deps/bullet3: deps/bullet3-src
	#cd deps/bullet3/build3 && ./premake4_linux64 gmake
	#cd deps/bullet3/build3/gmake && make
	mkdir -p deps/bullet3-src/build-bullet3
	cd deps/bullet3-src/build-bullet3 && \
		cmake .. -G "Unix Makefiles" -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX=${BULLET3_PATH} && \
		make -j4 && \
		make install

### Py-Bullet

deps/pybulletphysics:
	git clone https://github.com/20tab/pybulletphysics.git deps/pybulletphysics

pybulletphysics: export CPATH=${BULLET3_PATH}/include/bullet
pybulletphysics: export PKG_CONFIG_PATH=${BULLET3_PATH}/lib/pkgconfig
pybulletphysics:
	cd deps/pybulletphysics && \
		python setup.py build --debug && \
		python setup.py install

