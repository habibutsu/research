
OCB_FLAGS = -use-ocamlfind -no-links -I src -I lib
OCB = 		ocamlbuild $(OCB_FLAGS)

all: build

init:
	opam init -a
	opam pin add web-service . -n

get-deps:
	opam install --deps-only web-service

build:
	#eval `opam config env`
	 $(OCB) -pkg postgresql -tags thread db.native
	 $(OCB) -pkg lwt -pkg cohttp -pkg cohttp.lwt http.native
