OS := $(shell uname)

default: image-blur

profile: dependencies
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-option=-auto-all --ghc-option=-rtsopts
	cabal build
	./dist/build/image-blur/image-blur +RTS -p -RTS input-medium.jpg output.png

image-blur: configure dependencies src/Main.hs
	cabal build

dependencies: image-blur.cabal
	cabal sandbox init
ifeq ($(OS),Darwin)
	cabal install friday --enable-library-profiling \
		--extra-include-dirs=$(shell brew --prefix)/include \
		--extra-lib-dirs=$(shell brew --prefix)/lib
endif
	cabal install --enable-library-profiling --only-dep -j4

configure: image-blur.cabal
	cabal configure

out.png: clean image-blur
	./dist/build/image-blur/image-blur input.png output.png

clean:
	rm -f out.png
	rm -f image-blur
