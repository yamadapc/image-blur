default: image-blur

image-blur: configure dependencies src/Main.hs
	cabal build
	ln -s dist/build/image-blur/image-blur image-blur

dependencies: image-blur.cabal
	cabal install --only-dep -j4

configure: image-blur.cabal
	cabal configure

out.png: clean image-blur
	./image-blur

clean:
	rm -f out.png
	rm -f image-blur
