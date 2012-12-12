dist/build/ControlV/ControlV: Main.hs
	cabal install
	git pull
	git commit -a
	git push

clean:
	rm -f Main.hi Main.o

realclean: clean
	rm -rf state/ _state/
