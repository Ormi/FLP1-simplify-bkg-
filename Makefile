all:
	ghc --make main.hs

clean:
	rm -f main{,.o,.hi} *.zip 2>/dev/null

pack:
	zip -r flp-fun-xormos00.zip main.hs README