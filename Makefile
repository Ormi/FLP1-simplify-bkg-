all:
	ghc --make -o simplify-bkg SimplifyBKGMain.hs
clean:
	rm *.hi *.o simplify-bkg

