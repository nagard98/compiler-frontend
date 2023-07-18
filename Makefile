compile:
	# alex --ghc LexGrammar.x
	# happy --ghc --coerce --array --info ParGrammar.y
	ghc TestGrammar.hs
	@# TODO: compile main file here when we will have one

TESTSDIR = ./tests

demo:
ifeq ($(strip $(file)),) # no file specified, ran on all files in tests directory
	$(foreach testFile, $(wildcard $(TESTSDIR)/*), $(MAKE) --no-print-directory testSingle file=$(notdir $(testFile));)
else # run only on specified file
	$(MAKE) --no-print-directory testSingle $(file)
endif

# runs TestsGrammar on the file speficied by the file argument
testSingle:
	ghc TestGrammar.hs
	@echo "\n\n********************** TESTING FILE" $(file) "**********************"
	@echo "******************************************************************"
	./TestGrammar < $(TESTSDIR)/$(file)

clean:
	rm -f info
	rm -f *.o
	rm -f *.hi
	rm -f *.info
	rm -f TestGrammar
