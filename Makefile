compile:
	# alex --ghc LexGrammar.x
	# happy --ghc --coerce --array --info ParGrammar.y
	ghc TestGrammar.hs
	@# TODO: compile main file here when we will have one


test:
	ghc TestGrammar.hs
	@echo "******** TESTING GRAMMAR ON FILE testfile1"
	./TestGrammar < testfile1
	@echo "\n\n******** TESTING GRAMMAR ON FILE testfile2"
	./TestGrammar < testfile2
	@echo "\n\n******** TESTING GRAMMAR ON FILE testfile3"
	./TestGrammar < testfile3
	@echo "\n\n******** TESTING GRAMMAR ON FILE testfile4"
	./TestGrammar < testfile4
	@echo "\n\n******** TESTING GRAMMAR ON FILE testfile5"
	./TestGrammar < testfile5


TESTSDIR = ./tests

demo:
ifeq ($(strip $(file)),)
	$(foreach testFile, $(wildcard $(TESTSDIR)/*), make testSingle file=$(notdir $(testFile));)
else
	make testSingle $(file)
endif

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
