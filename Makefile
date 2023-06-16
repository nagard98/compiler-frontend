compile:
	alex --ghc LexGrammar.x
	happy --ghc --coerce --array --info ParGrammar.y
	@# TODO: compile main file here when we will have one

compile-par-lex:
	alex --ghc LexGrammar.x
	happy --ghc --coerce --array --info ParGrammar.y

test-no-bnfc:
	ghc TestGrammar.hs
	@echo "******** TESTING GRAMMAR ON FILE testfile1"
	./TestGrammar < testfile1
	@echo "******** TESTING GRAMMAR ON FILE testfile2"
	./TestGrammar < testfile2

test:
	ghc TestGrammar.hs
	@echo "******** TESTING GRAMMAR ON FILE testfile1"
	./TestGrammar < testfile1
	@echo "******** TESTING GRAMMAR ON FILE testfile2"
	./TestGrammar < testfile2
	@echo "******** TESTING GRAMMAR ON FILE testfile3"
	./TestGrammar < testfile3

clean:
	rm -f info
	rm -f *.o
	rm -f *.hi
	rm -f *.info
	rm -f LexGrammar.hs
	rm -f ParGrammar.hs
	rm -f TestGrammar
