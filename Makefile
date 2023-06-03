compile:
	bnfc grammar.cf
	alex --ghc LexGrammar.x
	happy --ghc --coerce --array --info ParGrammar.y
	@# TODO: compile main file here when we will have one

compile-par-lex:
	alex --ghc LexGrammar.x
	happy --ghc --coerce --array --info ParGrammar.y

test-no-bnfc: compile-par-lex
	ghc TestGrammar.hs
	@echo "******** TESTING GRAMMAR ON FILE testfile1"
	./TestGrammar < testfile1
	@echo "******** TESTING GRAMMAR ON FILE testfile2"
	./TestGrammar < testfile2

test: compile
	ghc TestGrammar.hs
	@echo "******** TESTING GRAMMAR ON FILE testfile1"
	./TestGrammar < testfile1
	@echo "******** TESTING GRAMMAR ON FILE testfile2"
	./TestGrammar < testfile2

clean:
	rm -f info
	rm -f *.o
	rm -f *.hi
	rm -f *.info
	rm -f DocGrammar.txt
	rm -f AbsGrammar.hs
	rm -f ErrM.hs
	rm -f LexGrammar.*
	rm -f ParGrammar.*
	rm -f PrintGrammar.hs
	rm -f SkelGrammar.hs
	rm -f TestGrammar*