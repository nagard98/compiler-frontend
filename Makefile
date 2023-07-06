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

clean:
	rm -f info
	rm -f *.o
	rm -f *.hi
	rm -f *.info
	rm -f TestGrammar
