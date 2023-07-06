compile:
	# alex --ghc LexGrammar.x
	# happy --ghc --coerce --array --info ParGrammar.y
	ghc TestGrammar.hs
	@# TODO: compile main file here when we will have one

# TODO: rename this to demo before submission
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

# make test on a single file with command:
# make testSinge file=filename
testSingle:
	ghc TestGrammar.hs
	@echo "******** TESTING GRAMMAR ON FILE" $(file)
	./TestGrammar < $(file)

clean:
	rm -f info
	rm -f *.o
	rm -f *.hi
	rm -f *.info
	rm -f TestGrammar
