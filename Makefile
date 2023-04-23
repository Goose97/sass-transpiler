repl:
	stack repl --package pretty-simple --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint"

TEST_FOLDER=test
prepare-test:
	find test -name "*.css" -delete
	ls $(TEST_FOLDER) | sed "s/.scss//g" | xargs -I {} sass $(TEST_FOLDER)/{}.scss $(TEST_FOLDER)/{}.css --sourcemap=none

