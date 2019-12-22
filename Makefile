.PHONY : test repl doctest ghcid

doctest :
	doctest src

repl :
	ghci -Wall -isrc -itest test/Tests.hs

ghcid :
	ghcid --command='ghci -Wall -isrc -itest test/Tests.hs'

test :
	cabal run zinza-tests

load-licenses :
	ghci -isrc -itest fixtures/licenses.hs

load-fancy :
	ghci -isrc -itest fixtures/fancy.hs

load-cabal :
	ghci -isrc -itest fixtures/cabal-install.hs

load-bools :
	ghci -isrc -itest fixtures/bools.hs
