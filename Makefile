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
	ghci -itest fixtures/licenses.hs

load-fancy :
	ghci -itest fixtures/fancy.hs

load-cabal :
	ghci -itest fixtures/cabal-install.hs
