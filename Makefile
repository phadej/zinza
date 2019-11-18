doctest :
	doctest src

repl :
	ghci -Wall -isrc -itest test/Tests.hs

ghcid :
	ghcid --command='ghci -Wall -isrc -itest test/Tests.hs'
