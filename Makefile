doctest :
	doctest src

repl :
	ghci -Wall -isrc -itest test/Tests.hs

ghcid :
	ghcid -c 'ghci -Wall -isrc -itest test/Tests.hs'
