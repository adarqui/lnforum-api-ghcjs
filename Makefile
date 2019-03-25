build:
	stack build --fast

clean:
	stack clean

tests:
	stack test --fast

sync:
	rsync -av ../lnforum-api/src/ ./src/

ghci:
	stack ghci lnforum-api-ghcjs
