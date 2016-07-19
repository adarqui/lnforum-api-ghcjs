build:
	stack build --fast

clean:
	stack clean

tests:
	stack test --fast

sync:
	rsync -av ../ln-api/src/ ./src/
