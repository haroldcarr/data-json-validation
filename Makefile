# TODO: for some reason `stack test` does not run the test

test : FORCE
	stack test
	./.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/data-json-validation-test/data-json-validation-test

FORCE :
