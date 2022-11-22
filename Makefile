build: exceptiot.cabal
	cabal build

exceptiot.cabal: package.yaml
	hpack

test: exceptiot.cabal
	cabal test
