build: TEMPLATE.cabal
	cabal build

TEMPLATE.cabal: package.yaml
	hpack

test: TEMPLATE.cabal
	cabal test
