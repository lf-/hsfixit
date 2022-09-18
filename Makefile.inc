%.cabal: package.yaml ../hpack-common/flags.yaml
	hpack
