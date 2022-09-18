hpack:
	$(MAKE) -C hsfixit-types hsfixit-types.cabal
	$(MAKE) -C hsfixit-plugin hsfixit-plugin.cabal
.PHONY: hpack
