hpack-%: %/package.yaml
	hpack $<
.PHONY: hpack-%

hpack: hpack-hsfixit-types hpack-hsfixit-plugin hpack-hsfixit-test
.PHONY: hpack
