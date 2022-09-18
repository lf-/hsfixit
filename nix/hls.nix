# this entire thing is a workaround for hls not providing their
# configuration-ghc-9.4.nix in their overlay for ... some reason
{ hls }:
self: super:
let
  hlib = self.haskell.lib;
  hlsHpkgs = ((hls.overlays.default self super).hlsHpkgs "ghc942").extend (
    hfinal: hprev: {
      lucid = hlib.doJailbreak hprev.lucid;
      invariant = hlib.doJailbreak hprev.invariant;
      implicit-hie-cradle = hlib.doJailbreak hprev.implicit-hie-cradle;
      cereal = hprev.callHackage "cereal" "0.5.8.3" {};
      base-compat = hprev.callHackage "base-compat" "0.12.2" {};
      base-compat-batteries = hprev.callHackage "base-compat-batteries" "0.12.2" {};
      hashable = hprev.callHackage "hashable" "1.4.1.0" {};
      primitive = hprev.callHackage "primitive" "0.7.4.0" {};
      ghc-check = hprev.callHackage "ghc-check" "0.5.0.8" {};
      lens = hprev.callHackage "lens" "5.2" {};
      integer-logarithms = hprev.callHackage "integer-logarithms" "1.0.3.1" {};
      # alarming; but these are jacked on 9.4
      hiedb = hlib.dontCheck (hprev.callHackage "hiedb" "0.4.2.0" {});
      hie-bios = hlib.dontCheck (hprev.callHackage "hie-bios" "0.11.0" {});
      lsp = hprev.callHackage "lsp" "1.6.0.0" {};
      lsp-types = hprev.callHackage "lsp-types" "1.6.0.0" {};
    }
  );
  mkExe = hlsHpkgs:
    with self.haskell.lib;
    (
      enableSharedExecutables (
        overrideCabal hlsHpkgs.haskell-language-server
          (
            _: {
              postInstall = ''
                remove-references-to -t ${hlsHpkgs.shake.data} $out/bin/haskell-language-server
                remove-references-to -t ${hlsHpkgs.js-jquery.data} $out/bin/haskell-language-server
                remove-references-to -t ${hlsHpkgs.js-dgtable.data} $out/bin/haskell-language-server
                remove-references-to -t ${hlsHpkgs.js-flot.data} $out/bin/haskell-language-server
              '';
            }
          )
      )
    ).overrideAttrs (
      old: {
        pname = old.pname + "-ghc${hlsHpkgs.ghc.version}";
      }
    );

  haskell-language-server = mkExe hlsHpkgs;
in
{
  inherit haskell-language-server;
}
