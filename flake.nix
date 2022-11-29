{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    nixpkgs.url = "github:lf-/nixpkgs/ghc-942";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server?submodules=1";

    all-cabal-hashes = {
      type = "file";
      flake = false;
      url = "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/tarball/b60ac6061f28cfc1b0f3ce353df62576f20807cf";
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = inputs@{ self, nixpkgs, flake-utils, all-cabal-hashes, hls }:
    let
      ghcVer = "ghc942";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (
              oldArgs: {
                overrides =
                  prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                    (overlay prev);
              }
            );
          };
        };
      };

      out = system:
        let
          pkgs = import /home/jade/dev/nixpkgs {
          # pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.hashes (import nix/hls.nix { inherit hls; }) self.overlays.default ];
            # config.allowBroken = true;
          };
          mkShell = withFancyTools:
            let
              haskellPackages = pkgs.haskell.packages.${ghcVer};
              toolsPackages = pkgs.haskell.packages.ghc924;
            in
            haskellPackages.shellFor {
              packages = p: with self.packages.${system}; [ hsfixit-types hsfixit-plugin ];
              withHoogle = withFancyTools;
              buildInputs =
                pkgs.lib.optionals withFancyTools
                  (
                    with haskellPackages; [
                      pkgs.haskell-language-server
                      implicit-hie
                    ]
                  ) ++ (
                  with toolsPackages; [
                    ghcid
                    cabal-install
                    cabal2nix
                    fourmolu
                  ]
                ) ++ (
                  with pkgs; [
                    sqlite
                  ]
                );
              # Change the prompt to show that you are in a devShell
              # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
            };

        in
        {
          packages = rec {
            inherit (pkgs.haskell.packages.${ghcVer}) hsfixit-types hsfixit-plugin;
          };

          checks = {
            # inherit (self.packages.${system}) sample;
          };

          # for debugging
          inherit pkgs;

          devShells.default = mkShell true;
          devShells.quick = mkShell false;
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        hashes = self: super: {
          inherit all-cabal-hashes;
        };
        default = makeHaskellOverlay (
          prev: hfinal: hprev:
            let
              hlib = prev.haskell.lib;
              traceId = v: builtins.trace v v;
              pathFilter = path: type:
                prev.lib.hasSuffix ".cabal" path
                || baseNameOf path == "package.yaml"
                || builtins.any (pat: builtins.match pat path != null)
                  [
                    ".*hpack-common(/.*\.yaml)?"
                    ".*hsfixit-types(/package.yaml)?"
                    ".*hsfixit-plugin(/package.yaml)?"
                  ];
              subdir = name: hfinal.callCabal2nixWithOptions'
                {
                  inherit name;
                  src = ./.;
                  extraCabal2nixOptions = "--subpath=${name}";
                  inherit pathFilter;
                }
                { };
            in
            {
              hsfixit-types = subdir "hsfixit-types";
              hsfixit-plugin = subdir "hsfixit-plugin";
              fourmolu = hfinal.fourmolu_0_8_2_0;
              # here's how to do hacks to the package set
              # don't run the test suite
              # fast-tags = hlib.dontCheck hprev.fast-tags;
              #
              # don't check version bounds
              # friendly = hlib.doJailbreak hprev.friendly;
            }
        );
      };
    };
}
