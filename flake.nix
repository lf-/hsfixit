{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs.url = "/home/jade/dev/nixpkgs";
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
                  prev.lib.composeExtensions (oldArgs.overrides or (_: _: {}))
                    (overlay prev);
              }
            );
          };
        };
      };

      out = system:
        let
          pkgs = import /home/jade/dev/nixpkgs {
            inherit system;
            overlays = [ self.overlays.hashes (import nix/hls.nix { inherit hls; }) self.overlays.default ];
            # config.allowBroken = true;
          };

        in
          {
            packages = rec {
              default = sample;
              sample = pkgs.haskell.packages.${ghcVer}.sample;
            };

            checks = {
              inherit (self.packages.${system}) sample;
            };

            # for debugging
            inherit pkgs;

            devShells.default =
              let
                haskellPackages = pkgs.haskell.packages.${ghcVer};
                toolsPackages = pkgs.haskell.packages.ghc924;
              in
                haskellPackages.shellFor {
                  packages = p: [ self.packages.${system}.sample ];
                  withHoogle = false;
                  buildInputs = (
                    with haskellPackages; [
                      pkgs.haskell-language-server
                    ]
                  ) ++ (
                    with toolsPackages; [
                      ghcid
                      cabal-install
                    ]
                  ) ++ (
                    with pkgs; [
                      sqlite
                    ]
                  );
                  # Change the prompt to show that you are in a devShell
                  # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
                };
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
              in
                {
                  sample = hfinal.callCabal2nix "sample" ./. {};

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
