{
  description = "Example Haskell flake showing overrides and adding stuff to the dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = inputs@{ self, nixpkgs, flake-utils }:
    let
      ghcVer = "ghc943";
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
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            # config.allowBroken = true;
          };
          mkShell = withFancyTools:
            let
              haskellPackages = pkgs.haskell.packages.${ghcVer};
              toolsPackages = pkgs.haskellPackages;
            in
            haskellPackages.shellFor {
              packages = p: with self.packages.${system}; [ hsfixit-types hsfixit-plugin ];
              withHoogle = withFancyTools;
              buildInputs =
                pkgs.lib.optionals withFancyTools
                  (
                    with haskellPackages; [
                      haskell-language-server
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
          # inherit all-cabal-hashes;
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
              subdir = name: hfinal.callCabal2nixWithOptions
                name
                (builtins.path { path = ./.; filter = pathFilter; })
                "--subpath=${name}"
                { };
            in
            {
              hsfixit-types = subdir "hsfixit-types";
              hsfixit-plugin = subdir "hsfixit-plugin";
              fourmolu = hfinal.fourmolu_0_8_2_0;
              # Due to a ghc bug in 9.4.3 and 9.2.5
              ListLike = hlib.dontCheck hprev.ListLike;
            }
        );
      };
    };
}
