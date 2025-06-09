{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) root;
      };
      pname = "nix-at-home";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            (hfinal: hprev: {
              ${pname} = hfinal.callCabal2nix pname (sourceFilter ./.) { };
            })
          ];
        };
      };
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let pkgs = pkgs'.extend overlay; in {
        formatter.${system} = pkgs.nixpkgs-fmt;
        legacyPackages.${system} = pkgs;
        packages.${system} = {
          default = pkgs.haskellPackages.${pname};
          nixStatic = (pkgs.nixStatic.overrideAttrs {
            outputs = ["out"];
          }).out;
        };
        devShells.${system}.default = pkgs.haskellPackages.shellFor {
          packages = ps: [ ps.${pname} ];
          buildInputs = with pkgs; with haskellPackages; [
            haskell-language-server
            cabal-install
            fourmolu
          ];
        };
      }
    );
}

