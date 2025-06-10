{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nix.url = "github:nixos/nix/2.28.3";
    nixpkgs.follows = "nix/nixpkgs";
  };

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
          nixStatic =
            let nix = inputs.nix.packages.${system}.nix-cli-static; in 
            pkgs.runCommand nix.name {
              nativeBuildInputs = with pkgs; [nukeReferences upx];
            } ''
              mkdir -p $out/bin
              cp ${lib.getExe nix} $out/bin
              nuke-refs $out/bin/nix
              upx $out/bin/nix
            '';
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

