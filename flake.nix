{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nix.url = "github:nixos/nix/2.29.0";
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    nix-bundle = {
      # https://github.com/nix-community/nix-bundle/pull/120
      url = "github:TroyNeubauer/nix-bundle/fix-paths-from-graph";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
      let
        pkgs = pkgs'.extend overlay;
        nah = pkgs.pkgsStatic.haskellPackages.${pname};
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
        unbundled = pkgs.runCommand nah.name { inherit (nah) pname version meta; } ''
          mkdir -p $out/bin $out/share
          install -m755 ${lib.getExe' nah "nah"} $out/bin
          install -m755 ${nixStatic}/bin/nix $out/bin/nix-static
          cp -r --no-preserve=mode ${./static}/* $out/bin
          mv $out/bin/nix.conf $out/share/nix.conf
          ${lib.getExe pkgs.upx} $out/bin/nah
        '';
        bundled = pkgs.runCommand nah.name { inherit (nah) pname version meta; } ''
          mkdir -p $out/bin
          install -m755 ${inputs.nix-bundle.bundlers.${system}.default unbundled} $out/bin/nah
        '';
      in {
        formatter.${system} = pkgs.nixpkgs-fmt;
        legacyPackages.${system} = pkgs;
        packages.${system} = {
          default = bundled;
          inherit bundled unbundled;
          nix-static = inputs.nix.packages.${system}.nix-cli-static;
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

