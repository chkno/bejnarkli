{
  inputs = {
    nixpkgs.url = "git+https://github.com/NixOS/nixpkgs?branch=nixos-20.03";
  };
  outputs = { self, nixpkgs, }:
    let
      inherit (nixpkgs.lib) genAttrs;

      supported-systems = [ "x86_64-linux" ]; # TODO: Expand
      bejnarkli = system:
        import ./. { pkgs = nixpkgs.legacyPackages."${system}"; };
      packages = system: { bejnarkli = bejnarkli system; };

    in {
      defaultPackage = genAttrs supported-systems bejnarkli;
      packages = genAttrs supported-systems packages;
    };
}
