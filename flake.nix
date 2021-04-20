{
  inputs = {
    nixpkgs.url = "git+https://github.com/NixOS/nixpkgs?branch=nixos-20.09";
  };
  outputs = { self, nixpkgs, }:
    let
      inherit (nixpkgs.lib) genAttrs;
      supported-systems = [ "aarch64-linux" "i686-linux" "x86_64-linux" ];
    in {
      overlay = final: prev: { bejnarkli = import ./. { pkgs = final; }; };
      overlays = { bejnarkly = self.overlay; };
      defaultPackage = genAttrs supported-systems (system:
        (nixpkgs.legacyPackages."${system}".appendOverlays
          [ self.overlay ]).bejnarkli);
      packages = genAttrs supported-systems
        (system: { bejnarkli = self.defaultPackage."${system}"; });
      nixosModules = { bejnarkli = import ./nixos-module.nix; };
      checks = genAttrs supported-systems (system: {
        bejnarkli-module = import ./nixos-test.nix { inherit nixpkgs system; };
      });
    };
}
