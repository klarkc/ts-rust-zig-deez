{
  inputs = {
    purs-nix.url = "github:purs-nix/purs-nix";
    nixpkgs.follows = "purs-nix/nixpkgs";
    utils.url = "github:ursi/flake-utils";
    # optional
    ps-tools.follows = "purs-nix/ps-tools";
  };

  outputs = { self, utils, ... }@inputs:
    let
      # TODO add missing arm to match standard systems
      #  right now purs-nix is only compatible with x86_64-linux
      systems = [ "x86_64-linux" ];
    in
    utils.apply-systems
      { inherit inputs systems; }
      ({ system, pkgs, ps-tools, ... }:
        let
          purs-nix = inputs.purs-nix { inherit system; };
          project = purs-nix.purs
            {
              # Project dir (src, test)
              dir = ./.;
              # Dependencies
              dependencies =
                with purs-nix.ps-pkgs;
                [
                  prelude
                  effect
                  aff
                  console
                  spec
                ];
              # FFI dependencies
              # foreign.Main.node_modules = [];
            };
        in
        {
          packages.default = project.output { };

          checks.default = project.test.check { };

          devShells.default =
            pkgs.mkShell
              {
                packages =
                  with pkgs;
                  [
                    (project.command { })
                    # optional devShell tools
                    ps-tools.for-0_15.purescript-language-server
                    ps-tools.purty
                  ];
              };
        });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig =
    {
      # This sets the flake to use nix cache.
      # Nix should ask for permission before using it,
      # but remove it here if you do not want it to.
      extra-substituters = [
        "https://klarkc.cachix.org?priority=99"
      ];
      extra-trusted-public-keys = [
        "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      ];
    };
}
