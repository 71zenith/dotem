{
  description = "crossbell cathedral";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hosts.url = "github:StevenBlack/hosts";
    hosts.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    stylix.inputs.nixpkgs.follows = "nixpkgs";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    zen-browser.inputs.nixpkgs.follows = "nixpkgs";
    flake-programs-sqlite.url = "github:wamserma/flake-programs-sqlite";
    flake-programs-sqlite.inputs.nixpkgs.follows = "nixpkgs";
    nix-gaming.url = "github:fufexan/nix-gaming";
    nix-gaming.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = {
    nixpkgs,
    ...
  } @ inputs: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    nixosConfigurations."izanagi" = nixpkgs.lib.nixosSystem {
      specialArgs = {inherit inputs;};
      modules = with inputs; [
        stylix.nixosModules.stylix
        sops-nix.nixosModules.sops
        flake-programs-sqlite.nixosModules.programs-sqlite
        hosts.nixosModule
        (import ./config.nix)
      ];
    };
  };
}
