{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = haskellPackages.callCabal2nix "problem-example" ./clash {};
in

project.env.overrideAttrs (oldEnv: {
  buildInputs = oldEnv.buildInputs ++ [
    pkgs.cabal-install
    pkgs.python311Packages.cocotb
    pkgs.python311Packages.scapy
    pkgs.python311Packages.cocotb-bus
    pkgs.cocotb-test
    pkgs.cocotb_lib
    pkgs.python311Packages.pytest
    pkgs.verilog
    haskellPackages.cabal-install
  ];
})
