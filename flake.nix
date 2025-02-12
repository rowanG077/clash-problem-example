
{
  description = "clash-problem-example";
  inputs = {
    # We use an older version of nixpkgs to get ghc 9.6.2; this is the most recent version of
    # ghc that's compatible with clash
    ghc-nixpkgs.url = "github:NixOS/nixpkgs/5148520bfab61f99fd25fb9ff7bfbb50dad3c9db";
    # Then an up-to-date nixpkgs, for general stuff
    nixpkgs.url = "github:NixOS/nixpkgs";
    # This is the clash compiler; it is a forked version of the original, because I've added
    # the clash-protocols module to it
    clash-compiler = {
      url = "github:harris-chris/clash-compiler?ref=ff10f7a8493842a637bf5a74e08a4cd8031dff09";
    };
  };

  outputs = {
    self
    , ghc-nixpkgs
    , nixpkgs
    , clash-compiler
  }:
    let
      # Versions; feel free to change pythonVersion but ghcVersion should be locked to 962, 
      # because that's what clash is compatible with
      system = "x86_64-linux";
      pythonVersion = "311";
      ghcVersion = "962";

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ 
          ghcOverlay
          clash-compiler.overlays.default 
          (self: super: 
            with super."python${pythonVersion}Packages"; 
            { 
              "cocotb_lib" = callPackage ./nix/cocotb_lib.nix {}; 
              "cocotb-test" = callPackage ./nix/cocotb_test.nix {}; 
            } 
          ) 
        ];
      };

      # This is the overlay which adds ghc 9.6.2, from the older nixpkgs, to the newer nixpkgs
      ghcOverlay = let
        ghcPkgs = import ghc-nixpkgs { 
          inherit system;
        };
      in self: super: {
        haskell = super.lib.recursiveUpdate super.haskell 
          {
            compiler = { "ghc${ghcVersion}" = ghcPkgs.haskell.compiler."ghc${ghcVersion}"; };
            packages = { "ghc${ghcVersion}" = ghcPkgs.haskell.packages."ghc${ghcVersion}"; };
          };
      };

      # The packages that we need alongside the clash compiler itself
      clashPkgs = pkgs."clashPackages-ghc${ghcVersion}";

      # We also need the actual build inputs from all of these clash packages
      clashGetBuildInputsFrom = [ 
        clashPkgs.clash-benchmark
        clashPkgs.clash-cores
        clashPkgs.clash-cosim
        clashPkgs.clash-ffi
        clashPkgs.clash-ghc
        clashPkgs.clash-lib
        clashPkgs.clash-lib-hedgehog
        clashPkgs.clash-prelude
        clashPkgs.clash-prelude-hedgehog
        clashPkgs.clash-profiling
        clashPkgs.clash-profiling-prepare
        clashPkgs.clash-term
        clashPkgs.clash-testsuite
      ];
      
      # This is the combinmed set of build inputs we need to get clash running
      clashBuildInputs = [
        clashPkgs.cabal-install
        clashPkgs.haskell-language-server
        clashPkgs.hspec
        pkgs.ghdl-llvm
        pkgs.nixpkgs-fmt
        pkgs.symbiyosys
        pkgs.verilator
        pkgs.verilog
        pkgs.yosys
      ] ++ builtins.concatMap (pkg: pkg.env.nativeBuildInputs) clashGetBuildInputsFrom;

      # This is the main development shell
      # It does not run Vivado, that's too tricky to set up, so you need to have that working
      # locally already.
      # Everything else should run from this shell, that includes:
      # Python (with pytest, cocotb, myhdl, and all internal/external test packages)
      # Icarus Verilog
      # Clash (with clash-protocols module)
      # Ghc (for running clash tests, again with clash-protocols module)
      dev_shell = let
          # Python, with all of the test packages that are needed for cocotb/myhdl tests
          python = pkgs."python${pythonVersion}".withPackages (ps: [ 
              ps.cocotb
              ps.scapy
              ps.cocotb-bus
              pkgs.cocotb-test
              pkgs.cocotb_lib
              ps.pytest
            ]);
          # Ghc, with all of the clash packages
          ghc = pkgs.haskell.packages."ghc${ghcVersion}".ghcWithPackages (ps: with ps; [
              clashPkgs.clash-ghc
              clashPkgs.clash-prelude
              clashPkgs.ghc-typelits-extra
              ps.data-default
            ]);
        in pkgs.mkShell {
          buildInputs = [  python  pkgs.iverilog  ghc ] ++ clashBuildInputs;
        };
    in
    {
      devShells.${system} = {
        default = dev_shell;
      };
    };
}
