 {
  lib,
  python,
  fetchFromGitHub,
  setuptools-scm,
  pip,
  pytest,
  cocotb,
  find-libpython,
}:

python.pkgs.buildPythonPackage rec {
  pname = "cocotb-test";
  version = "0.2.5";
  format = "setuptools";

  # pypi source doesn't include tests
  src = fetchFromGitHub {
    owner = "themperek";
    repo = "cocotb-test";
    rev = "refs/tags/v${version}";
    hash = "sha256-xM36rLWfTmtuLZZobTyxUAhrozYIdUDeVDnx49B0Dzs=";
  };

  nativeBuildInputs = [ python.pkgs.setuptools-scm ];

  buildInputs = [ python.pkgs.setuptools ];
  propagatedBuildInputs = [ python.pkgs.find-libpython ];
}
