{ 
  lib
  , buildPythonPackage
  , setuptools-scm
  , cocotb
}:
buildPythonPackage rec {
  pname = "cocotb_lib";
  version = "1.0.0";

  buildInputs = [ setuptools-scm ];

  src = lib.cleanSource ../${pname};
} 
