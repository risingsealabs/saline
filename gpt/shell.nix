{}:
let
  # merge of https://github.com/NixOS/nixpkgs/pull/375144
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/ef8ec643f4838522e2584884ffee0c0faa14c142.tar.gz";
    sha256 = "sha256:03bx0j49hjqrsfs7qy5r8djxp0daazy43q06lp2mmgcfg24w3ag9";
  };
  pkgs = import nixpkgs {};

  py = pkgs.python312.withPackages
    (ps: with ps; [
      # pyairports
      cloudpickle
      datasets
      diskcache
      instructor
      interegular
      joblib
      jsonschema
      lark
      llama-cpp-python
      nest-asyncio
      numba
      outlines
      pycountry
      pydantic
      scipy
      sentencepiece
      torch
      transformers
    ]);
in
pkgs.mkShell {
  buildInputs = [
    py
  ];
}
