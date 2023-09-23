# { pkgs ? import <nixpkgs> {} }:
let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/ace5093e36ab1e95cb9463863491bee90d5a4183.tar.gz"; # unstable as of 9/16/23
    sha256 = "sha256:0scwlhcz9kzl86yqrdk1hc3fjbli6yxyd0na9qn4q5cm53nzdqg6";
  }) {};
in
pkgs.mkShell {
  buildInputs = [ pkgs.hlint (pkgs.ghc.withPackages (p: [ p.data-has ])) ];
}
