{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

with pkgs;

haskell.lib.buildStackProject {
  name = "my-haskell-libraries";
  inherit ghc;
  buildInputs = [ zlib ];

  # https://github.com/commercialhaskell/stack/commit/568938da
  TMPDIR = "/tmp";
}
