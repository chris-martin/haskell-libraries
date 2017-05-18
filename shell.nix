{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

with pkgs;

haskell.lib.buildStackProject {
  name = "my-haskell-libraries";
  inherit ghc;
  buildInputs = [
    cairo
    glib
    gtk3
    pango
    pkgconfig
    xorg.libX11
    xorg.libXrandr
    xorg.libXext
    zlib
  ];

  # https://github.com/commercialhaskell/stack/commit/568938da
  TMPDIR = "/tmp";
}
