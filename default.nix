#vim: set et ts=4 sw=4:
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, basic-prelude, base, bytestring, chromaprint, directory,
        ffmpeg, HandsomeSoup, http-conduit, hxt, lens, lens-aeson, liblastfm,
        makeWrapper, optparse-applicative, process-extras, safe, safe-exceptions, stdenv, text,
        temporary, youtube-dl
      }:
      mkDerivation {
        pname = "musicdownloader";
        version = "0.1.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildDepends = [ makeWrapper ];
        libraryHaskellDepends = [
          basic-prelude base bytestring directory HandsomeSoup http-conduit hxt
          lens lens-aeson liblastfm process-extras safe safe-exceptions temporary text
        ];
        executableHaskellDepends = [
          base basic-prelude directory optparse-applicative temporary text
        ];
        postInstall = ''
          wrapProgram $out/bin/musicdownloader $wrapperfile --prefix PATH : "${pkgs.lib.makeBinPath [chromaprint ffmpeg youtube-dl]}"
        '';
        executableSystemDepends = [ chromaprint ffmpeg youtube-dl ];
        homepage = "http://github.com/zenhoth/musicdownloader#readme";
        description = "Download, convert and tag music from youtube via last.fm";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
