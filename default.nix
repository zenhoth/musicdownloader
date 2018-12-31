{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, basic-prelude, base, bytestring, chromaprint, directory
      , ffmpeg, HandsomeSoup, http-conduit, hxt, lens, lens-aeson
      , liblastfm, makeWrapper, process-extras, safe, safe-exceptions, stdenv, text
      , temporary, youtube-dl
      }:
      mkDerivation {
        pname = "musicdownloader";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildDepends = [ makeWrapper ];
        libraryHaskellDepends = [
          basic-prelude base bytestring directory HandsomeSoup http-conduit
          hxt lens lens-aeson liblastfm process-extras safe safe-exceptions temporary text
        ];
        executableHaskellDepends = [
          base basic-prelude directory temporary text
        ];
        postInstall = ''
          wrapProgram $out/bin/musicdownloader $wrapperfile --prefix PATH : "${pkgs.lib.makeBinPath [chromaprint ffmpeg youtube-dl]}"
        '';
        executableSystemDepends = [ chromaprint ffmpeg youtube-dl ];
        homepage = "http://github.com/xenos5/musicdownloader#readme";
        description = "Scrape music from youtube via last.fm";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
