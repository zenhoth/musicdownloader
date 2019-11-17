# musicdownloader
This is a simple utility that does the following:
* Takes a song name and artist
* Looks it up on [last.fm](https://www.last.fm)
* If a video is provided, downloads it
* Confirms that it is the right song by using [chromaprint](https://github.com/acoustid/chromaprint), [AcoustID](https://acoustid.org) and [MusicBrainz](https://musicbrainz.org/)
* Converts it to 320k CBR MP3 (not my preferred choice, but required for compatibility with my personal use case of Deezer uploading)
* Tags it with the confirmed metadata from [MusicBrainz](https://musicbrainz.org/) (title, artist, album) 
* Downloads and embeds the album cover art using [Cover Art Archive](https://coverartarchive.org/)
## Installation
* Install [nix](https://nixos.org/nix/) (a package manager that runs in parallel to your system package manager; works on linux and mac)
* In the repository root, run `nix-env -f default.nix -i`

This will install all the needed haskell dependencies, all the needed system dependencies, and `musicdownloader` itself.

If it fails to build, nixpkgs probably upgraded to a package version that broke backwards API compatibility. Barring actually fixing the API incompatibility, you can do a build with a known working set of nix packages:

```
env NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz nix-env -f default.nix -i
```
This isn't done by default because of the potential for backwards-compatible bug fixes in the newer packages, and because using a fixed snapshot may require nix to perform some recompilation, depending on how much the nix binary cache keeps.
## Usage
You will need a [last.fm API key](https://www.last.fm/api/account/create) and an [AcoustID API key](https://acoustid.org/new-application). Provide them in the LAST_FM_API_KEY and ACOUSTID_API_KEY environment variables. Provide track title and artist in the `--title` and `--artist` arguments.

Example:
```
user@host$ env LAST_FM_API_KEY=abcdefghijklmnopqrstuvwxyz012345 ACOUSTID_API_KEY=ABCDEFGHIJ musicdownloader --track "Surface" --artist "Aero Chord"
Found last.fm url: https://www.last.fm/music/Aero+Chord/_/Surface
Found youtube video: https://www.youtube.com/watch?v=BrCKvKXvN2c
Downloaded temporary audio to: /tmp/musicdownloader-25ff2a1d21ca5bd9/[Trap] - Aero Chord - Surface [Monstercat Release].webm
Scanned fingerprint
Found recording MBID: 556be399-7dbf-4edb-a475-98f0b967f584
Track: Surface
Artists: Aero Chord
Album: Surface
Matched track and artist
Converted final result to: Aero Chord - Surface.mp3
```

It outputs the resulting file to the current directory.
