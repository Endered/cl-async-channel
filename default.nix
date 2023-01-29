{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell
  {
    nativeBuildInputs = with pkgs; [ roswell libuv ];
    shellHook = ''
                   export LD_LIBRARY_PATH=${pkgs.libuv.out}/lib:$LD_LIBRARY_PATH
'';
  }
