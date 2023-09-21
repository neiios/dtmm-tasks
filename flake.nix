{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/23.05";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux.pkgs;
  in {
    devShells.x86_64-linux.default = let
      R-with-packages =
        pkgs.rWrapper.override
        {
          packages = with pkgs.rPackages; [
            # libraries
            tidyverse # provides many packages at once
            psych
            # dev tools
            lintr
            httpgd
            languageserver
          ];
        };
    in
      pkgs.mkShell {
        packages = with pkgs; [
          R-with-packages
        ];
      };
  };
}
