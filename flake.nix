{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, ... }:
    let
      package = { haskell, stdenvNoCC }:
        stdenvNoCC.mkDerivation {
          name = "autofan";

          src = ./.;
          buildInputs = [
            (haskell.packages.ghc901.ghcWithPackages
              (haskellPackages: with haskellPackages; [ HTTP xml ]))
          ];

          buildPhase = "ghc -Wall Autofan.hs";
          installPhase = "install -D Autofan $out/bin/autofan";
        };

      module = { pkgs, config, lib, ... }:
        with lib;
        let
          cfg = config.services.autofan;
          autofan = "${self.packages.${pkgs.system}.autofan}/bin/autofan";
        in {
          options.services.autofan = {
            enable = mkEnableOption "Automatic fan controller";

            location = mkOption {
              description = "Location to fetch temperature data for.";
              type = types.str;
            };
          };

          config.systemd.services.autofan = {
            description = "Automatic fan controller";

            wants = [ "network-online.target" ];
            after = [ "network-online.target" ];
            wantedBy = [ "default.target" ];

            serviceConfig = {
              Restart = "on-failure";
              ExecStart = "${autofan} ${escapeShellArg cfg.location}";
            };

            # The Haskell program assumes that the fan is off when it starts
            preStart = "echo 1-1 >/sys/bus/usb/drivers/usb/unbind || exit 0";
            postStop = "echo 1-1 >/sys/bus/usb/drivers/usb/unbind || exit 0";
          };
        };

    in {
      nixosModules.autofan = module;
    } // (utils.lib.eachSystem [ "aarch64-linux" "i686-linux" "x86_64-linux" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          autofan = pkgs.callPackage package { };
        in {
          packages.autofan = autofan;
          defaultPackage = autofan;
        }));
}
