{
  description = "Projeto Compiladores";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };

      davidMatosUrl = "https://web.tecnico.ulisboa.pt/~david.matos/w/pt/images";

      cdk = pkgs.stdenv.mkDerivation {
        name = "cdk";
        src = pkgs.fetchzip {
          url = "${davidMatosUrl}/3/39/Libcdk18-202304130000.tar.bz2";
          sha256 = "sha256-+/ovPwAWWv/Bf/uuCpAapx+9vg8E8IZMBV3jxXO+CYg=";
        };
        buildInputs = with pkgs; [ python3 doxygen graphviz ];
        configurePhase = ''
          mkdir -p $out
          sed -i 's|^ROOT = .*|ROOT = $\{out}|' Makefile
          sed -i 's|#DOC ||' Makefile
        '';
        preBuild = ''
          patchShebangs bin/cdk
          export HOME=$PWD
        '';
        FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
        FONTCONFIG_PATH = "${pkgs.fontconfig.out}/etc/fonts/";
      };

      rts = pkgs.stdenv.mkDerivation {
        name = "rts";
        src = pkgs.fetchzip {
          url = "${davidMatosUrl}/b/be/Librts5-202103031636.tar.bz2";
          sha256 = "sha256-lc4+X9A0BkxmAlUv+Z915F2rKMojXMA14EhQFKpqJlc=";
        };
        buildInputs = with pkgs; [ yasm ];
        configurePhase = ''
          mkdir -p $out
          sed -i 's|^ROOT = .*|ROOT = $\{out}|' Makefile
          sed -i '/^COMMON_FLAGS =/ s|$| -fno-stack-protector|' Makefile
        '';
      };

      c_cpp_properties = pkgs.writeText "c_cpp_properties.json" (builtins.toJSON {
        configurations = [{
          name = "Linux";
          includePath = [ "$\{workspaceFolder}/**" "$\{ROOT}/usr/include/" ];
        }];
      });
    in
    {
      devShell = pkgs.mkShell (rec {
        buildInputs = with pkgs; [ git cmake gcc yasm flex bison zsh ];

        ROOT = pkgs.symlinkJoin {
          name = "root";
          paths = [ rts cdk ];
        };

        shellHook = ''
          if [ ! -f ".nix-init" ]; then
            sed -i '/^ROOT = /d' Makefile
            git update-index --assume-unchanged Makefile
            mkdir .vscode
            cp --no-preserve=mode ${c_cpp_properties} .vscode/c_cpp_properties.json
	    touch .nix-init
          fi

          if [ ! -d "docs" ]; then
            mkdir -p docs
            cp --no-preserve=mode -r ${ROOT}/usr/share/doc/packages/cdk/html docs/cdk
          fi
        '';
      });
    });
}
