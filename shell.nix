{ pkgs ? import <nixpkgs> { } }:
let
  updateCabal = pkgs.writers.writeBashBin "update-cabal" ''
    echo "# created by cabal2nix " > ${toString ./.}/current-project.nix
    cd ${toString ./.}
    ${pkgs.cabal2nix}/bin/cabal2nix . >> ${toString ./.}/current-project.nix
  '';

  run = pkgs.writers.writeBashBin "run" ''
    cd ${toString ./.}
    ${pkgs.cabal-install}/bin/cabal run site -- clean
    ${pkgs.cabal-install}/bin/cabal run site -- watch
  '';

  deploy = pkgs.writers.writeBashBin "deploy" ''
    cd ${toString ./.}
    ${pkgs.cabal-install}/bin/cabal run site -- clean
    ${pkgs.cabal-install}/bin/cabal run site -- build
    ${pkgs.cabal-install}/bin/cabal run site -- deploy
  '';

  # fixme: a hack until I have a nicer solution in haskell
  genOptions = pkgs.writers.writeBashBin "gen-options"
  ''
    cat > ${toString ./.}/options.html <<EOF
    <p>
    Here are the options negatively available by terranix.
    You can also seem them via man terranix-modules.

    <div class="alert alert-error">This page is still a work in progress and will change soon</div>
    </p>
    EOF

    cat ${toString ./.}/options.json | ${pkgs.jq}/bin/jq --raw-output 'to_entries | .[] | "
    <div class=\"media\">
      <div class=\"media-left\">
        <div class=\"avatarholder\"></div>
      </div>
      <div class=\"media-body\">
        <div class=\"media-heading\">\( @html "\(.key)" )</div>
        <div class=\"media-content\">\( @html "\( .value.description )" )</div>
      </div>
    </div>
    "' >> ${toString ./.}/options.html
    '';


in pkgs.mkShell { buildInputs = with pkgs; [ updateCabal run deploy lessc genOptions ]; }

