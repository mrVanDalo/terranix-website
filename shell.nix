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


    <ul class="options-list">

    EOF

    cat ${toString ./.}/options.json | ${pkgs.jq}/bin/jq --raw-output 'to_entries | .[] | "
    <li>
    <strong>\(@html "\( .key )" )</strong>
    <p><strong>type</strong>: \( .value.type )</p>
    <p><strong>default</strong>: \( .value.default )</p>
    <p><strong>description</strong>: \( @html "\(.value.description)" )</p>
    <p><strong>defined</strong>: in <a href=\"\( .value.declarations[0].url )\"> \( .value.declarations[0].path )</a>
    \( if .value.example then "<p><strong>example</strong>: \(.value.example)" else "" end )
    </li>
    "' >> ${toString ./.}/options.html
    echo "</ul>" >> ${toString ./.}/options.html
    '';


in pkgs.mkShell { buildInputs = with pkgs; [ updateCabal run deploy lessc genOptions ]; }

