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

  genOptions = pkgs.writers.writeBashBin "gen-options" ''
    cd ${toString ./options}
    for option in `ls | egrep json$`
    do
      file=`basename $option ".json"`
      echo $file

      cat > ${toString ./options}/''${file}.html <<EOF
    ---
    title: ''${file}
    letter: ''${file:0:1}
    info: ""
    ---
    EOF

      # create first part
      ${pkgs.pandoc}/bin/pandoc --from markdown --to html5 \
        ${toString ./options}/''${file}.markdown \
      >> ${toString ./options}/''${file}.html

      echo '<ul class="options-list">' >> ${toString ./options}/''${file}.html
      # create options list
      cat ${toString ./options}/''${file}.json \
      | ${pkgs.jq}/bin/jq --raw-output 'to_entries | .[] | "
      <li>
      <strong>\(@html "\( .key )" )</strong>
      <p><strong>type</strong>: \( .value.type )</p>
      <p><strong>default</strong>: \( .value.default )</p>
      <p><strong>description</strong>: \( @html "\(.value.description)" )</p>
      <p><strong>defined</strong>: in <a href=\"\( .value.declarations[0].url )\"> \( .value.declarations[0].path )</a>
      \( if .value.example then "<p><strong>example</strong>: \(.value.example)" else "" end )
      </li>
      "' >> ${toString ./options}/''${file}.html
      echo "</ul>" >> ${toString ./options}/''${file}.html

    done
  '';

in pkgs.mkShell {
  buildInputs = with pkgs; [ updateCabal run deploy lessc genOptions ];
}

