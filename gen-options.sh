cat options.json | jq --raw-output 'to_entries | .[] | "
<div class=\"media\">
  <div class=\"media-left\">
    <div class=\"avatarholder\"></div>
  </div>
  <div class=\"media-body\">
    <div class=\"media-heading\">\( @html "\(.key)" )</div>
    <div class=\"media-content\">\( @html "\( .value.description )" )</div>
  </div>
</div>
"' > options.html
