# how to update

## generate core-options.json
```
terranix-doc-json \
  --path /nix/store/yp1h149c9mbc4sp4c65iwwlh1rfcxh9x-terranix \
  --url https://github.com/mrVanDalo/terranix/tree/master/ \
  core-options.nix | jq '.' > core-options.json
```

`--path` is to shorten the url, just run without it, and you'll see.

## generate core-options.markdown

than run `gen-options`.
