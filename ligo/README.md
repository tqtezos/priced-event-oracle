
An implementation of `user_contract.tz` in LIGO.

Compile with:

```bash
docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.19.0 compile-contract \
  --syntax=cameligo --output=ligo_user_contract.tz user_contract.ligo.ml main
```

