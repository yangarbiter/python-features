Based on [https://github.com/ucsd-progsys/nanomaly](https://github.com/ucsd-progsys/nanomaly)

Build using `stack build`

Generate features using

```
stack exec -- generate-features \
           --source example.json \
           --features op+context \
           --out out
```

where json in example.json should have three fields - "bad" and "fix" are two Python programs, and "index" is an arbitrary integer.

This will create a `out/op+context/[index].csv` file with the feature vectors, which can then be used normally by NATE.
