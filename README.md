# lambda
[⚠️wip] a simple lambda calculus interpreter in haskell 

# How to run examples
You can run the [](./examples) by first entering the nix-shell:
```sh
nix-shell
```

Afterwards you can either build with `ghc lambda.hs`
```sh
./lambda -- -f <path_to_example>
```

It is also possible to directly pass an expression to the interpreter:
```sh
./lambda -- -c "((\x.\y.x true) false)"
```
