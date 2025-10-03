let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.10-20231025/src/packages.dhall

let overrides = {=}

in  upstream // overrides