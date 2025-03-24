<!---
This file was generated from `meta.yml`, please do not edit manually.
Follow the instructions on https://github.com/coq-community/templates to regenerate.
--->
# Copland Parser

[![Docker CI][docker-action-shield]][docker-action-link]

[docker-action-shield]: https://github.com/ku-sldg/copland-parser/actions/workflows/docker-action.yml/badge.svg?branch=main
[docker-action-link]: https://github.com/ku-sldg/copland-parser/actions/workflows/docker-action.yml




A parser for the Copland DSL for layered attestation

## Meta

- Author(s):
  - Will Thomas
- License: [Creative Commons Attribution Share Alike 4.0 International](LICENSE)
- Compatible Coq versions: 8.20 later
- Compatible OCaml versions: 4.12 or later
- Additional dependencies:
  - [ExtLib](https://github.com/coq-community/coq-ext-lib)
  - [RocqCandy](https://github.com/ku-sldg/rocq-candy)
  - [CoplandSpec](https://github.com/ku-sldg/copland-spec)
  - [Parsec](https://github.com/liyishuai/coq-parsec)
  - [Menhir](http://gallium.inria.fr/~fpottier/menhir/)
  - [MenhirLib](https://gitlab.inria.fr/fpottier/menhir/-/tree/master/coq-menhirlib/)
  - [Dune](https://dune.build) 3.17 or later
- Related publication(s): none

## Building and installation instructions

The easiest way to install the latest released version of Copland Parser
is via [OPAM](https://opam.ocaml.org/doc/Install.html):

```shell
opam repo add -a --set-default ku-sldg/opam-repo https://github.com/ku-sldg/opam-repo.git
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-copland-parser
```

To instead build and install manually, do:

``` shell
git clone https://github.com/ku-sldg/copland-parser.git
cd copland-parser
dune build
dune install
```



