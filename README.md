# mini-lambda

Minimal lambda language for the Compiler Construction course supervisions.

### Prerequisites

`opam2` is required to install `dune` and the `menhir` parser generator.


### Usage


To compile a file and dump assembly to stdout, run:

```
lambda <path-to-file>
```

To link with the runtime and produce an executable:

```
lambda <path-to-file> -o <path-to-output>
```
