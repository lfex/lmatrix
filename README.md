# lmatrix

<img src="resources/images/linear-subspaces.png" />

*A Simple Linear Algebra Library for LFE*


## Introduction

Erlang is not a programming language designed for nor performant with
numerical calculations. This library is for fun only, not for scientific
computing purposes.


## Installation

Just add it to your ``rebar.config`` deps:

```erlang
    {deps, [
        ...
        {lmatrix, ".*", {git, "git@github.com:YOURNAME/lmatrix.git", "master"}}
      ]}.
```

And then do the usual:

```bash
    $ rebar get-deps
    $ rebar compile
```


## Usage

Start up the repl, after compiling as above:

```bash
$ make repl-no-deps
```

Then use the library at will:

```cl
> (set matrix
    '((1 2 3)
      (4 5 6)
      (7 8 9)))
((1 2 3) (4 5 6) (7 8 9))
> (lmatrix:dim matrix)
(3 3)
> (lmatrix:transpose matrix)
((1 4 7) (2 5 8) (3 6 9))
```
