[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)
Leverage JuliaFormatter.jl to indent julia code in Emacs.

## Why?

There are several issues with current indentation with `julia-mode`. Several solutions (tree-sitter, SMIE, fixing current indentation engine) seem impractical.

## How
Provide formatting tools for live coding.  These tools are packed into a service
that can be called using JSON-RPC on stdin / stdout.  Exposing JuliaFormatter.jl
as a service because the compile time required to get the result of that
first format_text() call is considerable and it hinders the coding process.

The code that's being formatted must be self-contained (parseable, all if's
and while's with a corresponding end).  This is a requirement from
JuliaFormatter.jl since it needs to get the AST from the code.

## Usage

The simplest way to use this package is by activating [`aggressive-indent-mode'](https://github.com/Malabarba/aggressive-indent-mode/)
and setting the proper functions.

Like so:
```elisp
  ;; load julia-formatter.el after downloading this package (or installing with straight.el)
  (load-file "/somewhere/to/julia-formatter.el/julia-formatter.el")
  (require 'julia-formatter)
  ;; load aggressive indent and setup appropiate variables
  (julia-formatter-setup-hooks)
```

## Code of Conduct
Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Addendum

See https://github.com/domluna/JuliaFormatter.jl
