Leverage JuliaFormatter.jl to indent julia code in Emacs.

## Why?
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

## Addendum

See https://github.com/domluna/JuliaFormatter.jl
