Leverage JuliaFormatter.jl to indent julia code in Emacs.

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)


## Why?

There are several issues with current indentation with `julia-mode`. Several solutions (tree-sitter, SMIE, fixing current indentation engine) seem impractical.

## Usage

The simplest way to use this package is by activating format on save on Julia buffers like so:

```elisp
  ;; load julia-formatter.el after downloading this package (or installing with straight.el)
  (load-file "/somewhere/to/julia-formatter.el/julia-formatter.el")
  (require 'julia-formatter)
  (add-hook 'julia-mode-hook #'julia-formatter-mode)

  ;; (recommended) load the server in the background after startup
  (add-hook 'after-init-hook #'julia-formatter--ensure-server)
```

Alternatively, formatting can be done live by activating [`aggressive-indent-mode'](https://github.com/Malabarba/aggressive-indent-mode/)
and setting the proper functions.

Like so:
```elisp
  (add-hook 'julia-formatter-mode-hook #'aggressive-indent)
```

## How

This packages is comprised by several pieces.
- Pack text formatting ([JuliaFormatter.jl](https://github.com/domluna/JuliaFormatter.jl)) into a service that will be used with live coding.
- Expose this service with JSON-RPC on stdin / stdout.
- Boot/start this service from Emacs
- Send julia code from a buffer (by either manual command or automatized indent) to the service, get formatted code and paste it into the buffer.

The exposure of JuliaFormatter.jl is done as a service because calling `format_text()` as a standalone script takes a lot of time. This would hinder the coding process.

The code that's being formatted must be self-contained (parseable, all `if`'s and `while`'s must have a corresponding `end`, all parentheses must be closed, etc).  This is a requirement from JuliaFormatter.jl since it will fail if the code cannot be fully parsed.


## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Addendum

See https://github.com/domluna/JuliaFormatter.jl
