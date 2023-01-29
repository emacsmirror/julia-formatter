Use JuliaFormatter.jl to format julia code in Emacs.

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

[![MELPA](https://melpa.org/packages/julia-formatter-badge.svg)](https://melpa.org/#/julia-formatter)

## Why?

There are several issues with current indentation with `julia-mode`. Several solutions (tree-sitter, SMIE, fixing current indentation engine) seem impractical.

## Usage

The simplest way to use this package is by activating format on save on Julia buffers like so:

```elisp
  ;; load julia-formatter.el after downloading this package
  ;; You can also install with MELPA or with straight.el and skip this line
  (load-file "/somewhere/to/julia-formatter.el/julia-formatter.el")

  (require 'julia-formatter)
  (add-hook 'julia-mode-hook #'julia-formatter-mode)

  ;; (recommended) load the server in the background after startup
  (add-hook 'after-init-hook #'julia-formatter--ensure-server)
```

Alternatively, formatting can be done live by activating [`aggressive-indent-mode'](https://github.com/Malabarba/aggressive-indent-mode/)
and setting the proper indentation functions (yes, indentation functionality can be leveraged for live formatting).

Like so:
```elisp
  (add-hook 'julia-formatter-mode-hook #'aggressive-indent)
```
### Doom Emacs

Because this package also contains project files to handle Julia dependencies, we need to explicitly tell [Doom](https://docs.doomemacs.org/latest/) (through [straight.el](https://github.com/radian-software/straight.el)):
Like so:

```elisp
;; add the following to packages.el
(package! julia-mode) ;; you probably already have this line
(package! julia-formatter
  :recipe (:host codeberg :repo "FelipeLema/julia-formatter.el"
           :files ( "julia-formatter.el" ;; main script executed by Emacs
                    "formatter_service.jl" ;; script executed by Julia
                    "Manifest.toml" "Project.toml" ;; project files
                    )))
```

### No other dependencies ?

No. Julia packages are handled with this repo's Project.toml & Manifest.toml files.

Think Python's virtual-envs.

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
