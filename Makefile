# * makem.sh/Makefile --- Script to aid building and testing Emacs Lisp packages

# URL: https://github.com/alphapapa/makem.sh
# Version: 0.5

# * Arguments

# For consistency, we use only var=val options, not hyphen-prefixed options.

# NOTE: I don't like duplicating the arguments here and in makem.sh,
# but I haven't been able to find a way to pass arguments which
# conflict with Make's own arguments through Make to the script.
# Using -- doesn't seem to do it.

ifdef install-deps
	INSTALL_DEPS = "--install-deps"
endif
ifdef install-linters
	INSTALL_LINTERS = "--install-linters"
endif

ifdef sandbox
	ifeq ($(sandbox), t)
		SANDBOX = --sandbox
	else
		SANDBOX = --sandbox=$(sandbox)
	endif
endif

ifdef debug
	DEBUG = "--debug"
endif

# ** Verbosity

# Since the "-v" in "make -v" gets intercepted by Make itself, we have
# to use a variable.

verbose = $(v)

ifneq (,$(findstring vvv,$(verbose)))
	VERBOSE = "-vvv"
else ifneq (,$(findstring vv,$(verbose)))
	VERBOSE = "-vv"
else ifneq (,$(findstring v,$(verbose)))
	VERBOSE = "-v"
endif

# * Rules

# TODO: Handle cases in which "test" or "tests" are called and a
# directory by that name exists, which can confuse Make.

%:
	@./makem.sh $(DEBUG) $(VERBOSE) $(SANDBOX) $(INSTALL_DEPS) $(INSTALL_LINTERS) $(@)

.DEFAULT: init
init:
	@./makem.sh $(DEBUG) $(VERBOSE) $(SANDBOX) $(INSTALL_DEPS) $(INSTALL_LINTERS)

JULIA = julia

.instantiated-marker:
	$(JULIA) --project=. -e 'using Pkg; Pkg.instantiate(); write(open(".instantiated-marker","w"), "")'

formatter_service_precompile.jl: .instantiated-marker
	$(JULIA) --project=. --trace-compile=formatter_service_precompile.jl -e 'using JSON; using JuliaFormatter; using CSTParser; JSON.json(JSON.parse("{\"a\":[1,2]}"));format_text("Channel()"); CSTParser.parse("Channel()")'

formatter_service_sysimage.so: formatter_service_precompile.jl
	$(JULIA) --project=. -e 'using PackageCompiler;PackageCompiler.create_sysimage(["JSON", "JuliaFormatter", "CSTParser"],sysimage_path="formatter_service_sysimage.so", precompile_statements_file="formatter_service_precompile.jl")'

run-service: formatter_service_sysimage.so
	@$(JULIA) --project=. --sysimage=formatter_service_sysimage.so formatter_service.jl
