# Minimal makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = Servant
SOURCEDIR     = .
BUILDDIR      = _build

# Put it first so that "make" without argument is like "make help".
help:
	@if [ ! -d venv ]; then echo "WARNING: There is no venv directory, did you forget to 'virtualenv venv'. Check building-the-docs file."; fi
	@if [ ! "z$$(which $(SPHINXBUILD))" = "z$$(pwd)/venv/bin/sphinx-build" ]; then echo "WARNING: Did you forgot to 'source venv/bin/activate'"; fi
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
