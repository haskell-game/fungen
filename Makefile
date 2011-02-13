# project makefile

######################################################################
# BUILD

# BUILDFLAGS=-threaded -W -fwarn-tabs -Werror $(PREFERMACUSRLIBFLAGS)

# build:
# 	cabal build

# AUTOBUILDCMDARGS=tests
# autobuild auto:
# 	sp --no-exts --no-default-map -o $(EXE) ghc --make $(BUILDFLAGS) $(EXE).hs --run $(AUTOBUILDCMDARGS)

test: haddock
	cabal install

######################################################################
# DOC

# called on each darcs commit
commithook: docs

docs: haddock

# build haddock docs
haddock:
	cabal configure && cabal haddock

# render docs locally, for preview
localdocs:
	pandoc -s README.md > README.html
	pandoc -s TUTORIAL.md > TUTORIAL.html

######################################################################
# RELEASE

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell basename $(TARBALL) .tar.gz | cut -d- -f2-)

showversion:
	@echo $(VERSION)

tagrepo:
	@(darcs show tags | grep -q "^$(VERSION)$$") && echo tag $(VERSION) present, ok || (please run darcs tag $(VERSION); exit 1)

uploadordebug:
	(cabal upload $(TARBALL) --check | grep '^Ok$$') \
		&& cabal upload $(TARBALL) \
		|| (cabal upload $(TARBALL) --check -v3; false)

release: test tagrepo uploadordebug

######################################################################
# MISC

tag: emacstags

emacstags:
	rm -f TAGS; hasktags -e `find . -name dist -prune -o -name "*hs" -print` *.cabal

clean:
	cabal clean
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`
