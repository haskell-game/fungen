# project makefile

export LANG=en_US.UTF-8

######################################################################
# BUILD

test:
	cabal test

install:
	cabal install

build:
	cabal build

# build automatically on file change using sp
# flag(s) to work around ghc vs. macports issue on mac, if needed:
#PREFERMACUSRLIBFLAGS=-L/usr/lib
#BUILDFLAGS=-threaded -W -fwarn-tabs -Werror $(PREFERMACUSRLIBFLAGS)
# sp needs an executable:
AUTOBUILDEXE=fungentest
AUTOBUILDEXEARGS=tests
autobuild auto:
	@echo "auto-building with sp (if this fails, do make sp)"
	sp --no-exts --no-default-map -o $(AUTOBUILDEXE) ghc $(BUILDFLAGS) $(AUTOBUILDEXE).hs --run $(AUTOBUILDEXEARGS)

sp:
	@echo "To install sp:"
	@echo "darcs get http://joyful.com/repos/searchpath,"
	@echo "cd searchpath, make, add the sp executable to your PATH"

######################################################################
# DOC

# called on each darcs commit
commithook: #docs

docs: haddock site-build

# build haddock docs
haddock:
	cabal configure && cabal haddock #--executables

# preview haddock docs
VIEWHTML=open
haddock-view: haddock
	$(VIEWHTML) dist/doc/html/FunGEn/index.html

# build site with "hakyll", my generic hakyll site build script
# twice to work around a glitch
site-build:
	-hakyll build
	hakyll build

site-clean: site
	hakyll clean

######################################################################
# RELEASE

TARBALL:=$(shell cabal sdist | tail -1 | cut -d' ' -f4)
VERSION:=$(shell basename $(TARBALL) .tar.gz | cut -d- -f2-)

showversion:
	@echo $(VERSION)

tagrepo:
	@(darcs show tags | grep -q "^$(VERSION)$$") && echo tag $(VERSION) present, ok || (echo please run: darcs tag $(VERSION); exit 1)

uploadordebug:
	(cabal upload $(TARBALL) --check | grep '^Ok$$') \
		&& cabal upload $(TARBALL) \
		|| (cabal upload $(TARBALL) --check -v3; false)

release: test tagrepo uploadordebug

######################################################################
# STATS

# show project stats useful for release notes
releasestats stats: \
	showreleasedays \
	showunreleasedchangecount \
	showloc \
	showreleaseauthors \
	showunreleasedcodechanges \
	showunpushedchanges
#	showtestcount \
#	showunittestcoverage \

showreleasedays:
	@echo Days since last release:
	@../hledger/tools/dayssincerelease.hs | head -1 | cut -d' ' -f-1
	@echo

showunreleasedchangecount:
	@echo Commits since last release:
	@darcs changes --from-tag . --count
	@echo

showreleaseauthors:
	@echo Patch authors since last release:
	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq
	@echo

showloc:
	@echo Current lines of code including tests:
	@sloccount `find . -name '*hs'` | grep haskell:
	@echo

# showtestcount:
# 	@echo "Unit tests:"
# 	@hledger test 2>&1 | cut -d' ' -f2
# 	@echo "Functional tests:"
# 	@make --no-print functest | egrep '^ Total' | awk '{print $$2}'
# 	@echo

# showunittestcoverage:
# 	@echo Unit test coverage:
# 	@make --no-print quickcoverage | grep 'expressions'
# 	@echo

# showerrors:
# 	@echo Known errors:
# 	@awk '/^** errors/, /^** / && !/^** errors/' NOTES | grep '^\*\*\* ' | tail +1
# 	@echo

showunpushedchanges unpushed:
	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
	@-darcs push --dry-run | grep '*' | tac
	@echo

showunreleasedcodechanges unreleased:
	@echo "code changes since last release:"
	@darcs changes --from-tag . --matches "not (name docs: or name doc: or name site: or name tools:)" | egrep '^  \* '
	@echo

showcodechanges:
	@echo "code changes:"
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

######################################################################
# MISC


TAGCMD=hasktags -e

tag: TAGS

TAGFILES=*.hs Graphics/UI/*.hs Graphics/UI/*/*.hs *.cabal *.md Makefile #tests/*.test 
TAGS: $(TAGFILES)
	$(TAGCMD) $(TAGFILES)


clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean siteclean
	rm -f TAGS # _cache #_site
	cabal clean
