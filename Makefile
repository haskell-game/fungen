# project makefile

export LANG=en_US.UTF-8

######################################################################
# BUILD


######################################################################
# DOC


######################################################################
# RELEASE


######################################################################
# STATS

# # show project stats useful for release notes
# releasestats stats: \
# 	showreleasedays \
# 	showunreleasedchangecount \
# 	showloc \
# 	showreleaseauthors \
# 	showunreleasedcodechanges \
# 	showunpushedchanges
# #	showtestcount \
# #	showunittestcoverage \

# showreleasedays:
# 	@echo Days since last release:
# 	@../hledger/tools/dayssincerelease.hs | head -1 | cut -d' ' -f-1
# 	@echo

# showunreleasedchangecount:
# 	@echo Commits since last release:
# 	@darcs changes --from-tag . --count
# 	@echo

# showreleaseauthors:
# 	@echo Patch authors since last release:
# 	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq
# 	@echo

# showloc:
# 	@echo Current lines of code including tests:
# 	@sloccount `find . -name '*hs'` | grep haskell:
# 	@echo

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

# showunpushedchanges unpushed:
# 	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
# 	@-darcs push --dry-run | grep '*' | tac
# 	@echo

# showunreleasedcodechanges unreleased:
# 	@echo "code changes since last release:"
# 	@darcs changes --from-tag . --matches "not (name docs: or name doc: or name site: or name tools:)" | egrep '^  \* '
# 	@echo

# showcodechanges:
# 	@echo "code changes:"
# 	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
# 	@echo

######################################################################
# MISC


# TAGCMD=hasktags -e

# tag: TAGS

# TAGFILES=*.hs Graphics/UI/*.hs Graphics/UI/*/*.hs *.cabal *.md Makefile #tests/*.test 
# TAGS: $(TAGFILES)
# 	$(TAGCMD) $(TAGFILES)


# clean:
# 	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

# Clean: clean
# 	rm -f TAGS
# 	stack clean
