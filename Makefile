.PHONY: all clean compile test xref

PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
ERLANG_BIN=$(shell dirname $(shell which erl))
REBAR=./rebar
VERSION_NO=$(if $(shell ls version),$(shell cat version))
COMMIT_NO:=$(shell git log -n 1 --oneline | cut -f 1 -d ' ')
PKG_NAME:=subpub-v.$(VERSION_NO)-$(COMMIT_NO)
DIR_NAME:=dist/$(PKG_NAME)

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably fail"))
$(if $(VERSION_NO),,$(warning "Warning: No version file found, package names may be silly looking"))

all:	compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit

package:
	rm -rfv $(DIR_NAME)
	mkdir -p $(DIR_NAME)
	cp -v -R src priv sbin include ebin deps deploy misc $(DIR_NAME)/
	cp -v regexes.config bshell logrotate.conf version clean compile dev.config LICENSE README.md rebar rebar.config shell Makefile $(DIR_NAME)/
	tar -czf $(PKG_NAME).tar.gz -C $(DIR_NAME) .
	mv $(PKG_NAME).tar.gz dist/
