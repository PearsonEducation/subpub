###-*-makefile-*-   ; force emacs to enter makefile-mode

######################################################################
## C

CC      	:= gcc
CFLAGS  	:= -g -O2 -DPACKAGE_NAME=\"sgte\" -DPACKAGE_TARNAME=\"sgte\" -DPACKAGE_VERSION=\"0.7\" -DPACKAGE_STRING=\"sgte\ 0.7\" -DPACKAGE_BUGREPORT=\"pacini@sgconsulting.it\" -DPACKAGE_URL=\"\"
LD_SHARED       := ld -shared
######################################################################


PREFIX             = /usr/local
prefix             = ${PREFIX}
ETCDIR             = ${prefix}/etc
VARDIR             = ${prefix}/var

ERL=/usr/bin/erl
ERLC=/usr/bin/erlc

ERLDIR=/opt/erlang-R15B/lib/erlang
ERL_C_INCLUDE_DIR=$(ERLDIR)/usr/include

EMULATOR=beam
ifdef debug
  ERLC_FLAGS+=-Ddebug
endif

ifdef trace
  ERLC_FLAGS=+trace
endif

ifdef export_all
  ERLC_FLAGS+=-Dexport_all
endif

# Hmm, don't know if you are supposed to like this better... ;-)
APPSCRIPT = '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'

# Targets

../ebin/%.app: %.app.src ../vsn.mk Makefile
	perl -e $(APPSCRIPT) "$(VSN)" $(MODULES) < $< > $@

../ebin/%.appup: %.appup 
	cp $< $@

../ebin/%.$(EMULATOR): %.erl $(wildcard $(YAWSDIR)/lib/*/*/*.hrl)
	"$(ERLC)" $(ERLC_FLAGS) -o ../ebin $<


