.POSIX:
EMACS   = emacs
CC      = cc
CFLAGS  = -std=c99 -Os -Wall -Wextra
LDFLAGS = -s
LDLIBS  = $(shell $(EMACS) --batch --eval \
	      "(if (eq system-type 'windows-nt) (princ \"-lxinput1_3\"))")
SUFFIX  = $(shell $(EMACS) --batch --eval '(princ module-file-suffix)')

all: joymacs$(SUFFIX) joydemo.elc

joymacs$(SUFFIX): joymacs.c
	$(CC) -shared -fPIC $(CFLAGS) $(LDFLAGS) -o $@ joymacs.c $(LDLIBS)

joydemo.elc: joydemo.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile joydemo.el

run: joydemo.elc joymacs$(SUFFIX)
	$(EMACS) -Q -L . -l joydemo.elc -f joydemo

clean:
	rm -f joymacs$(SUFFIX) joydemo.elc
