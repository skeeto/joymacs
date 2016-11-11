EMACS    = emacs25
MINGW_CC = x86_64-w64-mingw32-gcc
CFLAGS   = -std=c99 -s -Wall -Wextra -O3 -fpic

MODULE_SUFFIX := $(shell $(EMACS) -batch --eval '(princ module-file-suffix)')

all : joymacs.so joymacs.dll joydemo.elc
linux : joymacs.so joydemo.elc
windows : joymacs.dll joydemo.elc

joymacs.so : joymacs.c
	$(CC) -shared $(CFLAGS) -o $@ $^

joymacs.dll : joymacs.c
	$(MINGW_CC) -shared $(CFLAGS) -o $@ $^ -lxinput1_3

joydemo.elc : joydemo.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

run : joydemo.elc joymacs$(MODULE_SUFFIX)
	$(EMACS) -Q -L . -l $< -f joydemo

clean :
	$(RM) joymacs.so joymacs.dll joydemo.elc

.PHONY : clean all linux windows
