EMACS  = emacs25
CFLAGS = -std=c99 -Wall -Wextra -O3 -g3

all : joymacs.so joydemo.elc

joymacs.so : joymacs.c
	$(CC) -shared -fpic $(LDFLAGS) $(CFLAGS) -o $@ $^ $(LDFLAGS)

joydemo.elc : joydemo.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $^

run : joydemo.elc joymacs.so
	$(EMACS) -Q -L . -l $< -f joydemo

clean :
	$(RM) joymacs.so
