LISPS = ros sbcl clisp cmucl ccl
CMDS = --eval "(ql:quickload :df-bug-bot)" --eval "(asdf:make :df-bug-bot)" --eval "(quit)"


ifeq ($(OS),Windows_NT)
	LISP := $(foreach lisp,$(LISPS), \
		$(shell where $(lisp)) \
		$(if $(.SHELLSTATUS),$(strip $(lisp)),))
else
	LISP := $(foreach lisp,$(LISPS), \
		$(if $(findstring $(lisp),"$(shell which $(lisp) 2>/dev/null)"), $(strip $(lisp)),))
endif

ifeq ($(LISP),)
	$(error "No lisps found")
endif

all:
	$(LISP) $(CMDS)

clean:
	test -d bin && rm bin/df-bug-bot
