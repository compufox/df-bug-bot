define LISP_CMDS
"(handler-case                    \
    (progn (ql:quickload :df-bug-bot) \
           (asdf:make :df-bug-bot))   \
  (error (e)                      \
    (format t \"~A~%\" e)         \
    (uiop:quit 1)))"
endef

.PHONY: clean all

all:
	ros --eval $(LISP_CMDS)

clean: 
	rm -ri bin/