QUICKLISP_HOME = $(HOME)/quicklisp
SBCL = sbcl --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP = $(SBCL) --eval '(load "$(QUICKLISP_HOME)/setup.lisp")' --eval "(push '*default-pathname-defaults* asdf:*central-registry*)"
BUILDAPP = buildapp

all: meeting-calendar

clean:
	rm -f quicklisp-index.txt loaded-once.txt meeting-calendar

loaded-once.txt: *.lisp
	$(QUICKLISP) --eval '(ql:quickload "meeting-calendar")'
	touch loaded-once.txt

quicklisp-index.txt: loaded-once.txt
	$(QUICKLISP) --eval '(ql:write-asdf-manifest-file "quicklisp-index.txt")'

meeting-calendar: quicklisp-index.txt
	$(BUILDAPP) \
		--asdf-path . --manifest-file quicklisp-index.txt \
		--load-system meeting-calendar \
		--output meeting-calendar \
		--entry meeting-calendar::toplevel
