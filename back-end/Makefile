#change this appropriately
SML=/usr/local
ARCH=x86-darwin

# commands
BASENAME = basename
CUT      = cut
DIFFALL  = diff -r
ECHO     = printf
GREP     = egrep
MKDIR    = mkdir
RMALL    = rm -rf
WC       = wc


## !!! DO NOT CHANGE ANYTHING BELOW THIS, UNLESS YOU *KNOW* HOW
## !!! SPACES, TABS, AND ESCAPED NEWLINES WORK UNDER GNU-MAKE

compile :	
		$(SML)/bin/ml-build sources.cm Main.main

# test runs a set of tests on the testcases in ./tests
test :	sources.$(ARCH) testast testpup testesc testpec testuec testuect

# testast parses and dumps AST for a set of testcases
#     test/foo.tig  ---->   ast/foo.ast
#                   ---->   ast/foo.err
testast :	
		-$(RMALL) ./ast
		 $(MKDIR) ./ast
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
		        $(SML)/bin/sml @SMLload=./sources                           \
			 	        -p                                           \
				             $$f                                          \
				              > ./ast/`$(BASENAME) $$f .tig`.ast          \
				             2> ./ast/`$(BASENAME) $$f .tig`.err             ; \
		  done)                                                       ; \
		$(ECHO) "parse errors detected:"                              ; \
		$(GREP) "parse failed" ./ast/*.err 2>/dev/null                  \
			  | $(WC)                                                    \
			  | $(CUT) -c1-8


# testpup parses and pretty-prints a set of testcases
#      pup1 is the first go-around
#      pup2 uses pup1's output, and so should be stable
testpup : testpup1 testpup2

testpup1 :
		-$(RMALL) ./pup1
		 $(MKDIR) ./pup1
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
			$(SML)/bin/sml @SMLload=./sources                           \
			                -u                                           \
				             $$f                                          \
				              > ./pup1/`$(BASENAME) $$f`                  \
				             2> ./pup1/`$(BASENAME) $$f .tig`.err       ; \
		  done)                                                       ; \
		                                                                \
		$(ECHO) "Phase 1 parse errors detected:"                      ; \
		$(GREP) "parse failed" ./pup1/*.err 2>/dev/null                 \
			  | $(WC)                                                    \
			  | $(CUT) -c1-8

testpup2 :
		-$(RMALL) ./pup2
		 $(MKDIR) ./pup2
		-(for f in `$(GREP) -L "parse failed" ./pup1/*.err`           ; \
		  do                                                            \
			$(SML)/bin/sml @SMLload=./sources                           \
				        -u                                           \
				             ./pup1/`$(BASENAME) $$f .err`.tig            \
				              > ./pup2/`$(BASENAME) $$f .err`.tig         \
				             2> ./pup2/`$(BASENAME) $$f`                ; \
		  done)                                                       ; \
		                                                                \
		$(ECHO) "Phase 2 parse errors detected:"                      ; \
		$(GREP) "parse failed" ./pup2/*.err 2>/dev/null                 \
				| $(WC)                                                   \
				| $(CUT) -c1-8                                          ; \
																							 \
		$(DIFFALL) ./pup1 ./pup2                                        \
				| $(GREP) -v ".err"				\
				| $(GREP) -v DISCARDS				\
				| $(GREP) -v "^Only in pup1" > ./pup2/DISCARDS          ; \
		$(ECHO) "Phase 2 discarded files:"                            ; \
		$(WC) ./pup2/DISCARDS                                           \
				| $(CUT) -c1-8

# tests ast execution -- works just like testast
testexe:	
		-$(RMALL) ./exe
		 $(MKDIR) ./exe
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
			echo $$f                                              ; \
		        $(SML)/bin/sml @SMLload=./sources                           \
					-x                                        \
				             $$f                                          \
				              > ./exe/`$(BASENAME) $$f .tig`.out          \
				             2> ./exe/`$(BASENAME) $$f .tig`.err             ; \
		  done)                                                       ; \
		$(ECHO) "execution errors:"                              ; \
		$(GREP) "CRASH" ./exe/*.err 2>/dev/null                  \
			  | $(WC)                                                    \
			  | $(CUT) -c1-8

# tests escape analysis -- works just like testast
testesc:	
		-$(RMALL) ./esc
		 $(MKDIR) ./esc
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
		        $(SML)/bin/sml @SMLload=./sources                           \
					-p -e                                        \
				             $$f                                          \
				              > ./esc/`$(BASENAME) $$f .tig`.esc          \
				             2> ./esc/`$(BASENAME) $$f .tig`.err             ; \
		  done)                                                       ; \
		$(ECHO) "escape errors detected:"                              ; \
		$(GREP) "parse failed" ./esc/*.err 2>/dev/null                  \
			  | $(WC)                                                    \
			  | $(CUT) -c1-8

# tests type-checking (which requires escape analysis)
#                      and generates abstract syntax
testpec:	
		-$(RMALL) ./chk
		 $(MKDIR) ./chk
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
		        $(SML)/bin/sml @SMLload=./sources                           \
					-p e c                                      \
				             $$f                                          \
				              > ./chk/`$(BASENAME) $$f .tig`.chk          \
				             2> ./chk/`$(BASENAME) $$f .tig`.err             ; \
		  done)                                                       ; \
		$(ECHO) "type-checking errors detected:"                              ; \
		$(GREP) "type error" ./chk/*.err 2>/dev/null                  \
			  | $(WC)                                                    \
			  | $(CUT) -c1-8

# tests type-checking (which requires escape analysis)
#                      and generates intermediate representation
testuec:	
		-$(RMALL) ./chk
		 $(MKDIR) ./chk
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
		        $(SML)/bin/sml @SMLload=./sources                           \
					-u e c                                      \
				             $$f                                          \
				              > ./chk/`$(BASENAME) $$f .tig`.chk          \
				             2> ./chk/`$(BASENAME) $$f .tig`.err             ; \
		  done)                                                       ; \
		$(ECHO) "type-checking errors detected:"                              ; \
		$(GREP) "type error" ./chk/*.err 2>/dev/null                  \
			  | $(WC)                                                    \
			  | $(CUT) -c1-8

# tests trace-scheduling and generates intermediate representation
testuect:	
		-$(RMALL) ./chk
		 $(MKDIR) ./chk
		-(for f in ./tests/*.tig                                      ; \
		  do                                                            \
		        $(SML)/bin/sml @SMLload=./sources                           \
					-u -e -c -t                                 \
				             $$f                                          \
				              > ./chk/`$(BASENAME) $$f .tig`.chk          \
				             2> ./chk/`$(BASENAME) $$f .tig`.err             ; \
		  done)
		# nothing can be checked for ... you need to manually inspect

clean :
		-$(RMALL) sources.$(ARCH) ast pup1 pup2 esc chk exe *~ \
					 tiger.grm.* tiger.lex.* .cm
