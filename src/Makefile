OCAMLC=ocamlc
OCAMLFIND=ocamlfind
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
SRCDIR=.
OBJDIR=../objects
INCLUDES=-I $(SRCDIR)
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLFIND_FLAGS=-linkpkg -package lwt,cohttp,cohttp.lwt,core,lpi -thread \
		$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

LIBLPI_OBJS= ast.cmo environment.cmo basis.cmo parser.cmo \
	     lexer.cmo staticsemantics.cmo interpret.cmo

LPI_OBJS= interpreter.cmo

LPI_SERVER_OBJS= main.cmo

build: 	$(LIBLPI_OBJS)
	$(OCAMLC) -a $(OCAMLFLAGS) unix.cma $(LIBLPI_OBJS) -o ../lpi.cma
	mv $(SRCDIR)/*.cm* $(OBJDIR)

lpi:	$(LPI_OBJS)
	$(OCAMLC) -o ../lpi-repl $(OCAMLFLAGS) ../lpi.cma $(LPI_OBJS)
	mv $(SRCDIR)/*.cm* $(OBJDIR)

server:
	$(OCAMLFIND) $(OCAMLC) -o ../lpi-server $(OCAMLFIND_FLAGS) $(LPI_SERVER_OBJS)
	mv $(SRCDIR)/*.cm* $(OBJDIR)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: $(SRCDIR)/%.ml
		 $(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmi: $(SRCDIR)/%.mli
		 $(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: $(SRCDIR)/%.ml
		 $(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.ml > $(OBJDIR)/.depend

install:
	$(OCAMLFIND) install lpi ../META ../lpi.cma ../lpi-repl \
	../objects/*.cm*

clean:
	rm -f ../lpi-repl ../lpi-server ../liblpi.cma
	rm -f $(OBJDIR)/*.cm*
	rm -f $(SRCDIR)/*.cm* $(SRCDIR)/*~
	rm -f $(SRCDIR)/parser.ml $(SRCDIR)/parser.mli $(SRCDIR)/lexer.ml

include $(OBJDIR)/.depend
