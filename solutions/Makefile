SRC := Grimaud_Maps2
EXE := create_map
NODE := Node
EDGE := Edge

.PHONY: clean all

all: $(EXE)

$(EXE): $(SRC).ml $(NODE).cmi $(NODE).cmx $(EDGE).cmi $(EDGE).cmx
	ocamlfind ocamlopt -o $@ -linkpkg -package graphics $(NODE).cmx $(EDGE).cmx $<

# Compile the module, generate $(NODE).cmx
$(NODE).cmx: $(NODE).ml $(NODE).cmi 
	ocamlopt -c $<

# Compile the interface, generate $(NODE).cmi
$(NODE).cmi: $(NODE).mli
	ocamlopt -c $<

# Compile the module, generate $(EDGE).cmx
$(EDGE).cmx: $(EDGE).ml $(EDGE).cmi 
	ocamlopt -c $<

# Compile the interface, generate $(EDGE).cmi
$(EDGE).cmi: $(EDGE).mli
	ocamlopt -c $<

clean:
	rm *.cmi *.cmo *.cmx *.o $(EXE)
