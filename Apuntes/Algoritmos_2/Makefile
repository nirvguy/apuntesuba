PDF:=pdflatex
OBJ:=Apuntes_Algoritmos_2

.PHONY: clean rm-aux

all: $(OBJ).pdf rm-aux

%.pdf: %.tex
	$(PDF) $<

rm-aux: $(OBJ).pdf
	rm -rf $(OBJ).aux $(OBJ).toc $(OBJ).log

clean:
	rm -rf $(OBJ).aux $(OBJ).pdf $(OBJ).toc $(OBJ).log
