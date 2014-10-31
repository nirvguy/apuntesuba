PDF:=pdflatex
OBJ:=Apuntes_Analisis_2.pdf Apuntes_Final_Analisis_2.pdf

.PHONY: clean rm-aux

all: $(OBJ)

%.pdf: %.tex
	$(PDF) $<

rm-aux: $(OBJ).pdf
	rm -rf $(OBJ).aux $(OBJ).toc $(OBJ).log

clean:
	rm -rf $(OBJ).aux $(OBJ).pdf $(OBJ).toc $(OBJ).log
