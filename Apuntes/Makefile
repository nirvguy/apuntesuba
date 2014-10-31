MAKE:=make
SUBDIR:=Algebra_1 Algoritmos_1 Analisis_2 Probabilidad_y_Estadistica

.PHONY: carpetas clean

all: carpetas

carpetas:
	for dir in $(SUBDIR); do \
		$(MAKE) -C $$dir; \
	done

clean:
	for dir in $(SUBDIR); do \
		$(MAKE) clean -C $$dir; \
	done
