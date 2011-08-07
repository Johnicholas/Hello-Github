
# configuration

STEM=val_schorre_metaII
HELPERS=example_in_valgol_2.txt \
	valgol_2_in_meta_2.txt \
	meta_2_in_meta_2.txt \
	valgol_1_in_meta_2.txt

# default target
default: all
.PHONY: default

$(STEM).pdf: $(STEM).tex $(HELPERS)
	latex $(STEM).tex
	latex $(STEM).tex
	latex $(STEM).tex
	dvips $(STEM).dvi
	ps2pdf $(STEM).ps

# convenience targets

all: $(STEM).pdf
.PHONY: all

clean:
	rm -f $(STEM).aux
	rm -f $(STEM).log
	rm -f $(STEM).ps
	rm -f $(STEM).dvi
	rm -f $(STEM).pdf
	rm -f $(STEM).toc
	rm -f *~ # emacs backup files
.PHONY: clean
