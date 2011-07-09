
# configuration

STEM=val_schorre_metaII

# default target
default: all
.PHONY: default

$(STEM).pdf: $(STEM).tex
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
