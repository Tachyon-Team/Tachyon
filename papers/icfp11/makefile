# makefile for research paper

PAPER=ChevalierBoisvertLavoieDufourFeeleyICFP11
SECTIONS=introduction.tex experiments.tex conclusion.tex
BIBLIO=biblio

.SUFFIXES:
.SUFFIXES: .tex .dvi .ps .pdf .gv .acro .xpdf

all: $(PAPER).pdf

$(PAPER).pdf: $(PAPER).tex $(BIBLIO).bib $(SECTIONS) makefile
	latex $(PAPER).tex
	bibtex $(PAPER)
	latex $(PAPER).tex
	dvips -tletter -Ppdf -o $(PAPER).ps $(PAPER).dvi
	ps2pdf $(PAPER).ps $(PAPER).pdf
	#open $(PAPER).pdf # comment out if not on a Mac

clean:
	rm -f *~ $(PAPER).log $(PAPER).aux $(PAPER).out $(PAPER).dvi $(PAPER).bbl $(PAPER).blg $(PAPER).ps $(PAPER).pdf
