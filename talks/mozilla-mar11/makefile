# makefile for research paper

TARGET=mozilla-talk
SOURCES=$(shell ls slides/*.tex) $(TARGET).tex
PDFVIEWER=open # change according to environment

.SUFFIXES:
.SUFFIXES: .tex .pdf .gv .acro .xpdf

all: $(TARGET).pdf

$(TARGET).pdf: $(SOURCES) makefile
	pdflatex $(TARGET).tex
	$(PDFVIEWER) $(TARGET).pdf # comment out if not on a Mac

clean:
	rm -f *~ $(TARGET).log $(TARGET).aux $(TARGET).out $(TARGET).bbl $(TARGET).blg $(TARGET).nav $(TARGET).snm $(TARGET).toc $(TARGET).pdf
