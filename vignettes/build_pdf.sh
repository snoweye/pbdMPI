#!/bin/sh

PKGVER=`grep "Version:" ../DESCRIPTION | sed -e "s/Version: //"`
sed -i -e "s/demoversion}{[0-9][.][0-9]-[0-9]}/demoversion}{${PKGVER}}/" cover.tex


rm *.aux *.bbl *.blg *.log *.out *.toc
pdflatex pbdMPI-guide.Rnw
bibtex pbdMPI-guide
pdflatex pbdMPI-guide.Rnw
pdflatex pbdMPI-guide.Rnw
pdflatex pbdMPI-guide.Rnw
rm *.aux *.bbl *.blg *.log *.out *.toc

mv -f *.pdf ../inst/doc/
cp -f *.Rnw ../inst/doc/
