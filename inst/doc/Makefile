ROOT=maqcNotes
$(ROOT).dvi: $(ROOT).tex
	latex $(ROOT)

$(ROOT).pdf: $(ROOT).tex
	pdflatex $(ROOT).tex
#
#$(ROOT).pdf: $(ROOT).ps
#	ps2pdf $(ROOT).ps
#
#$(ROOT).ps: $(ROOT).dvi
#	dvips -o $(ROOT).ps $(ROOT)
#
#
$(ROOT).tex: $(ROOT).Rnw
	echo 'Sweave("$(ROOT).Rnw")' | Rdev --no-save

edit:
	vi $(ROOT).Rnw

$(ROOT)4.pdf: $(ROOT)4.tex
	latex $(ROOT)4.tex
	latex $(ROOT)4.tex
	dvips -Ppdf -tletter $(ROOT)4 -o xyzzy.ps
	psnup -4 -W128mm -H96mm -b1cm -pletter xyzzy.ps $(ROOT)4.ps
	ps2pdf $(ROOT)4.ps
