PREFIX:=sequencereference
FILE:=$(PREFIX).tex

export TEXINPUTS:=${TEXINPUTS}:../../latex/

pdf: $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)
	pdflatex $(FILE)

clean:
	rm -f $(PREFIX).log
	rm -f $(PREFIX).toc
	rm -f $(PREFIX).aux
	rm -f $(PREFIX).out
	rm -f $(PREFIX).pdf
