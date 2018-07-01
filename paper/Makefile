all: main.pdf

main.pdf: main.tex sw.bib nanomaly.bib
	latexmk -pdf main.tex

clean:
	$(RM) main.{aux,bbl,blg,dvi,fls,log,pdf} 
