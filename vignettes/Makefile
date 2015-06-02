paper.pdf: paper.Rnw
	Rscript -e "library(knitr); knit2pdf('paper.Rnw')"

basic.html: basic.Rnw
	Rscript -e "library('rmarkdown'); render('basic.Rmd')"

clean:
	rm -Rf *.log *.aux *~ *.tpt texput.log .#*
