all: docx html pdf

docx: survey.docx
html: survey.html
pdf: survey.pdf

%.docx: %.Rmd
	#R -e "library(knitr);opts_chunk\$$set(dev='win.metafile');knit('$<', 'survey_docx.md')"
	R -e "library(knitr);opts_chunk\$$set(fig.width=6, fig.height=6, dpi=300, dev.antialias='cleartype');knit('$<', 'survey_docx.md')"
	pandoc survey_docx.md -o $@

%.html: %.Rmd
	R -e "library(knitr);knit2html('$<', '$@')"

%.pdf: %.Rmd
	R -e "library(knitr);opts_chunk\$$set(dev='pdf');knit('$<', 'survey_pdf.md')"
	pandoc survey_pdf.md -o $@

clean:
	rm -f *.md *.docx *.pdf *.html

