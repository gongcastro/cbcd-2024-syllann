all:
	Rscript -e "R/figures.R"
	quarto render
	quarto publish