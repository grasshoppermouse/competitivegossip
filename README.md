# competitivegossip

[![DOI](https://zenodo.org/badge/)](https://zenodo.org/badge/latestdoi/)

This repository contains all code necessary to reproduce:

[*The impact of resource value, resource contestedness, and coalitions on gossiping*](https://grasshoppermouse.github.io/competitivegossip/). Nicole H. Hess and Edward H. Hagen

Instructions:

1. Clone this repository
2. Open the project in RStudio or `cd` into the directory and launch `R`. This will automatically bootstrap [`renv`](https://rstudio.github.io/renv/index.html).
3. After the bootstrapping process, enter the following in the console: `renv::restore()`. This should install all the necessary packages, including the separate data package [`gossipdata2008`](https://github.com/grasshoppermouse/gossipdata2008), in an isolated project-specific library.
4. knit the `paper.Rmd` file using the RStudio GUI or with `rmarkdown::render('paper.Rmd')`. This will generate the preprint file [`paper.html`](https://grasshoppermouse.github.io/competitivegossip/), which will display in the RStudio Viewer or can be viewed in any web browser. (Note: if not using RStudio, you will need a recent version of [pandoc](https://pandoc.org) installed.)

Note: Analyses used "R version 3.6.3 (2020-02-29)". You might need to install this version of R to reproduce them.