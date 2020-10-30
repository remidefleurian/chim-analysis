# ChiM analysis

[![doi](https://zenodo.org/badge/DOI/10.5281/TODO.svg)](https://doi.org/10.5281/TODO)
[![license](https://img.shields.io/github/license/remidefleurian/chim-analysis)](https://github.com/remidefleurian/chim-analysis/blob/master/LICENSE)

This R script can be used to replicate the methods and results presented in the following article, and can be cited as such:

> de Fleurian, R., & Pearce, M. T. (2020). The relationship between valence and chills in music: A corpus analysis. *PsyArXiv*. doi: [10.31234/osf.io/TODO](https://doi.org/10.31234/osf.io/TODO)

More specifically, since the data used in the article is proprietary and can therefore not be shared, this script is designed to download all the data required to replicate every step of the analysis presented in the article. Please note that data retrieved from the Spotify Web API changes slightly with time. As a consequence, a replication of the analysis might lead to slightly different results than those reported in the article.

## Setup

This script runs on [R](https://cloud.r-project.org/), and is best executed from [RStudio](https://rstudio.com/). You will also need a [Spotify for Developers](https://developer.spotify.com/dashboard/) account. Once this is done, edit `scripts/load-config.R` to add your Spotify credentials.

In the same file, you will also need to change `refresh_data` from `FALSE` to `TRUE`. Please note that running the script for the first time takes a considerable amount of time, due to the quantity of data that needs to be pulled from the Spotify Web API. Once this is done, we highly recommend changing `refresh_data` back to `FALSE` to skip this step.

## Instructions

To run the script, open `chim-analysis.Rproj` in RStudio, in order to prevent working directory issues. Then, simply open `main.R` and run the entire script. All the downloaded data, analysis results, and plots will be created in the `output/` folder.