
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FungiExpresZ

[![](https://www.r-pkg.org/badges/version/FungiExpresZ?color=green)](https://cran.r-project.org/package=FungiExpresZ)
[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-orange.svg)](https://github.com/cparsania/FungiExpresZ)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->

FungiExpresZ is a R-shiny package to analyse and visualise fungal gene
expression data.

### Key features

#### 12 different data exploratory visualizations

Browser based user friendly interface, which allow user to generate
<<<<<<< HEAD
**ggplot2** based 12 different publication-ready elegant visualizations.
=======
**ggplot2** based 12 different publication ready elegant visualizations.
>>>>>>> master

1.  Scatter Plot  
2.  Multi-Scatter Plot  
3.  Corr Heat Box
4.  Density Plot
5.  Histogram
6.  Joy Plot
7.  Box Plot
8.  Violin Plot
9.  Bar Plot
10. PCA Plot
11. Line
Plot  
12. Heatmap

#### Gene Ontology (GO) analysis and visualizations for the gene clusters interactively selected from plots.

1.  Emap plot
2.  Cnet plot
3.  Dot plot
4.  Bar plot
5.  Heat plot
6.  Upset
plot

#### Ready to visualize preprocessed normalised gene expression values (FPKM) for XXX SRA data from XXX different fungal species.

// TO DO

#### Gene groups and sample groups visualizations

// TO DO

### How to use ?

  - It has been hosted on shinyapps.io and can be accessed via
    [link](http://cparsania.shinyapps.io/fungiexpresz).

  - It can also be installed locally as an R package. Follow instruction
    below to install locally.

### Install

##### Prerequisites

  - `R version (>= 3.6.1)`

##### Install pre-installation dependency

  - `install.packages("devtools")`
  - `install.packages("remotes")`
  - `install.packages("BiocManager")`

##### Set repos to download and install FungiExpresZ dependencies

  - `options(repos = BiocManager::repositories())`

##### Install from pre-compiled binray package

  - Download [precompiled binary]()
  - `devtools::install_local("path/to/.tgz file" , dependencies=TRUE,
    build = FALSE , repos=BiocManager::repositories())`

##### Install development version

  - `remotes::install_github("cparsania/FungiExpresZ" ,build = FALSE)`

### Run

  - Once installtion complete successfully, it can be run using
    `FungiExpresZ::run_app()`
  - Copy and paste url printed on the console to browser and you are
    ready to go.
