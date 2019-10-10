
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FungiExpresZ

[![](https://www.r-pkg.org/badges/version/FungiExpresZ?color=green)](https://cran.r-project.org/package=FungiExpresZ)
[![](https://img.shields.io/badge/devel%20version-0.0.1.9000-orange.svg)](https://github.com/cparsania/FungiExpresZ)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->

FungiExpresZ is an R-shiny package to analyse and visualise fungal gene
expression data.

### Key features

#### 12 different data exploratory visualizations

Browser based user friendly interface, which allow user to generate
**ggplot2** based 12 different publication-ready elegant visualizations.

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

  - Access through <http://cparsania.shinyapps.io/fungiexpresz>. Using
    this approach requires active internet connection.

  - Run as a docker container on any platform, where [docker
    engine](https://docs.docker.com/docker-for-mac/install/) is
    installed. Once installed, it can be run locally without active
    internet.

  - Install as an R package. Once installed, it can be run locally
    without active internet.

#### Install docker image and run container (Highly recommended)

  - This approach is highly recommended as user does not need to worry
    about any dependency related stuff.

##### Install docker

// TO DO

##### Pull image

// TO DO

##### Run container

// TO DO

#### Install as an R package

##### Prerequisites

    R version (>= 3.6.1)

##### Install pre-installation dependency

``` r
   install.packages("devtools")
   install.packages("BiocManager")
   install.packages("data.table" , type = "binary") ## installing from source may cause error. 
```

##### Set repos to download and install FungiExpresZ dependencies

``` r

  options(repos = BiocManager::repositories())
```

##### Install from pre-compiled binray package **(Recommanded)**

Download precompiled binary of latest release [for MacOS (\*.tgz
file)](https://github.com/cparsania/FungiExpresZ/releases/) and install
it using command
below.

``` r
  devtools::install_local("path/to/.tgz file" , dependencies=TRUE,  build = FALSE , repos=BiocManager::repositories())
```

> ##### NOTE: Installing developmental version **is not supported** as **`install_github()`** does not allow downloading git-lfs files.

##### Run

  - Once installtion complete successfully, it can be run using
    `FungiExpresZ::run_app()`
  - Copy and paste url printed on the console to browser and you are
    ready to go.
