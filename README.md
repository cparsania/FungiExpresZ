
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FungiExpresZ

[![](https://www.r-pkg.org/badges/version/FungiExpresZ?color=green)](https://cran.r-project.org/package=FungiExpresZ)
[![](https://img.shields.io/badge/devel%20version-0.0.1.9000-orange.svg)](https://github.com/cparsania/FungiExpresZ)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->

FungiExpresZ is an R-shiny package to analyse and visualise fungal gene
expression
data.

## 1\. Key features

#### 1.1 More than 13,000 [NCBI-SRA](https://www.ncbi.nlm.nih.gov/sra) data from 8 different fungal species.

FungiExpresZ provides normalized gene expression values (FPKM) for more
than 13,000 SRA samples. User can select one more data for
visualizations. Data can be searched based on species, genotype, strain
or free text which will be matched against several SRA columns.

| species                            | sra\_samples |
| :--------------------------------- | -----------: |
| *Aspergillus nidulans FGSC A4*     |          151 |
| *Candida albicans SC5314*          |          639 |
| *Saccharomyces cerevisiae*         |        11872 |
| *Aspergillus fumigatus Af293*      |          242 |
| *Aspergillus niger CBS 513.88*     |          253 |
| *Candida glabrata CBS 138*         |          126 |
| *Talaromyces marneffei ATCC 18224* |           26 |
| *Candida auris B8 441*             |           46 |

> NOTE: We are continuously processing fungal SRA data. This table will
> be updated as we add new
data.

#### 1.2 Users can visualize their own data with or without integration of selected SRA data

Users can analyze and visualize their own data by uploading .txt/.csv
file (columns are samples and rows are genes). Optionally, user data can
be integrated with selected SRA data for combined analysis and
visualization.

#### 1.3 Visualize multiple gene groups and sample groups in a single plot

Optionally, user can upload sample groups (e.g. replicates, control vs
treatmet, wild type vs deletion etc.) and multiple gene groups to
compare between them. Group information can be used across several plots
against fill and facet plot attributes.

#### 1.4 12 different data exploratory visualizations

FungiExpresZ provides browser based user friendly interface, which allow
users to generate **ggplot2** based 12 different publication-ready
elegant visualizations. Users are allowed to adjust several common plot
such as plot title, axis title, font size, plot theme, legend size,
legend position etc. and other plot specific attributes. Available plot
options are …

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

#### 1.5 Supports Gene Ontology (GO) enrichment and visualizations for more than 100 different fungal species

FungiExpresZ allow users to define geneset(s) directly from plot
(Scatter plot, Line plot and Heatmap) to perform gene ontology
enrichment and visualizations. Available GO visualizations are …

1.  Emap plot
2.  Cnet plot
3.  Dot plot
4.  Bar plot
5.  Heat plot
6.  Upset plot

## 2\. How to use ?

#### 2.1 Access through internet

FungiExpresZ has been hosted on shinyapps.io server, which can be
accessed through link : <http://cparsania.shinyapps.io/fungiexpresz>.
Due to limited resources, we recommand this to use only if you have
smaller data set (\< 10 MB file) or if you want to get a quick look from
few SRA samples (\<= 5 samples). Current setup allows 15 concurrent
users. Further, website performance may also affect by internet speed
and number of active users.

> NOTE: Website may disconnect from server if internet is not stable,
> increasing traffic or keeping it idle for 30 minutes. We strongly
> recommand using local version for stable analysis of large
dataset.

#### 2.2 Run locally as a container from docker image (Highly recommended)

This approach is highly recommended as user does not need to worry about
any dependency related stuff.

##### 2.2.1 Install docker desktop

Follow the instructions given below to install docker desktop on …

  - [Mac](https://docs.docker.com/docker-for-mac/install/)
  - [Windows](https://docs.docker.com/docker-for-windows/install/)
  - [Linux](https://docs.docker.com/install/linux/docker-ce/ubuntu/).

##### 2.2.2 Pull FungiExpresZ docker image to local computer

Before you pull the image make sure your docker desktop is running.
Next, open the terminal and type the command below.

    docker pull cparsania/fungiexpresz:<tagName>

Replace `<tagName>` with the version you want to download. For example,
command below will download the version `0.0.1`

    docker pull cparsania/fungiexpresz:0.0.1

Possible values for `<tagName>` can be obtained from
[here](https://hub.docker.com/r/cparsania/fungiexpresz/tags). We
recommand you to pull latest tag always.

##### 2.2.3 Run container

After getting the image on local computer, it can be run as a container.
The command below will open the port given as `<port_number>` on local
computer and launch the application on same.

    docker run -p 80:<port_number> cparsania/fungiexpresz:<tagName>

You can give valid TCP `<port_number>` which is not occupied by your
system (e.g. 3232, 3233, 5434, … etc.).

Successful launch will print standard `R` welcome message on terminal
with the final the line `http://0.0.0.0:80`.

##### 2.2.4 Run on browser, Finally ..\!\!

After launch, hitting one of these URLs `http://localhost:<port_number>`
or `http://127.0.0.1:<port_ _number>` or
`http://<your_ip_address>:<port_number>` should load the application on
browser.

Congrats..\!\! 🎉🎉🎉🎉 .Your application will keep running until you stop
container explicitly.

##### 2.2.5 How to stop container

Regardless of closing terminal window, your container will keep running.
You can stop container explicitly using below command on new terminal
window.

    ## get container id 
    
    docker ps
    
    docker stop <CONTAINER ID>

#### 3\. Install as an R package

##### 3.1 Prerequisites

    R version (>= 3.6.1)

##### 3.2 Install pre-installation dependency

``` r
   install.packages("devtools")
   install.packages("BiocManager")
   install.packages("data.table" , type = "binary") ## installing from source may cause error. 
```

##### 3.3 Set repos to download and install FungiExpresZ dependencies

``` r

  options(repos = BiocManager::repositories())
```

##### 3.4 Install from pre-compiled binray package **(Recommanded)**

Download pre-compiled binary of latest release [for MacOS (\*.tgz
file)](https://github.com/cparsania/FungiExpresZ/releases/) and install
it using command
below.

``` r
  devtools::install_local("path/to/.tgz file" , dependencies=TRUE,  build = FALSE , repos=BiocManager::repositories())
```

> **NOTE:** Installing developmental version using
> **`devtools::install_github()`** is not supported as
> **`devtools::install_github()`** does not allow downloading git-lfs
> files.

##### 3.5 Run

  - Once installation complete successfully, it can be run using
    `FungiExpresZ::run_app()`
  - Copy and paste URL printed on the console to browser and you are
    ready to go.
