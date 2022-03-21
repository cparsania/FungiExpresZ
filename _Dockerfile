FROM rocker/r-apt:bionic

RUN apt-get update && \
  apt-get install libcurl4-openssl-dev libv8-3.14-dev  libssl-dev libxml2-dev libxslt-dev libudunits2-dev libproj-dev libgdal-dev libfontconfig1-dev libcairo2-dev libgmp3-dev -y

COPY ./requirement_bin.txt .
RUN cat requirement_bin.txt | xargs apt-get install -y -qq

#COPY packrat/bundles/FungiExpresZ-2019-11-12.tar.gz /

COPY FungiExpresZ_1.0.0.tar.gz /

## Download and install library
RUN R -e "install.packages(c('BiocManager','packrat'), dependencies = TRUE, repos=c(rstudiopm = 'https://demo.rstudiopm.com/all/__linux__/bionic/latest',BiocManager::repositories()))"

## create unbundle dir
RUN mkdir ./install

#RUN R -e 'options(repos=c(rstudiopm="https://demo.rstudiopm.com/all/__linux__/bionic/latest",BiocManager::repositories()));packrat::unbundle("/FungiExpresZ-2019-11-12.tar.gz",where = "./install",restore =TRUE)'

RUN R -e 'options(repos=c(rstudiopm="https://demo.rstudiopm.com/all/__linux__/bionic/latest",BiocManager::repositories()));packrat::unbundle("/FungiExpresZ_1.0.0.tar.gz",where = "./install",restore =TRUE)'

EXPOSE 80
CMD R -e "setwd('./install/FungiExpresZ/');packrat::on();options('shiny.port'=80,shiny.host='0.0.0.0');library(shinyBS);golem::document_and_reload();FungiExpresZ::run_app()"

