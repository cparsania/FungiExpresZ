FROM rocker/r-apt:bionic

RUN apt-get update && \
  apt-get install libcurl4-openssl-dev libv8-3.14-dev  libssl-dev libxml2-dev libxslt-dev libudunits2-dev libproj-dev libgdal-dev libfontconfig1-dev libcairo2-dev libgmp3-dev -y

# install renv on docker  

ENV RENV_VERSION 0.9.2
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


## create install dir
WORKDIR /FungiExpresZ
COPY renv.lock renv.lock
RUN R -e 'renv::consent(provided = TRUE)'
RUN R -e 'renv::restore()'

#copy tar.gz file  
COPY FungiExpresZ_1.2.0.tar.gz /FungiExpresZ

#install devtools 
RUN R -e 'install.packages("devtools")'

# install FungiExpresZ
RUN R -e 'devtools::install_local("/FungiExpresZ/FungiExpresZ_1.2.0.tar.gz" , dependencies=FALSE,  build = FALSE)'

EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(shinyBS);FungiExpresZ::run_app()"


