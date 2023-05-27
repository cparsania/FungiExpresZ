FROM rocker/r-base:4.2.2

RUN apt-get update && \
  apt-get install cmake libcurl4-openssl-dev libssl-dev libxml2-dev libxslt-dev libudunits2-dev libproj-dev libgdal-dev libfontconfig1-dev libcairo2-dev libgmp3-dev -y

# install renv on docker  

ENV RENV_VERSION 0.17.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

## create install dir
WORKDIR /FungiExpresZ
COPY renv.lock renv.lock
RUN R -e 'renv::consent(provided = TRUE)'
RUN R -e 'renv::restore()'

#install specific version of devtools. Latest version may not be compatible with older R version (v 3.6)
#RUN R -e 'remotes::install_version("devtools", version = "2.4.5")'
RUN apt-get install -y libharfbuzz-dev  libfribidi-dev
RUN R -e 'install.packages("devtools", dependencies = T)'

#copy tar.gz file  
COPY FungiExpresZ_2.0.0.tar.gz /FungiExpresZ

# install FungiExpresZ
RUN R -e 'devtools::install_local("/FungiExpresZ/FungiExpresZ_2.0.0.tar.gz" , dependencies=FALSE,  build = FALSE)'

EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(shinyBS);FungiExpresZ::run_app()"


