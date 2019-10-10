FROM rocker/r-apt:bionic

RUN apt-get update && \
  apt-get install libcurl4-openssl-dev libv8-3.14-dev  libssl-dev libxml2-dev libxslt-dev libudunits2-dev libproj-dev libgdal-dev libfontconfig1-dev libcairo2-dev -y

COPY ./requirement_bin.txt .
RUN cat requirement_bin.txt | xargs apt-get install -y -qq

COPY ./FungiExpresZ_0.0.1.tar.gz /app.tar.gz
RUN R -e 'devtools::install_local("/app.tar.gz" , dependencies=TRUE,  build = FALSE ,repos=BiocManager::repositories() )'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(shinyBS);FungiExpresZ::run_app()"

