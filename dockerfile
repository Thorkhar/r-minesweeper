FROM rocker/r-ver:4.5.2

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev

RUN R -e "install.packages(c('shiny', 'R6', 'dplyr', 'jsonlite', 'ggplot2'))"

COPY / /

EXPOSE 8100

ENTRYPOINT ["Rscript", "src/main.R"]