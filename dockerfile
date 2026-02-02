FROM rocker/r-ver:4.5.2
RUN apt-get update -qq
RUN R -e "install.packages(c('shiny', 'R6', 'dplyr', 'jsonlite', 'ggplot2', 'png'))"
COPY / /
ENTRYPOINT ["Rscript", "src/main.R"]