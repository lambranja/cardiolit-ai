FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c( \
  'shiny','httr2','jsonlite','xml2','dplyr','purrr','stringr','tidyr', \
  'openxlsx','glue','tibble','shinythemes','shinyjs' \
), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD /usr/bin/shiny-server
