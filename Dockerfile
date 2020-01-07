FROM rocker/shiny-verse:3.6.1
ENV http_proxy http://cache.ipea.gov.br:3128
ENV https_proxy http://cache.ipea.gov.br:3128
ENV HTTP_PROXY http://cache.ipea.gov.br:3128
ENV HTTPS_PROXY http://cache.ipea.gov.br:3128
RUN apt-get install -y \
        libv8-dev \
        libudunits2-dev \
        libgeos-dev \
        libgdal-dev
RUN install2.r --error \
        --deps TRUE \
        data.table \
        dplyr \
        mapdeck \
        RColorBrewer \
        readr \
        sf \
        shiny \
        shinyBS \
        shinyWidgets
COPY ./atlasacessibilidade /srv/shiny-server