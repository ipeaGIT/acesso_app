FROM docker-registry.ipea.gov.br/shiny/atlasacessibilidade-base:latest
COPY ./atlasacessibilidade /srv/shiny-server
WORKDIR /srv/shiny-server/data
RUN unzip -o acess_app_data_2019.zip && rm acess_app_data_2019.zip