FROM docker-registry.ipea.gov.br/shiny/atlasacessibilidade-base:teste
COPY ./atlasacessibilidade /srv/shiny-server
WORKDIR /srv/shiny-server/data
RUN wget https://github.com/ipeaGIT/acesso_app/releases/download/v0.3/acess_app_data_2022.zip
RUN unzip -o acess_app_data_2019.zip && rm acess_app_data_2019.zip
RUN unzip -o acess_app_data_2022.zip && rm acess_app_data_2022.zip