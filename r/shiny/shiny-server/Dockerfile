# source: https://datawookie.dev/blog/2022/09/using-shiny-server-in-docker/

FROM rocker/shiny:4.1.0

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

# Copy the config file into the container
COPY shiny-server.conf /etc/shiny-server/

COPY ./app.R ./app.R
