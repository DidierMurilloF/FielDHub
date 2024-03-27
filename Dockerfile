FROM rstudio/r-base:4.1.1-focal

LABEL maintainer "Didier Murillo <didier.murilloflorez@ndsu.edu>"

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libv8-dev \
    libpq-dev \
    libudunits2-dev \
    libgdal-dev \
    gdal-bin \
    libproj-dev \
    proj-data \
    proj-bin \
    libgeos-dev \
    git \
    curl

RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"))' > /root/.Rprofile

# basic shiny functionality
RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# install remotes
RUN R -e 'install.packages("remotes")'

# install dependencies of the FielDHub app
RUN R -q -e "install.packages('config')"
RUN R -q -e "install.packages('golem')"
RUN R -q -e "install.packages('htmltools')"
RUN R -q -e "install.packages('DT')"
RUN R -q -e "install.packages('shinythemes')"
RUN R -q -e "install.packages('dplyr')"
RUN R -q -e "install.packages('numbers')"
RUN R -q -e "install.packages('blocksdesign')"
RUN R -q -e "install.packages('shinycssloaders')"
RUN R -q -e "install.packages('ggplot2')"
RUN R -q -e "install.packages('plotly')"
RUN R -q -e "install.packages('viridis')"
RUN R -q -e "install.packages('shinyalert')"
RUN R -q -e "install.packages('desplot')"
RUN R -q -e "install.packages('shinyjs')"

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');FielDHub::run_app()"
