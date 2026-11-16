
FROM rocker/shiny:4.4.3

# Dépendances Linux pour les bibliothèques R
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libnlopt-dev \
    libblas-dev \
    liblapack-dev \
    && rm -rf /var/lib/apt/lists/*

# Installation des bibliothèques R
RUN Rscript -e "install.packages(c( \
    'shiny', \
    'ggplot2', \
    'lme4', \
    'lmerTest', \
    'DT', \
    'readxl', \
    'openxlsx' \
    ), repos='https://cloud.r-project.org', Ncpus=2)"

# Vérification des bibliothèques installées
RUN Rscript -e "pkgs <- c('shiny','ggplot2','lme4','lmerTest','DT','readxl','openxlsx'); stopifnot(all(vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)))"

# Suppression des applications de démonstration
RUN rm -rf /srv/shiny-server/*

# Copie du code R avec le nom attendu par Shiny
COPY App.R /srv/shiny-server/app.R

# Vérification de la syntaxe
RUN Rscript -e "invisible(parse('/srv/shiny-server/app.R'))"

# Permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Port Shiny
EXPOSE 3838

# Démarrage
CMD ["/usr/bin/shiny-server"]
