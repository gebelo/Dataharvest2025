# Pre-built Dev Container Image for R. More info: https://github.com/rocker-org/devcontainer-images/pkgs/container/devcontainer%2Ftidyverse
# Available R version: 4, 4.1, 4.0
ARG VARIANT="4.4.3"
FROM ghcr.io/rocker-org/devcontainer/geospatial:${VARIANT}

RUN install2.r --error --skipinstalled -n -1 \
        RColorBrewer\
        formattable\
        corrplot\
        GGally\
        tigris\
        ggridges\
        
    && rm -rf /tmp/downloaded_packages 
#    && R -q -e 'remotes::install_github("https://github.com/dcomtois/summarytools/tree/0-8-9")'
