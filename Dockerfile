FROM rocker/shiny:4.4.1

ARG DEBIAN_FRONTEND=noninteractive
ARG HTTP_PROXY
ARG HTTPS_PROXY
ARG NO_PROXY
ENV http_proxy=${HTTP_PROXY} https_proxy=${HTTPS_PROXY} no_proxy=${NO_PROXY}
ENV HTTP_PROXY=${HTTP_PROXY} HTTPS_PROXY=${HTTPS_PROXY} NO_PROXY=${NO_PROXY}

# System deps (add libwebp-dev to satisfy ragg: libwebpmux.so.3)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev \
    libharfbuzz-dev libfribidi-dev libwebp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/shiny-app/
COPY . /home/shiny-app/

# Install renv + BiocManager
RUN R -q -e "install.packages(c('renv','BiocManager'), repos='https://cloud.r-project.org')"

# Install the core Bioconductor packages outside renv (so we don't fight the lockfile)
RUN R -q -e "BiocManager::install( \
    c('BiocGenerics','BiocVersion','ComplexHeatmap','InteractiveComplexHeatmap','IRanges','S4Vectors'), \
    ask = FALSE, update = FALSE \
  )"

# Use renv for everything else, but exclude those Bioc packages
RUN R -q -e "renv::consent(provided = TRUE); \
             options(repos = BiocManager::repositories(), \
                     renv.config.install.staged = FALSE); \
             renv::restore(exclude = c('BiocGenerics','BiocVersion', \
                                       'ComplexHeatmap','InteractiveComplexHeatmap', \
                                       'IRanges','S4Vectors'))"

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', host='0.0.0.0', port=3838)"]
