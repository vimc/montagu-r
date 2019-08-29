FROM rocker/r-ver:3.5.2

RUN apt-get update && apt-get -y install \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    qpdf \
    texinfo \
    wget

# Install Pandoc
ENV PANDOC_VERSION "2.7"
RUN wget https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/pandoc-${PANDOC_VERSION}-1-amd64.deb \
  && dpkg --install pandoc-${PANDOC_VERSION}-1-amd64.deb \
  && rm pandoc-${PANDOC_VERSION}-1-amd64.deb

COPY docker/bin /usr/local/bin/

RUN install_packages --repo "https://vimc.github.io/drat" \
    R6 \
    getPass \
    httr \
    jsonlite \
    knitr \
    openssl \
    orderly \
    orderlyweb \
    pkgdown \
    progress \
    rmarkdown \
    storr \
    sys \
    testthat \
    tinytex \
    withr

RUN Rscript -e "tinytex::install_tinytex()"
ENV PATH="/root/bin:$PATH"

COPY . /montagu
