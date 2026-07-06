# RIDS application image.
#
# R package installs use Posit Package Manager Linux binaries pinned to a
# snapshot date — the reproducibility mechanism for this image (equivalent
# in effect to a lockfile: same date, same versions, prebuilt binaries).
# Bump RIDS_CRAN_SNAPSHOT deliberately, then re-run the test suite.

FROM rocker/r-ver:4.4.3

ARG RIDS_CRAN_SNAPSHOT=2026-07-01

RUN apt-get update && apt-get install -y --no-install-recommends \
      libsodium-dev \
      libpq-dev \
      libssl-dev \
      libcurl4-openssl-dev \
      libxml2-dev \
      zlib1g-dev \
      libicu-dev \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "\
  options(repos = c(CRAN = sprintf('https://packagemanager.posit.co/cran/__linux__/jammy/%s', '${RIDS_CRAN_SNAPSHOT}')), timeout = 600); \
  install.packages(c( \
    'DBI', 'duckdb', 'RPostgres', 'sodium', 'digest', \
    'shiny', 'bs4Dash', 'waiter', 'shinyFeedback', 'shinyjs', \
    'reactable', 'DT', 'jsonlite', 'zip', 'scales', \
    'dplyr', 'tidyr', 'stringr', 'purrr', 'readr', 'openxlsx', 'later' \
  )); \
  missing <- setdiff(c('DBI','duckdb','RPostgres','sodium','digest','shiny','bs4Dash','waiter','shinyFeedback','shinyjs','reactable','DT','jsonlite','zip','scales','dplyr','tidyr','stringr','purrr','readr','openxlsx','later'), rownames(installed.packages())); \
  if (length(missing) > 0) stop('missing packages: ', paste(missing, collapse = ', '))"

RUN useradd --create-home --shell /bin/bash rids

COPY --chown=rids:rids . /app
WORKDIR /app

# Writable runtime dirs for duckdb/dev mode and file outputs; in postgres
# mode only uploads/outputs/logs are used. Mount volumes over these in
# production if the data must outlive the container.
RUN mkdir -p /app/data /app/uploads /app/outputs /app/logs \
    && chown -R rids:rids /app/data /app/uploads /app/outputs /app/logs

USER rids

ENV RIDS_APP_HOST=0.0.0.0 \
    RIDS_APP_PORT=3838 \
    RIDS_ICT_UPLOAD_DIR=/app/uploads \
    RIDS_EDGE_OUTPUT_DIR=/app/outputs \
    RIDS_APP_LOG_DIR=/app/logs

EXPOSE 3838

CMD ["Rscript", "docker/run_app.R"]
