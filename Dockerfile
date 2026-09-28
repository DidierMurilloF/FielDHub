# Update the tag and digest together, then rebuild and review the dependency lock.
ARG BASE_IMAGE=rocker/r-ver:4.5.3@sha256:c3f39b365d1077fe24f8e9ab2742e352b6d3950897f51af1624a5bb5550c21c0
FROM ${BASE_IMAGE} AS base

LABEL org.opencontainers.image.source="https://github.com/DidierMurilloF/FielDHub"
LABEL org.opencontainers.image.authors="Didier Murillo <didier.murilloflorez@ndsu.edu>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl libcurl4-openssl-dev libssl-dev libxml2-dev \
    libicu-dev zlib1g-dev libgit2-dev pkg-config \
    && rm -rf /var/lib/apt/lists/*

ENV R_LIBS_USER=/opt/fieldhub/library \
    RENV_CONFIG_CACHE_ENABLED=false \
    RENV_CONFIG_SANDBOX_ENABLED=false \
    RENV_CONFIG_PAK_ENABLED=false

FROM base AS build
WORKDIR /opt/fieldhub
COPY deployment/renv.lock ./renv.lock
# Bootstrap is pinned independently; restore never installs a moving latest renv.
RUN curl --fail --location --retry 3 \
      https://cran.r-project.org/src/contrib/Archive/renv/renv_1.1.4.tar.gz \
      --output /tmp/renv.tar.gz \
    && printf '%s  %s\n' e81cfeaad56eed1959cc597c4229888527f46bdb783c843843675b8c656390d3 /tmp/renv.tar.gz | sha256sum --check - \
    && R CMD INSTALL /tmp/renv.tar.gz \
    && rm /tmp/renv.tar.gz
RUN Rscript --vanilla -e 'dir.create(Sys.getenv("R_LIBS_USER"), recursive=TRUE); renv::restore(project="/opt/fieldhub", lockfile="renv.lock", library=Sys.getenv("R_LIBS_USER"), prompt=FALSE)'
COPY . /src/FielDHub
RUN R CMD INSTALL --no-multiarch --library=/opt/fieldhub/library /src/FielDHub

FROM base AS runtime
RUN groupadd --gid 10001 fieldhub \
    && useradd --uid 10001 --gid 10001 --create-home --shell /usr/sbin/nologin fieldhub
WORKDIR /opt/fieldhub
COPY --from=build /opt/fieldhub/library ./library
COPY deployment/renv.lock ./renv.lock
COPY deployment/check-runtime.R ./check-runtime.R
USER 10001:10001
RUN Rscript --vanilla check-runtime.R
EXPOSE 3838
CMD ["Rscript", "--vanilla", "-e", "shiny::runApp(FielDHub::run_app(launch.browser=FALSE), host='0.0.0.0', port=3838L)"]
