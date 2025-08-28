##############################
# Stage 1: Build
##############################
FROM ghcr.io/edgar-treischl/shinyserver:latest as builder

USER root
WORKDIR /build

COPY . /build

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" \
 && R -e "renv::restore(confirm = FALSE)"

##############################
# Stage 2: Runtime - Minimal
##############################
FROM debian:bullseye-slim as runtime

ENV DEBIAN_FRONTEND=noninteractive

# Install only required runtime libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4 \
    libssl1.1 \
    libxml2 \
    libpng16-16 \
    libjpeg62-turbo \
    libxt6 \
    ca-certificates \
    locales \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && rm -rf /var/lib/apt/lists/*

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Add user
RUN useradd -m shiny
USER shiny

WORKDIR /app

# Copy app files
COPY --from=builder /build /app

# Copy required R binaries and libraries
COPY --from=builder /usr/local/lib/R /usr/local/lib/R
COPY --from=builder /usr/local/bin/R /usr/local/bin/R
COPY --from=builder /usr/local/bin/Rscript /usr/local/bin/Rscript
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=builder /usr/local/lib/R/library /usr/local/lib/R/library
COPY --from=builder /usr/local/lib/R/modules /usr/local/lib/R/modules
COPY --from=builder /usr/local/lib/R/etc /usr/local/lib/R/etc
COPY --from=builder /usr/local/lib/R/share /usr/local/lib/R/share

# Expose Shiny port
EXPOSE 3838

# Set default CMD (you can change this)
CMD ["R", "-e", "shiny::runApp('/app')"]
