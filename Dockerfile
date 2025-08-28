##############################
# Stage 1: Build
##############################
FROM ghcr.io/edgar-treischl/shinyserver:latest as builder

USER root
WORKDIR /build

# Copy all app code and configuration files
COPY . /build

# Restore R packages using renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
 && R -e "renv::restore(confirm = FALSE)"

##############################
# Stage 2: Runtime - Minimal
##############################
FROM debian:bullseye-slim as runtime

ENV DEBIAN_FRONTEND=noninteractive

# Install only required runtime libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    sed \
    libblas3 \
    libcurl4 \
    libssl1.1 \
    libxml2 \
    libpng16-16 \
    libjpeg62-turbo \
    libxt6 \
    ca-certificates \
    nginx \
    apache2-utils \
    locales \
    libfontconfig1 \
 && ln -s /bin/sed /usr/bin/sed || true \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && rm -rf /var/lib/apt/lists/*


ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Create a user for running the app
RUN useradd -m -s /bin/bash shiny

# Set working directory
WORKDIR /app

# Copy your app from the builder
COPY --from=builder /build /app

# Fix permissions and make start script executable
RUN chmod +x /app/start.sh \
 && chown -R shiny:shiny /app

# Copy necessary R components from the builder
COPY --from=builder /usr/local/lib/R /usr/local/lib/R
COPY --from=builder /usr/local/bin/R /usr/local/bin/R
COPY --from=builder /usr/local/bin/Rscript /usr/local/bin/Rscript
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=builder /usr/local/lib/R/library /usr/local/lib/R/library
COPY --from=builder /usr/local/lib/R/modules /usr/local/lib/R/modules
COPY --from=builder /usr/local/lib/R/etc /usr/local/lib/R/etc
COPY --from=builder /usr/local/lib/R/share /usr/local/lib/R/share

# Copy Shiny Server binary and config from builder
COPY --from=builder /usr/bin/shiny-server /usr/bin/shiny-server
COPY --from=builder /etc/shiny-server /etc/shiny-server

# Copy nginx config and .htpasswd (from builder)
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd

# Prepare nginx dirs and set ownership
RUN mkdir -p /var/lib/nginx/body /var/log/nginx /run \
 && chown -R shiny:shiny /var/lib/nginx /var/log/nginx /run

# Switch to non-root user
USER shiny

# Expose app port
EXPOSE 80

# Run your custom startup script
CMD ["/app/start.sh"]
