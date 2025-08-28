# ------------------------------------------------------
# Stage 1: Build dependencies and app using full image
# ------------------------------------------------------
FROM ghcr.io/edgar-treischl/shinyserver:latest AS build

USER root
WORKDIR /build

# Copy app code
COPY . /build

# Install R packages via renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
 && R -e "renv::restore(confirm = FALSE)" \
 && R -e "install.packages('markdown', repos = 'https://cloud.r-project.org')"

# ------------------------------------------------------
# Stage 2: Final, slim runtime image
# ------------------------------------------------------
FROM rocker/shiny:4.4.0 AS runtime

# Add nginx and apache2-utils (if needed)
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    apache2-utils \
 && rm -rf /var/lib/apt/lists/*

# Create shinyuser and prepare dirs
RUN useradd -m -s /bin/bash shinyuser \
 && mkdir -p /var/lib/nginx/body /var/log/nginx /run \
 && chown -R shinyuser:shinyuser /var/lib/nginx /var/log/nginx /run

WORKDIR /app

# Copy only what’s needed from the build image
COPY --from=build /build /app
COPY --from=build /build/renv /app/renv
COPY --from=build /build/renv.lock /app/renv.lock

# Copy configs and start script
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd
COPY start.sh /start.sh

# Make script executable and fix permissions
RUN chmod +x /start.sh \
 && chown -R shinyuser:shinyuser /app

USER shinyuser

EXPOSE 80

CMD ["/start.sh"]
