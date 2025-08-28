##############################
# Stage 1: Build
##############################
FROM rocker/shiny:4.4.0 AS builder

USER root
WORKDIR /build

# Copy app source and config files
COPY . /build

# Install renv if missing and restore packages
RUN R -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::restore(confirm = FALSE)"

# Fix ownership (just in case)
RUN chown -R shiny:shiny /build

##############################
# Stage 2: Runtime
##############################
FROM rocker/shiny:4.4.0

ENV DEBIAN_FRONTEND=noninteractive

# Install nginx and apache2-utils for basic HTTP auth (adjust as needed)
RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    apache2-utils \
    libfontconfig1 \
    libxt6 \
  && rm -rf /var/lib/apt/lists/*

# Add nginx directories and fix permissions
RUN mkdir -p /var/lib/nginx/body /var/log/nginx /run \
  && chown -R shiny:shiny /var/lib/nginx /var/log/nginx /run

USER shiny
WORKDIR /srv/shiny-server/app

# Copy app files and restored renv packages from builder
COPY --from=builder /build /srv/shiny-server/app

USER root
# Copy nginx config and .htpasswd (adjust paths if needed)
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd

# Copy start script and make executable
COPY start.sh /start.sh
RUN chmod +x /start.sh \
  && chown shiny:shiny /start.sh

USER shiny

# Expose ports for Shiny Server (default 3838) and nginx (80)
EXPOSE 80

# Start your script which should launch Shiny Server and nginx
CMD ["/start.sh"]
