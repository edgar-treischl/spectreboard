##############################
# Stage 1: Build
##############################
FROM ghcr.io/edgar-treischl/shinyserver:latest as builder

USER root
WORKDIR /build

# Copy all app code and configuration files
COPY . /build

# Install shiny + dependencies with renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
 && R -e "renv::restore(confirm = FALSE)"

##############################
# Stage 2: Runtime - Minimal
##############################
FROM rocker/r-base:4.4.0

ENV DEBIAN_FRONTEND=noninteractive

# Install only runtime dependencies (R is already included)
RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    apache2-utils \
    locales \
    libfontconfig1 \
    libxt6 \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && rm -rf /var/lib/apt/lists/*

# Add a non-root user
RUN useradd -m shiny
USER shiny

# Set working directory
WORKDIR /app

# Copy built app and packages from builder
COPY --from=builder /build /app

# Copy Shiny Server binary and config from builder
COPY --from=builder /usr/bin/shiny-server /usr/bin/shiny-server
COPY --from=builder /etc/shiny-server /etc/shiny-server

# Copy nginx config and .htpasswd (from builder or host)
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd

# Prepare nginx dirs and set ownership
USER root
RUN mkdir -p /var/lib/nginx/body /var/log/nginx /run \
 && chown -R shiny:shiny /var/lib/nginx /var/log/nginx /run \
 && chmod +x /app/start.sh \
 && chown -R shiny:shiny /app

USER shiny

EXPOSE 80
CMD ["/app/start.sh"]
