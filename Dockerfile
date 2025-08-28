##############################
# Stage 1: Build
##############################
FROM rocker/shiny:4.4.0 AS builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libpng-dev \
      libjpeg-dev \
      libxt-dev \
      pandoc \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy app, renv.lock, configs, start script
COPY . /app

# Install renv and restore packages
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
 && R -e "renv::restore(confirm = FALSE)" \
 && R -e "install.packages('markdown', repos = 'https://cloud.r-project.org')"

##############################
# Stage 2: Runtime (minimal)
##############################
FROM rocker/shiny:4.4.0

ENV DEBIAN_FRONTEND=noninteractive

# Install runtime system dependencies only
RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    apache2-utils \
    locales \
  && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
  && locale-gen \
  && rm -rf /var/lib/apt/lists/*

# Create user and nginx directories with correct permissions
RUN useradd -m -s /bin/bash shinyuser \
  && mkdir -p /var/lib/nginx/body /var/log/nginx /run \
  && chown -R shinyuser:shinyuser /var/lib/nginx /var/log/nginx /run

WORKDIR /app

# Copy app files and renv lock from builder
COPY --from=builder /app /app

# Copy start script and configs from builder if needed (or copy from local)
COPY --from=builder /start.sh /start.sh
COPY --from=builder /etc/nginx/nginx.conf /etc/nginx/nginx.conf
COPY --from=builder /etc/nginx/.htpasswd /etc/nginx/.htpasswd

RUN chmod +x /start.sh \
  && chown -R shinyuser:shinyuser /app

USER shinyuser

EXPOSE 80

CMD ["/start.sh"]
