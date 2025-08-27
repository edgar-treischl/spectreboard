# Use rocker image with R and Shiny Server preinstalled
FROM rocker/shiny:4.4.0

# Avoid interactive prompts during install
ENV DEBIAN_FRONTEND=noninteractive

# Install system libraries and nginx, create user, prepare nginx dirs, set permissions
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libpng-dev \
      libjpeg-dev \
      libxt-dev \
      pandoc \
      nginx \
      apache2-utils \
  && rm -rf /var/lib/apt/lists/* \
  && useradd -m -s /bin/bash shinyuser \
  && mkdir -p /var/lib/nginx/body /var/log/nginx /run \
  && chown -R shinyuser:shinyuser /var/lib/nginx /var/log/nginx /run

# Set working directory
WORKDIR /app

# Copy app code, renv.lock, nginx config, and htpasswd file
COPY . /app
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd
COPY start.sh /start.sh

# Ensure start.sh is executable, install R packages, and fix permissions
RUN chmod +x /start.sh \
  && R -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(confirm = FALSE)" \
  && R -e "install.packages('markdown', repos = 'https://cloud.r-project.org')" \
  && chown -R shinyuser:shinyuser /app

# Switch to non-root user
USER shinyuser

# Expose port 80 for nginx
EXPOSE 80

# Start both Shiny and nginx
CMD ["/start.sh"]
