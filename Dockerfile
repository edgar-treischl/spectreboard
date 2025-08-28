# Build spectrapp on top of your prebuilt base image
FROM ghcr.io/edgar-treischl/shinyserver:latest

# Switch to root to install app-specific packages and configs
USER root

# Set working directory
WORKDIR /app

# Copy app code, nginx config, htpasswd file, and startup script
COPY . /app
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd
COPY start.sh /start.sh

# Ensure start.sh is executable and install R dependencies
RUN chmod +x /start.sh \
  && R -e "renv::restore(confirm = FALSE)" \
  && R -e "install.packages('markdown', repos = 'https://cloud.r-project.org')" \
  && chown -R shinyuser:shinyuser /app

# Switch back to non-root user
USER shinyuser

# Expose nginx port
EXPOSE 80

# Start both Shiny and Nginx
CMD ["/start.sh"]
