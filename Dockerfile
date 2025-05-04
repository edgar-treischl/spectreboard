# Really
# Use the official R Shiny image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Copy the 'spectr' package tarball into the container
COPY ./spectr_0.0.0.1.tar.gz /srv/shiny-server/

# Install the 'spectr' package from the tarball
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_local('/srv/shiny-server/spectr_0.0.0.1.tar.gz')"


# Install R packages
RUN R -e "install.packages('pak', repos='https://cloud.r-project.org/'); \
          pak::pkg_install(c('shiny', 'bslib', 'dplyr', 'ggplot2', 'gt', 'tidyr', 'sysfonts', 'devtools', 'shinycssloaders', 'emoji', 'showtext'));"


# Create a directory for your specific app
RUN mkdir -p /srv/shiny-server/spectre/
# RUN mkdir -p /srv/shiny-server/myapp/www/
# RUN mkdir -p /srv/shiny-server/myapp/data/

# Copy your application files into the app directory
COPY R/ /srv/shiny-server/spectre/
COPY pointers/ /srv/shiny-server/spectre/pointers/
COPY validation/ /srv/shiny-server/spectre/validation/


# Set proper permissions for the shiny to run the app
#RUN chown -R shiny:shiny /srv/shiny-server

# Switch to non-root user before starting Shiny Server
#USER shiny

# Make all app files readable by the shiny user
RUN chmod -R 755 /srv/shiny-server/

# Expose port 3838 (default for Shiny Server)
EXPOSE 3838

# Run App
CMD ["/usr/bin/shiny-server"]
