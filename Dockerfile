# Use a base R image with Shiny pre-installed
FROM rocker/r-ver:4.4

# Set environment variables to avoid prompts
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libpng-dev \
        libjpeg-dev \
        libxt-dev && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy app files into container
COPY . /app

# Install renv + restore project packages
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(confirm = FALSE)"

# Create and switch to non-root user (shiny)
RUN useradd -m -s /bin/bash shiny && \
    chown -R shiny:shiny /app

USER shiny

# Expose port for Shiny app
EXPOSE 3838

# Start Shiny app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
