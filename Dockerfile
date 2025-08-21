# Use a base R image with Shiny pre-installed
FROM rocker/r-ver:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libjpeg-dev \
    libxt-dev \
    pandoc \
    pandoc-citeproc \
    && apt-get clean

# Install Shiny
RUN install2.r --error shiny

# Set working directory
WORKDIR /app

# Copy app files into container
COPY . /app

# Ensure shiny can access files
RUN chown -R shiny:shiny /app

# Switch to shiny user
USER shiny

# Install renv + restore project packages
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org'); renv::restore(confirm = FALSE)"

# Expose port
EXPOSE 3838

# Launch the Shiny app directly
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
