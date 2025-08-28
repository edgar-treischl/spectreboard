##############################
# Stage 1: Build (full deps + app)
##############################
FROM ghcr.io/edgar-treischl/shinyserver:latest as builder

USER root
WORKDIR /build

# Copy all app code + configs + start scripts
COPY . /build

# Install renv and restore app packages (including shiny)
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" \
 && R -e "renv::restore(confirm = FALSE)"

##############################
# Stage 2: Runtime (minimal with shinyserver + system deps)
##############################
FROM ghcr.io/edgar-treischl/shinyserver:latest

# Non-root user from base image
USER root

# Copy app code
COPY --from=builder /build /srv/shiny-server/app

# Copy renv library (where packages are installed)
COPY --from=builder /build/.renv /srv/shiny-server/app/.renv
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy Shiny Server configs if needed
COPY --from=builder /etc/shiny-server /etc/shiny-server

# Copy nginx config and .htpasswd
COPY nginx/nginx.conf /etc/nginx/nginx.conf
COPY nginx/.htpasswd /etc/nginx/.htpasswd

# Copy start.sh and make executable
COPY start.sh /start.sh
RUN chmod +x /start.sh && chown -R shinyuser:shinyuser /srv/shiny-server/app /etc/shiny-server

# Set user and working dir
USER shinyuser
WORKDIR /srv/shiny-server/app

EXPOSE 3838 80

CMD ["/start.sh"]
