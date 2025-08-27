#!/bin/sh

# Start nginx in the background
nginx

# Run shiny app (exposed to all interfaces)
R -e "shiny::runApp('/app', host='0.0.0.0', port=3838)"
