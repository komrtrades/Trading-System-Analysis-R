# Base R image
FROM rocker/r-ver:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    default-jdk \
    && apt-get clean

WORKDIR /app

RUN R -e "install.packages('rvest')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('httr')"
RUN R -e "install.packages('quantmod')"
RUN R -e "install.packages('TTR')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('mailR')"
RUN R -e "install.packages('dotenv')"

# Copy app files
COPY MC_System /app/MC_System
COPY Scrapers /app/Scrapers
COPY global.R /app/global.R
COPY .Renviron /app/.Renviron

# Expose port for API
EXPOSE 8081

# Run the R script
CMD ["Rscript", "/app/MC_System/strategy.R"]
