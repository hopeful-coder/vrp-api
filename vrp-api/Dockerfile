FROM rstudio/plumber:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .

RUN R -e "install.packages(c('plumber', 'jsonlite', 'TSP'), repos='https://cloud.r-project.org/')"

EXPOSE 8000

CMD ["Rscript", "plumber.R"]
