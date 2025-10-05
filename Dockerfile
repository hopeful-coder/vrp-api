FROM rstudio/plumber:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .

RUN Rscript install_packages.R

EXPOSE 8000

CMD ["Rscript", "plumber.R"]
