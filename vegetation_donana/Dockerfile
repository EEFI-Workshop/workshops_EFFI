# Use eco4cast/rocker-neon4cast as the base image
FROM eco4cast/rocker-neon4cast

# Install system dependencies required for ows4R and other packages
RUN apt-get update && apt-get install -y \
    libsecret-1-0 \
    libsecret-1-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libgit2-dev \
    git \
    curl \
    python3-venv \
    python3-pip \
    python3-dev

# Copy the R scripts and data files into the container
COPY . /home/rstudio/vegetation_donana

# Set the working directory
WORKDIR /home/rstudio/vegetation_donana

# Install necessary R packages first
RUN Rscript -e "install.packages('reticulate', repos='http://cran.rstudio.com/')"

# Install necessary R packages before starting the container
RUN Rscript load_packages.R

# Set the default command to run an R script when the container starts
CMD ["Rscript", "app.R"]


# RUN Rscript main_analyses_donana.r
# Install Shiny
#RUN R -e "install.packages('shiny')"

# Copy the Shiny app files into the container
#COPY shiny_app /home/rstudio/vegetation_donana/shiny_app

# Expose the Shiny app port
#EXPOSE 3838

# Run the Shiny app
#CMD ["R", "-e", "shiny::runApp('/home/rstudio/vegetation_donana/shiny_app', host = '0.0.0.0', port = 3838)"]