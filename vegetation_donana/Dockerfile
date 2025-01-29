# FROM eco4cast/rocker-neon4cast

# # Copy the R scripts and data files into the container
# COPY . /home/rstudio/vegetation_donana

# # Set the working directory
# WORKDIR /home/rstudio/vegetation_donana

# # Add the default command to run an R script when the container starts
# CMD ["Rscript", "load_packages.R"]
# # Set the default command to run an R script
# # RUN Rscript load_packages.R && echo "install_libraries.R has finished running"

# Use eco4cast/rocker-neon4cast as the base image
FROM eco4cast/rocker-neon4cast

# Install system dependencies required for ows4R and other packages
RUN apt-get update && apt-get install -y \
    libsecret-1-0 \            # Required for keyring (ows4R dependency)
    libsecret-1-dev \          # Development headers for keyring
    libcurl4-openssl-dev \     # Required for HTTP requests (httr, ows4R dependency)
    libxml2-dev \              # Required for XML parsing (ows4R dependency)
    libssl-dev \               # Required for SSL connections
    libgit2-dev \              # Needed for installing Git-based R packages
    git \                      # Required for installing Rchelsa from Git
    curl                       # Ensures web requests work properly

# Copy the R scripts and data files into the container
COPY . /home/rstudio/vegetation_donana

# Set the working directory
WORKDIR /home/rstudio/vegetation_donana

# Install necessary R packages before starting the container
RUN Rscript load_packages.R

# Set the default command to run an R script when the container starts
CMD ["Rscript", "load_packages.R"]

# RUN Rscript main_analyses_donana.r
# Install Shiny
#RUN R -e "install.packages('shiny')"

# Copy the Shiny app files into the container
#COPY shiny_app /home/rstudio/vegetation_donana/shiny_app

# Expose the Shiny app port
#EXPOSE 3838

# Run the Shiny app
#CMD ["R", "-e", "shiny::runApp('/home/rstudio/vegetation_donana/shiny_app', host = '0.0.0.0', port = 3838)"]