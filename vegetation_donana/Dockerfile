FROM eco4cast/rocker-neon4cast

# Copy the R scripts and data files into the container
COPY . /home/rstudio/vegetation_donana

# Set the working directory
WORKDIR /home/rstudio/vegetation_donana

# Set the default command to run an R script
RUN Rscript install_libraries.R && echo "install_libraries.R has finished running"


# Add the default command to run an R script when the container starts
CMD ["Rscript", "install_libraries.R"]

# RUN Rscript main_analyses_donana.r
# Install Shiny
#RUN R -e "install.packages('shiny')"

# Copy the Shiny app files into the container
#COPY shiny_app /home/rstudio/vegetation_donana/shiny_app

# Expose the Shiny app port
#EXPOSE 3838



# Run the Shiny app
#CMD ["R", "-e", "shiny::runApp('/home/rstudio/vegetation_donana/shiny_app', host = '0.0.0.0', port = 3838)"]

