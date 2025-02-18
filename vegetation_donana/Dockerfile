

#### Dockerfile


# A Dockerfile is a text file that contains a set of instructions for
# building a Docker image. It automates the process of creating a 
# containerized environment.

# Think of it like a recipe for building a custom operating system with
# all the tools and software you need.

# 1. You write a Dockerfile with instructions on how to set up your environment.
# 2. Docker reads the Dockerfile and builds a custom Docker image based on it.
# 3. You run a container from the built image, which behaves like a lightweight, isolated virtual machine.

#### IMAGE - your set of instructions for container

# FROM → This is the instruction that sets the base image.
# eco4cast/rocker-neon4cast → This is the Docker image name 
# When Docker processes this line, it downloads eco4cast/rocker-neon4cast 
# if it's not already on your system and uses it as a starting point  
# for your custom image.
FROM eco4cast/rocker-neon4cast

# This command updates the package list (apt-get update) and installs 
# essential libraries and tools (apt-get install -y ...) needed for R, 
# Git, Python, and networking.
# Basically - it installs system dependencies required for ows4R and 
# other packages used in R scripts
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

# Copy/Clone/Pull the R scripts and data files from current directory (.)
# into the docker (container) and save them in docker 
# into directory: "/home/rstudio/vegetation_donana" -> you decide how this is called
# The . (dot) represents the current directory in a file system, 
# meaning the directory you are currently in when running a command. 
COPY . /home/rstudio/vegetation_donana

# Set the working directory of docker
WORKDIR /home/rstudio/vegetation_donana

# RUN is used to install or set up things when building the Docker image
# (e.g., installing software).

# Install one more necessery R package into Docker - dependency of load_packeges.R
RUN Rscript -e "install.packages('reticulate', repos='http://cran.rstudio.com/')"

# Run load_packages.R - this command will stop after it is done
RUN Rscript load_packages.R

# CMD is used to specify the default command that runs when the container
# starts (e.g., launching a program).

#### CONTAINER = your virtual computer

# Set the default command to run the Shiny app when the container starts
# CMD → Defines the default command for the container.
# "Rscript" → Runs R scripts from the command line. -> Basically it is like the backend of the botton you press to run code in Rstudio 
# "app.R" → The script that will execute inside the container.
CMD ["Rscript", "app.R"]

# EXPOSE 3838 tells Docker that the container will listen for connections
# on port 3838, which is the default port for Shiny apps.
# ! in your app.R line have to be added: 
# shinyApp(
#  ui = ui,
#  server = server,
#  options = list(
#    host = "0.0.0.0",  # Listen on all network interfaces
#    port = 3838         # The port to be used (change to 3839 if needed)
#  )
# )
EXPOSE 3838
