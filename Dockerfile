FROM rocker/shiny:4.0.3 
# system libraries of general use
 
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libjpeg-dev \
    libxml2

# Install R Dependencies - installs the r packages you need - if this step fails you’re likely 
# missing system libraries that a package requires
# install R packages

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_version('shiny', version = '1.5.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinyBS', version = '0.61', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinyjs', version = '2.0.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinyalert', version = '2.0.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinydashboard', version = '0.7.1', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinycssloaders', version = '1.0.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('DT', version = '0.15', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('data.table', version = '1.13.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('DSI', version = '1.3.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('DSOpal', version = '1.3.0', repos = 'http://cran.r-project.org')"
RUN R -e "devtools::install_version('stringr', version = '1.4.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('dsBaseClient', repos = c(getOption('repos'), 'http://cran.obiba.org'), dependencies = TRUE)"
RUN R -e "devtools::install_version('ggrepel', version = '0.8.2', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_github('isglobal-brge/dsOmicsClient')"
RUN R -e "devtools::install_version('shinyWidgets', version = '0.5.4', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('stringr', version = '1.4.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('fresh', version = '0.2.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_github('Mikata-Project/ggthemr')"
RUN R -e "devtools::install_github('neelsoumya/dsSurvivalClient', 'privacy_survival_curves')"
RUN R -e "devtools::install_version('survminer', version = '0.4.9', repos = 'http://cran.us.r-project.org')"
RUN R -e "install.packages('forestplot')"

# Install CA certificates

RUN apt-get --no-install-recommends install -y ca-certificates

# copy shiny-server.sh to image
 
COPY shiny-server.sh /usr/bin/
  
# copy shiny server config to image
 
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

# copy the contents of app folder to image
 
COPY /inst/shinyApp/ /srv/shiny-server/R/

# copy the contents of www folder to image
 
COPY /inst/shinyApp/www/ /srv/www/

# copy the description file to have the version number to be displayed

COPY DESCRIPTION /srv/shiny-server/R/

# select port
 
EXPOSE 80
 
# allow permission for user ‘shiny’ to run
 
RUN sudo chown -R shiny:shiny /srv/shiny-server
 
# Change access permissions to shiny-server.sh - did not need this for my purposes
 
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"] 

