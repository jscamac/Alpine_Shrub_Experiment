FROM rocker/tidyverse:3.3.2
MAINTAINER James Camac <james.camac@gmail.com>

# Install latex, git and clang then clean up tmp files
RUN    apt-get update \
    && apt-get install -y --no-install-recommends \
         libcurl4-openssl-dev \
         texlive-latex-recommended \
         texlive-latex-extra \
         texlive-humanities \
         texlive-fonts-recommended \
         texlive-science \
         lmodern \
         git \
         clang \
    && apt-get clean \
    && apt-get autoremove \
    && rm -rf var/lib/apt/lists/*

# Global site-wide config
RUN mkdir -p $HOME/.R/ \
    && echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function\n" >> $HOME/.R/Makevars \
    && echo "CXXFLAGS+=-flto -ffat-lto-objects  -Wno-unused-local-typedefs\n" >> $HOME/.R/Makevars

# Install other dependent R packages
RUN install2.r -r "https://mran.revolutionanalytics.com/snapshot/2016-11-25/" --error \
    --deps "TRUE" \
    rstan reshape2 cowplot lubridate

# Install remake
RUN installGithub.r \
    --deps "TRUE" \
    richfitz/remake

# Remove unnecesarry tmp files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Clone shrub repository
RUN git clone https://github.com/jscamac/Alpine_Shrub_Experiment /home/Alpine_Shrub_Experiment

# Set working directory
WORKDIR /home/Alpine_Shrub_Experiment

# Open R
CMD ["R"]