#!/bin/bash

# sorvi-install-dependencies-debian
#
# Asentaa vaaditut riippuvuudet soRvi-pakettiin
# http://sorvi.r-forge.r-project.org/
# Huom: kokeellinen versio, joitain riippuvuuksia saattaa viela puuttua
#
# Copyright (C) 2011-2012 Leo Lahti <leo.lahti@iki.fi>
#
# Licence: FreeBSD (keep this notice)
#

set -e

# XML
sudo apt-get -y install libxml2-dev

# GEOS
sudo apt-get -y install libgeos-dev

# PROJ.4
sudo apt-get -y install proj

# CURL
sudo apt-get -y install libcurl3 libcurl4-openssl-dev

# GDAL
sudo apt-get -y install libgdal1-dev libproj-dev

# GLUT / OpenGL
sudo apt-get -y install freeglut3 freeglut3-dev

# Graphviz
sudo apt-get install graphviz

# Java for R XLConnect etc
sudo apt-get install python-software-properties
sudo add-apt-repository ppa:ferramroberto/java
sudo apt-get update
sudo apt-get install sun-java6-jdk sun-java6-plugin
sudo update-alternatives --config java
sudo R CMD javareconf


