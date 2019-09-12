
# Purpose: Set up repository for Cambodia Analysis and Capacity Building
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2019_09_10
# Audience: Cambodia Mission

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable", "sjmisc", "survey", "data.table", "lemon", "widyr", "RColorBrewer", "readxl")

# Create folders for project (if they do not exist)
folder_list <- list("Data", "Images", "Scripts", "Dataout", "GIS", "Documents", "Graphics")
map(folder_list, ~dir.create(.))

datapath <- "Data"
dataout <- "Dataout"
gispath <- "GIS"
graphpath <- "Graphics"
imagepath <- "Images"
rpath <- "Scripts"
