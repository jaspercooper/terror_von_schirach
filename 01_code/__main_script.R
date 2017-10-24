rm(list = ls())

# Scrape again? -----------------------------------------------------------

# TRUE = use newly-scraped data 
# FALSE = use most recent already-scraped data

scrape_again <- TRUE

# Load packages -----------------------------------------------------------

source("01_code/00_load_packages.R")

source("01_code/08_helpful_functions.R")

# Load data ---------------------------------------------------------------

source("01_code/02_read_data.R")

# Make new variables ------------------------------------------------------

source("01_code/03_make_variables.R")

# Poisson Analysis --------------------------------------------------------

source("01_code/04_poisson_analysis.R")

# Linear ------------------------------------------------------------------

source("01_code/05_linear_analysis.R")



