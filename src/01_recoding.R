# Tidy Data Project
# Andrea De Angelis
# 7th of June 2019

# This scripts sets up the data analysis' project:
# - Sources setup scripts
# - Makes data wrangling
# - Stores tidy dataset in /output/DSP_recoded.Rdata

# Source setup scripts:
source(here::here("src","00_setup.R"))

# Folder structure:
# - Tidy Data Project/ 
# |------ src/
# |------ data/
# |------ doc/ 
# |------ figs/
# |------ output/
# |------ junk/

# Working directory from .Rproj
here::here("")

# Load data:
load(here::here("data","DSP_original.Rdata"))

# Select only certain variables
DSP_rec <- subset(DSP_Dataset_v1, select=c(country_name, country_text_id, country_id, year, 
                                                  v2smgovdom, v2smgovdom, v2smgovsmmon, v2smregcon, v2smonper, v2smmefra, v2smhargr_0, v2smhargr_1, v2smhargr_2, v2smhargr_6, v2smarrest_ord, v2smpolsoc, v2smpolhate_ord))

# Country and year to factor:
DSP_rec$country_name <- as.factor(DSP_rec$country_name)
DSP_rec$year <- as.factor(DSP_rec$year)

# Drop duplicated variable:
DSP_rec$v2smgovdom.1 <- NULL

# Flip reversed-coded variable
DSP_rec$v2smpolsoc <- DSP_rec$v2smpolsoc*(-1)

# Save recoded dataset
save(DSP_rec, file=here::here("output","DSP_recoded.Rdata"))
