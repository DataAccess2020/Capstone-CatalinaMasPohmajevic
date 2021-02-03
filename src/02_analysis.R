# Tidy Data Project
# Andrea De Angelis
# 7th of June 2019

# This scripts analyze the data:
# - Sources setup scripts
# - Creates figures and stores them in /figs
# - Estimates the regression model2 "fm_govdom1-3"
# - Creates the table for the figures and stores them in /output/Table1.html


# Source setup scripts:
source(here::here("src","00_setup.R"))

# Load data:
load(here::here("output","DSP_recoded.Rdata"))


# Creating figures -----------------------------------------------------------------------------
# Check out this post for info:
# https://thepoliticalmethodologist.com/2013/11/25/making-high-resolution-graphics-for-academic-publishing/

# Figure 1 - monper
tiff(here::here("figs", "Fig1_monper.tif"), res = 600, compression = "lzw", height = 5, width = 5, units = "in")
hist(DSP_rec$v2smonper)
dev.off()

# Figure 2 - govsmmon
tiff(here::here("figs", "Fig2_govsmmon.tif"), res = 600, compression = "lzw", height = 5, width = 5, units = "in")
hist(DSP_rec$v2smgovsmmon)
dev.off()

# Figure 3 - regcon
tiff(here::here("figs", "Fig3_regcon.tif"), res = 600, compression = "lzw", height = 5, width = 5, units = "in")
hist(DSP_rec$v2smregcon)
dev.off()

# Figure 4 - scatter
tiff(here::here("figs", "Fig4_scatter.tif"), res = 600, compression = "lzw", height = 5, width = 5, units = "in")
plot(y = DSP_rec$v2smgovdom, x = DSP_rec$v2smregcon)
dev.off()

# Regression analysis ---------------------------------------------------------------------

# Model 1: baseline specification
fm_govdom1 <- lm(v2smgovdom ~ v2smregcon, data = DSP_rec)

# Model 2: adding country fixed-effects:
fm_govdom2 <- lm(v2smgovdom ~ v2smregcon + country_name, data = DSP_rec)

# Model 3: adding year fixed-effects
fm_govdom3 <- lm(v2smgovdom ~ v2smregcon + country_name + year, data = DSP_rec)

require(stargazer)
stargazer(fm_govdom1, fm_govdom2, fm_govdom3,
  type = "html", style = "apsr",
  out = here::here("output", "Table1.html"),
  omit = c("country_name", "year"),
  add.lines = list(
    c("Country FE", "No", "Yes", "Yes"),
    c("Year FE", "No", "No", "Yes")
  )
)
