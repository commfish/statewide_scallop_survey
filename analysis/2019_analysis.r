# notes ----
# 2019 Statewide Scallops Survey data analysis
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov

# load ----
library(scallopr)
library(tidyverse)
library(FNGr)
theme_set(FNGr::theme_sleek())

# constants for 2019 survey ----
YEAR <- 2019
Q <- 0.83  #dredge efficieny
Bed_levels <- c("WK1", "YAKB", "YAK3", "YAK4", "YAK5") #bed factor levels for plotting

# data ----
events_data <- read_csv(paste0('./data/', YEAR, '/HaulTableData_Cruise1901.csv')) 
catch_data <- read_csv(paste0('./data/', YEAR, '/CatchTableData_Cruise1901.csv'))
awl_data <- read_csv(paste0('./data/', YEAR, '/ScalMeatTableData_Cruise1901.csv'))
shell_data <- read_csv(paste0('./data/', YEAR, '/ScalShellTableData_Cruise1901.csv'))
set.seed(8456)

# clean data ----
tows <- clean_tow(events_data)
beds <- clean_bed(tows)
scal_catch <- clean_catch(catch_data, tows, YEAR)
scal_awl <- clean_awl(awl_data, tows)

# tables ----

## catch by bed
(tbl_catch <- tbl_catch(scal_catch, beds))

## abundance estimates by bed
(abund_est <- scallop_est(scal_catch, beds, Q = Q, abundance = TRUE, boot = TRUE))

## round weight biomass estimates by bed
(biom_est <- scallop_est(scal_catch, beds, Q = Q, abundance = FALSE, boot = TRUE))

## meat weight estimates by bed
meat_wt_est(scal_awl, scal_catch, Q = Q, beds, boot = TRUE)

## clappers by bed
tbl_clap(scal_catch, scal_awl)

## estimate of large scallops sex ratio by bed
tbl_sex(scal_awl, tows)

## gonad status by bed
tbl_gonad(scal_awl, tows)

## presence of boring worms by bed
tbl_worm(scal_awl, tows)

## presence of mud blisters by bed
tbl_blister(scal_awl, tows)

# figures ----

## catch rate
plot_catch_rate(scal_catch, tows, YEAR, Bed_levels)

## abundance estimates by bed
plot_scal_est(abund_est, abundance = TRUE, YEAR)

## round weight biomass estimates by bed
plot_scal_est(biom_est, abundance = FALSE, YEAR)

## meat weight ~ round weight, by district
plot_mw_rw(scal_awl, tows, Districts, YEAR)

## meat weight ~ shell height, by district
plot_mw_sh(scal_awl, tows, Districts, YEAR)

## SH size composition, by bed
plot_size_dist(scal_awl, scal_catch, tows, YEAR)


