# devtools::install_github("ben-williams/scallopr")

library(scallopr)
theme_set(FNGr::theme_sleek())

# data ----
events_data <- read_csv('data/2017/events_2017_170628.csv') 
catch_data <- read_csv('data/2017/catchComp_2017_170623.csv')
awl_data <- read_csv('data/2017/awl_2017_170623.csv')

set.seed(8456)

# Constants for 2017 survey ----
YEAR <- 2017
Q <- 0.83

# clean data ----
tows <- clean_tow(events_data)
beds <- clean_bed(tows)
scal_catch <- clean_catch(catch_data, tows, YEAR)
scal_awl <- clean_awl(awl_data, tows)

# table of catch results
tbl_catch <- tbl_catch(scal_catch, beds)
abund_est <- scallop_est(scal_catch, beds, Q = Q, abundance = TRUE, boot = TRUE)
biom_est <- scallop_est(scal_catch, beds, Q = Q, abundance = FALSE, boot = TRUE)
meat_wt_est(scal_awl, scal_catch, Q = Q, beds, boot = TRUE)
tbl_sex(scal_awl, tows)
tbl_worm(scal_awl, tows)
tbl_gonad(scal_awl, tows)


# figures ----
plot_catch_rate(scal_catch, tows, YEAR)

# Function for plotting abundance (abundance = TRUE) or biomass estimates
plot_scal_est(abund_est, abundance = TRUE, YEAR)
plot_scal_est(biom_est, abundance = FALSE, YEAR)

plot_size_dist(scal_awl, scal_catch, tows, YEAR)
plot_mw_rw(scal_awl, tows, Districts, YEAR)
plot_mw_sh(scal_awl, tows, Districts, YEAR)

