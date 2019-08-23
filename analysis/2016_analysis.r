devtools::install_github("ben-williams/scallopr")
theme_set(FNGr::theme_sleek())

library(scallopr)

# data ----
events_data <- vroom('data/2016/events_2016_161027.csv') 
catch_data <- vroom('data/2016/catchComp_2016_161027.csv', col_types=list(CONDITION_CODE_RII = col_double()))
area_data <- vroom('data/2016/area.csv') %>% 
  dplyr::select(Bed, area_nm2=area.nm2, stations)   # all areas
awl_data <- vroom::vroom('data/2016/awl_2016_161027.csv', delim = ",")

set.seed(8456)

# Constants for 2016 survey ----
YEAR <- 2016
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

