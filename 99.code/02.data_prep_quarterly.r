
#___________________________________________________________________________________________________
#/ Country-year-quarter level default estimation
#/ Data preparation file - yearly data set
#/ 05-06-2022 
#___________________________________________________________________________________________________

# 0.0 setup ----------------------------------------------------------------------------------------

options(scipen=999)

Sys.setenv(http_proxy='http://staff-proxy.ul.ie:8080')
Sys.getenv('HTTPS_PROXY')

lib <- c('tidyr','plyr', 'openxlsx', 'Hmisc')
lapply(lib, library, character.only = TRUE);rm(lib)

wd_y <- c('L:/99.BID app/01.tables/01.quarterly data'); setwd(wd_y)
y_files <- list.files(wd_y)

# 0.1 loads quarterly data -------------------------------------------------------------------------

# _|0.1.1. WB debt, GDP, capital data --------------------------------------------------------------

dt_WB_debt <- openxlsx::read.xlsx(y_files[1])

# 0.2 loads yearly data ----------------------------------------------------------------------------

# _|0.2.1. Merged panel, including crisis ----------------------------------------------------------

dt_yearly <- readRDS('L:/99.BID app/01.tables/02.out tables/dt_yearly.rds')

#/  subset with relevant variables
aux_vars  <- c('year_country', 'country_lab', 'LAC', 'LATAM', 'CENTAM', 'd_cy',
              'd_dy', 'er', 'rr', 'bbr', 'dr', 'ROL_overall','GGR_gdp','GGTEXP_gdp','GDP_US',
              'ne_gdi_totl_kd', 'GDPpercap_US','VA_estimate', 'PS_estimate', 'GE_estimate',
              'RQ_estimate', 'ROL_estimate', 'CCRR_estimate'
) 

dt_yearly <- dt_yearly[aux_vars]; rm(aux_vars)
  
# 0.3 creates quarterly panel ----------------------------------------------------------------------

#/ crisis data is at the year-country level. With the risk of less variation in data, the records
#/ from the yearly table are merged to each corresponding quarter within each year of the WB data

# _|0.3.1. year-country id for the qarterly data ---------------------------------------------------

dt_WB_debt$year         <- substr(dt_WB_debt$Time,0,4)
dt_WB_debt$country      <- tolower(dt_WB_debt$Country.Code); dt_WB_debt$Country.Code <- NULL
dt_WB_debt$year_country <- paste0( dt_WB_debt$country,dt_WB_debt$year)

table(dt_yearly$year_country %in% dt_WB_debt$year_country)

#/  Sets WB table as the frame for merging

dt_quarterly <- merge(dt_WB_debt,dt_yearly, all.x =  TRUE, by = 'year_country')

#/  Checks for NA in dependent variable and drops missing records

dt_quarterly <- dt_quarterly[is.na(dt_quarterly$d_cy) == F,];table(is.na(dt_quarterly$d_cy))


# 0.4 prepares labels and exports ------------------------------------------------------------------

# openxlsx::write.xlsx(data.frame(names = names(dt_quarterly)), '99.varlistd_dt_quarterly.xlsx',overwrite = T)

aux_varlist <- openxlsx::read.xlsx('L:/99.BID app/01.tables/99.aux tables/99.varlist_dt_quarterly.xlsx')
table(aux_varlist$old_names==names(dt_quarterly))
colnames(dt_quarterly) <- aux_varlist$new_names; rm(aux_varlist)

#/ Gives labels to each variable, code template created in the "99.varlist_dt_yearly.xlsx" aux file

aux_varlist <- openxlsx::read.xlsx('L:/99.BID app/01.tables/99.aux tables/99.varlist_dt_quarterly.xlsx',sheet = 2)
var_labels <- tibble::deframe(aux_varlist[c('new_names','label')]); rm(aux_varlist)

label(dt_quarterly) = as.list(var_labels[match(names(dt_quarterly), names(var_labels))])

#/ Sets quarter

dt_quarterly$quarter <- as.numeric(substr(dt_quarterly$year_quarter,nchar(dt_quarterly$year_quarter), nchar(dt_quarterly$year_quarter)))
dt_quarterly$year    <- as.numeric(substr(dt_quarterly$year_quarter,0, 4))

setwd('L:/99.BID app/01.tables/02.out tables')
haven::write_dta(dt_quarterly, "dt_quarterly.dta")
saveRDS(dt_quarterly, "dt_quarterly.rds")
