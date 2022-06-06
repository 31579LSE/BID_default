
#___________________________________________________________________________________________________
#/ Country-year level default estimation
#/ Data preparation file - yearly data set
#/ 05-06-2022 
#___________________________________________________________________________________________________

# 0.0 setup ----------------------------------------------------------------------------------------

options(scipen=999)

Sys.setenv(http_proxy='http://staff-proxy.ul.ie:8080')
Sys.getenv('HTTPS_PROXY')

lib <- c('tidyr','plyr', 'openxlsx', 'Hmisc','haven')
lapply(lib, library, character.only = TRUE);rm(lib)

wd_y <- c('L:/99.BID app/01.tables/01.yearly_data'); setwd(wd_y)
y_files <- list.files(wd_y)

# 0.1 loads yearly data ----------------------------------------------------------------------------
#/ Loops through files, excludes the WJP records.

file_list <- lapply(y_files[c(1:2,4:5)], function(files) {openxlsx::read.xlsx(files)})
names(file_list) <- gsub('.xlsx','',y_files[c(1:2,4:5)])

# _|0.1.1. WJP data --------------------------------------------------------------------------------
#/  Loads WJP sheets independently

sheets  <- openxlsx::getSheetNames(paste0(wd_y,'/',y_files[3])) #gets sheets names
dt_WJP  <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = paste0(wd_y,'/',y_files[3])) #loads sheets
dt_WJPl <- list() # empty list to fill
sheets  <- gsub('\\D+','', sheets) # vector to build year variable in sets

#/  Reshapes reports 
for(i in 1:length(sheets)){

  aux <- dt_WJP[[i]]
  colnames(aux)[colnames(aux) == "Country"] <- "Variable" # renames merged cell
  
  aux <- tidyr::gather(aux, country, measurement, 2:ncol(aux), factor_key=TRUE) # wide-long
  aux <- tidyr::spread(aux, Variable, measurement) #long - wide year-country level
  aux$Region <-NULL #removes regions
  aux$year <- sheets[i] #adds year
  
  dt_WJPl[[i]] <- aux; rm(aux)
}

#/  Appends reports. 2012-2019 and 2017-2018 data is repeated, lacks variation for FE? 
dt_WJP <- do.call("rbind.fill", dt_WJPl); rm(sheets,dt_WJPl)
table(dt_WJP$year)

dt_WJP$year[dt_WJP$year == '20122013'] <- 2012
dt_WJP$year[dt_WJP$year == '20172018'] <- 2017

aux1 <- dt_WJP[dt_WJP$year == '2012',]; aux1$year <- 2013
aux2 <- dt_WJP[dt_WJP$year == '2017',]; aux2$year <- 2018

dt_WJP <- rbind(dt_WJP,aux1,aux2); rm(aux1,aux2)

names(dt_WJP) <- tolower(names(dt_WJP))
dt_WJP$country <- tolower(dt_WJP$country)

# _|0.1.2. IMF crisis data -------------------------------------------------------------------------

dt_IMF_crisis <- file_list[['00.IMF_crisis_y']]
names(dt_IMF_crisis) <- tolower(names(dt_IMF_crisis))
dt_IMF_crisis$country_lab <- tolower(dt_IMF_crisis$country)
dt_IMF_crisis$country <- tolower(dt_IMF_crisis$code); dt_IMF_crisis$code <-NULL

# _|0.1.3. IMF fiscal rule data --------------------------------------------------------------------

dt_IMF_frule <- file_list[['01.IMF_fiscalrules_x']]
names(dt_IMF_frule) <- tolower(names(dt_IMF_frule))
dt_IMF_frule$country <- tolower(dt_IMF_frule$code); dt_IMF_frule$code <-NULL

# _|0.1.4. WB debt, GDP, capital data --------------------------------------------------------------

dt_WB_debt <- file_list[['11.WB_GPP_debt_x']]
names(dt_WB_debt) <- tolower(names(dt_WB_debt))
dt_WB_debt$country <- tolower(dt_WB_debt$country.code)
dt_WB_debt$country.name <- NULL
dt_WB_debt$country.code <- NULL

dt_WB_debt$year <- tolower(dt_WB_debt$time); dt_WB_debt$time <- NULL

# _|0.1.5. FMI World Economic Outlook data ---------------------------------------------------------

dt_IMF_WEO <- file_list[['12.IMF_WEO_x']]
dt_IMF_WEO <- tidyr::gather(dt_IMF_WEO, year, measurement, 5:ncol(dt_IMF_WEO), factor_key=TRUE)
dt_IMF_WEO <- tidyr::spread(dt_IMF_WEO, Subject.Descriptor, measurement)
names(dt_IMF_WEO) <- tolower(names(dt_IMF_WEO))
dt_IMF_WEO$country <- tolower(dt_IMF_WEO$iso)

dt_IMF_WEO$iso <- NULL
dt_IMF_WEO$weo.country.code <- NULL

# _|0.1.6. Daniel Kaufmann WGI----------------------------------------------------------------------

sheets  <- openxlsx::getSheetNames(paste0(wd_y,'/',y_files[6])) #gets sheets names
dt_WGI  <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = paste0(wd_y,'/',y_files[6])) #loads sheets
dt_WGIl <- list() # empty list to fill

#/  Reshapes reports 
for(i in 1:length(sheets)){
  
  aux <- dt_WGI[[i]]

  aux <- tidyr::gather(aux, year_meas, measurement, 3:ncol(aux), factor_key=TRUE) # wide-long
  aux$year <- gsub('\\D+','', aux$year_meas)
  aux$meas <- gsub('[0-9.]', '', aux$year_meas)
  aux$year_meas <- NULL
  
  aux <- tidyr::spread(aux, meas, measurement) #long - wide year-country level
  colnames(aux)[4:length(names(aux))] <- paste(sheets[i], colnames(aux)[4:length(names(aux))], sep = "_")
  
  aux$country  <- aux$code; aux$code <- NULL
  aux$year_country <- paste0(aux$country,aux$year)
  aux$country  <- NULL
  aux$year     <- NULL
  dt_WGIl[[i]] <- aux; rm(aux)
}

#/  Merges sets

dt_WGI <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "year_country", all.x = TRUE),dt_WGIl)

names(dt_WGI) <- tolower(names(dt_WGI))
dt_WGI$year_country <- tolower(dt_WGI$year_country)

# 0.2 creates yearly panel -------------------------------------------------------------------------
#/ builds year-country key

#table(unique(dt_IMF_crisis$country)%in%unique(dt_WB_debt$country))

dt_IMF_crisis$year_country <- paste0(dt_IMF_crisis$country, dt_IMF_crisis$year)
dt_IMF_WEO$year_country    <- paste0(dt_IMF_WEO$country, dt_IMF_WEO$year)
dt_IMF_frule$year_country  <- paste0(dt_IMF_frule$country, dt_IMF_frule$year)
dt_WB_debt$year_country    <- paste0(dt_WB_debt$country,dt_WB_debt$year)
dt_WJP$year_country        <- paste0(dt_WJP$country,dt_WJP$year)

dt_IMF_WEO$year   <- NULL; dt_IMF_WEO$country   <- NULL
dt_IMF_frule$year <- NULL; dt_IMF_frule$country <- NULL
dt_WB_debt$year   <- NULL; dt_WB_debt$country   <- NULL
dt_WJP$year       <- NULL; dt_WJP$country       <- NULL

#/  Sets crisis table as the frame for merging

dt_yearly <- merge(dt_IMF_crisis,dt_IMF_WEO, all.x = TRUE, by = 'year_country' )
dt_yearly <- merge(dt_yearly,dt_IMF_frule, all.x = TRUE, by = 'year_country' )
dt_yearly <- merge(dt_yearly,dt_WB_debt, all.x = TRUE, by = 'year_country' )
dt_yearly <- merge(dt_yearly,dt_WJP, all.x = TRUE, by = 'year_country' )
dt_yearly <- merge(dt_yearly,dt_WGI, all.x = TRUE, by = 'year_country' )

#openxlsx::write.xlsx(dt_yearly[!duplicated(dt_yearly$country),c('country','country_lab')],
  # '99.LAC_countries.xlsx',overwrite = T)

#/  Merges contry region ids

aux_country <- openxlsx::read.xlsx('L:/99.BID app/01.tables/99.aux tables/99.LAC_countries.xlsx')
dt_yearly <- merge(dt_yearly,aux_country, all.x = TRUE, by = 'country' ); rm(aux_country)


# 0.3 prepares labels and exports ------------------------------------------------------------------

##openxlsx::write.xlsx(data.frame(names = names(dt_yearly)), '99.names_ssdt_yearly.xlsx',overwrite = T)

aux_varlist <- openxlsx::read.xlsx('L:/99.BID app/01.tables/99.aux tables/99.varlist_dt_yearly.xlsx')
table(aux_varlist$old_names==names(dt_yearly))
colnames(dt_yearly) <- aux_varlist$new_names; rm(aux_varlist)

#/ Gives labels to each variable, code template created in the "99.varlist_dt_yearly.xlsx" aux file

aux_varlist <- openxlsx::read.xlsx('L:/99.BID app/01.tables/99.aux tables/99.varlist_dt_yearly.xlsx',sheet = 2)
var_labels <- tibble::deframe(aux_varlist[c('new_names','label')]); rm(aux_varlist)

label(dt_yearly) = as.list(var_labels[match(names(dt_yearly), names(var_labels))])

setwd('L:/99.BID app/01.tables/02.out tables')
haven::write_dta(dt_yearly, "dt_yearly.dta")
saveRDS(dt_yearly, "dt_yearly.rds")

