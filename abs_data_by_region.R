#' @author Ashley Betts, \email{ashley.betts@@saltbushsoftware.com}

library(plyr)
library(readr)
library(tidyr)
library(openxlsx)
library(data.table)
library(tibble)
library(dplyr)

# Data By Region Config ---------------------------------------------------

lga_data <- tribble(
  ~desc, ~url, ~xlsname, ~sheet, ~startRow, ~dsname,
  #
  "Population and People, LGA, 2011-2016",
  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&14100ds0002_2017-03.zip&1410.0&Data%20Cubes&3365E3D7D5C5D3B4CA2580F30016EE08&0&2011-16&31.03.2017&Latest",
  "14100DS0002_2017-03.xlsx",
  "Population and People _LGA_1540",
  5,
  "lga_pop_ds",
  #
  "Economy and Industry, LGA, 2011-2016",
  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&14100ds0004_2017-03.zip&1410.0&Data%20Cubes&075186AFEE000708CA2580F30016EFF4&0&2011-16&31.03.2017&Latest",
  "14100DS0004_2017-03.xlsx",
  "Economy and Industry _LGA_15424",
  4,
  "lga_ecind_ds",
  #
  "Income (including Government Allowances), Education and Employment, Health and Disability, LGA, 2011-2016",
  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&14100ds0006_2017-03.zip&1410.0&Data%20Cubes&F60E73CA39B052C1CA2580F30016F17E&0&2011-16&31.03.2017&Latest",
  "14100DS0006_2017-03.xlsx",
  "Income_Educ and Emp_Health_LGA_",
  4,
  "lga_inc_ds",
  #
  "Family and Community, Land and Environment, LGA, 2011-2016",
  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&14100ds0008_2017-03.zip&1410.0&Data%20Cubes&8EE9B9D069384332CA2580F30016F2C9&0&2011-16&31.03.2017&Latest",
  "14100DS0008_2017-03.xlsx",
  "Family_Land_LGA_1546198",
  4,
  "lga_fam_ds"
)

# Funcs to Assist with dataset creation ----------------------------------------

#' Generates column names
#'
#' @param ds Raw dataset (data.frame) from which to generate the column names.
#'
#' @return
gen_col_names <- function(ds) {
  col_groups <- ds[1,4:ncol(ds)]
  col_grp_names <- make.names(fill(transpose(col_groups),V1)$V1, unique = F)
  f_col_names <- transpose(ds[3,1:3])$V1
  r_col_vals <- transpose(ds[2,4:ncol(ds)])$V1
  r_col_names <- make.names(paste(col_grp_names,r_col_vals, sep="_"), unique = T)
  col_names <- c(f_col_names, r_col_names)
  col_names
}

#' Downloads and creates a base dataset.
#' 
#' Generally used in apply, hence why param is row.
#'
#' @param rw row
#'
#' @return
gen_ds <- function(rw) {
  url_ref <- rw["url"]
  xlsname <- rw["xlsname"]
  sheet <- rw["sheet"]
  startRow <- as.numeric(rw["startRow"])
  dsname <- rw["dsname"]
  zipname <- "gen_ds_tmp.zip"
  download.file(url_ref, zipname)
  unzip(zipname)
  unlink(zipname)
  
  raw_ds <- read.xlsx(xlsname, sheet=sheet, startRow=startRow, na.strings="-")
  names(raw_ds) <- gen_col_names(raw_ds)
  final_ds <- raw_ds %>% 
    filter(row_number() > 3, row_number() < n() - 2)
  assign(dsname, final_ds, envir = .GlobalEnv)
}

#' Creates the datasets identified in the config item.
#'
#' @param config a structure describing the datasets to create.
#'
#' @return
create_abs_dbr_dss <- function(config) {
  invisible(apply(config,1,gen_ds))
}

#' Convenience function to create datasets based on default config.
#'
#' @return
#' @export
#'
#' @examples
create_abs_dbr_dss_def <- function() {
  create_abs_dbr_dss(lga_data)
}

#' Used to create a mapping to LGA Code for the Qld Gaming Machine related data.
#'
#' @param abs_base_ds - one of the ABS data by region datasets to used to extract the codes.
#'
#' @return
create_qld_lga_mapping <- function(abs_base_ds) {
  abs_lgas <- abs_base_ds %>%
    dplyr::select(LABEL,CODE) %>% 
    filter(CODE > 30000, CODE < 40000) %>% 
    mutate(LABEL = gsub(" \\(.\\)","", LABEL)) %>% 
    mutate(LABEL=toupper(LABEL)) %>% 
    mutate(LABEL = gsub("-"," ", LABEL)) %>% 
    distinct()
  abs_lgas
}

# I use this to check we have all of the LGA's in the Qld Gaming datasets covered.
# unique(lg_egm_df$`LGA Region`[!(lg_egm_df$`LGA Region` %in% abs_lgas$LABEL)])

# End ------------------------------------------
