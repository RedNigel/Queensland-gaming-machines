library(dplyr)
library(readr)

## 1. Data sources

# a. Summary of all Qld gambling data
url_Allgamblingdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_All-gambling-data.csv'
# b. Summary data of total EGM in Qld
url_TotalGamingMachineData = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_Total-Queensland-EGM-data.csv'
# c. Summary data of total Hotel EGM in Qld
url_TotalhotelEGMdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_Total-hotel-EGM-data.csv'
# d. Summary data of total Clubs EGM in Qld
url_TotalclubEGMdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_Total-club-EGM-data.csv'
# e. Summary data of LGA EGM in Qld
url_LGAEGMdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_LGA-EGM-data.csv'

## 2. Data import

all_gambling_df <- read_csv(url(url_Allgamblingdata))
total_game_mach_df <-read_csv(url(url_TotalGamingMachineData))
total_hotel_egm_df <-read_csv(url(url_TotalhotelEGMdata))
total_club_egm_df <-read_csv(url(url_TotalclubEGMdata))
lg_egm_df<-read_csv(url(url_LGAEGMdata))
