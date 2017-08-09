library(dplyr)
library(readr)
library(tidyr)
library(rgdal)
library(rgeos)
library(ggplot2)

#Estimated Population by LGA and Year
url_LGAEstPop <- "http://www.qgso.qld.gov.au/products/tables/erp-lga-qld/erp-lga-qld.csv"
lga_est_pop_raw <- read_csv(url(url_LGAEstPop),col_types = cols(`1991`="n"), skip=3)
colnames(lga_est_pop_raw)[1] <- "LGA"
# Begin straightening up the raw data we imported
lga_est_pop_ds <- filter(lga_est_pop_raw, !(row_number() %in% c(1,2)), row_number() < n() - 7) %>% 
  mutate(LGA = gsub(" \\(.\\)","", LGA)) %>% 
  mutate(LGA=toupper(LGA)) %>%
  mutate(LGA = gsub("-"," ", LGA))
# Check we have a LGA pop record for each LGA in EGM data (should be zero)
unique(e$LGA.Region[!(e$LGA.Region %in% lga_est_pop_ds$LGA)])


# Normalise the data
lga_est_pop_dsn <- gather(lga_est_pop_ds,year,pop,`1991`:`2016pr`)

url_LGA_Boundaries <- "http://qldspatial.information.qld.gov.au/catalogue/custom/search.page?q=%22Local%20government%20area%20boundaries%20-%20Queensland%22#"

qld_boundary_sp <- readOGR(
  dsn = "data/State Boundaries MAY 2016/Standard/QLD_STATE_POLYGON_shp.shp",
  layer = "QLD_STATE_POLYGON_shp")

qld_boundary_df <- fortify(qld_boundary_sp)

qld_lga_sp <- readOGR(
  dsn = "data/QSC_Extracted_Data_20170809_220025273000-14836/Local_Government_Areas.shp",
  layer = "Local_Government_Areas")

qld_lga_clip <- gIntersection(qld_lga_sp, qld_boundary_sp, byid = TRUE)

qld_lga_df <- fortify(qld_lga_sp,region = "ABBREV_NAM")

qld_lga_sp@data$ABBREV_NAM <- as.character(qld_lga_sp@data$ABBREV_NAM)
qld_lga_df$id <- as.character(qld_lga_df$id)

qld_lga_df <- left_join(qld_lga_df,qld_lga_sp@data,by=c("id"="ABBREV_NAM"))

qld_map <- ggplot(qld_boundary_df,aes(long,lat,group=group)) + 
  geom_polygon(fill="white",colour="black") +
  coord_equal()
ggsave("qld_map.pdf", plot = qld_map)
#print(qld_map)

qld_lga_map <- ggplot(qld_lga_df,aes(long,lat,group=group)) + 
  geom_polygon(fill="white",colour="black") +
  coord_equal()
# print is waaayyyy too slow on Mac
#print(qld_lga_map)
ggsave("qld_lga_map.pdf", plot = qld_lga_map)

