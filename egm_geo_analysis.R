library(rgdal)
library(GISTools)
library(ggplot2)

# Estimated Population Data -----------------------------------------------

url_lga_est_pop <- "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&32180ds0002_2006-16.xls&3218.0&Data%20Cubes&52F52135AF8E88A5CA25816A00175E8D&0&2016&28.07.2017&Latest"
fn_lga_est_pop <- "32180ds0002_2006-16.xls"
download.file(url_lga_est_pop, fn_lga_est_pop)
lga_est_pop_raw <- read.xlsx2(fn_lga_est_pop, sheetName="Table 3", startRow=9, stringsAsFactors=F)
names(lga_est_pop_raw) <- c("LGACode", "LGA", "2006", "2007", "2008", "2009", "2010", "2011", "2012pr", "2013pr", "2014pr", "2015pr", "2016pr", "BLANK", "2006-2016pr-pct", "2006-2016pr-no", "BLANK2", "Area",	"PopDen2016")
# Begin straightening up the raw data we imported
lga_est_pop_ds <- lga_est_pop_raw %>%
  filter(row_number() < n() - 6) %>% 
  mutate(LGA = gsub(" \\(.\\)","", LGA)) %>% 
  mutate(LGA=toupper(LGA)) %>% 
  mutate(LGA = gsub("-"," ", LGA)) %>% 
  mutate(LGA = mapvalues(LGA,c("CENTRAL HIGHLANDS (QLD)", "FLINDERS (QLD)"), c("CENTRAL HIGHLANDS", "FLINDERS")))
# Check we have a LGA pop record for each LGA in EGM data (should be zero)
unique(e$LGA.Region[!(e$LGA.Region %in% lga_est_pop_ds$LGA)])

# Normalise the data
lga_est_pop_dst <- lga_est_pop_ds %>% select(LGACode:`2016pr`)
lga_est_pop_dsn <- gather(lga_est_pop_dst,year,pop,`2006`:`2016pr`)


# Spatial Data ------------------------------------------------------------

url_LGA_Boundaries <- "http://qldspatial.information.qld.gov.au/catalogue/custom/search.page?q=%22Local%20government%20area%20boundaries%20-%20Queensland%22#"

qld_boundary_sp <- readOGR(
  dsn = "data/State Boundaries MAY 2016/Standard/QLD_STATE_POLYGON_shp.shp",
  layer = "QLD_STATE_POLYGON_shp")

qld_boundary_df <- fortify(qld_boundary_sp)

qld_lga_sp <- readOGR(
  dsn = "data/QSC_Extracted_Data_20170809_220025273000-14836/Local_Government_Areas.shp",
  layer = "Local_Government_Areas")

qld_lga_clip <- gIntersection(qld_lga_sp, qld_boundary_sp, byid = TRUE)
qld_lga_clip_df <- fortify(qld_lga_clip)


# Plotting ----------------------------------------------------------------

qld_lgac_map <- ggplot(qld_lga_clip_df,aes(long,lat,group=group)) + 
  geom_polygon(fill="white",colour="black") +
  coord_equal()
# print is waaayyyy too slow on Mac
#print(qld_lgac_map)
ggsave("qld_lgac_map.pdf", plot = qld_lgac_map)

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

