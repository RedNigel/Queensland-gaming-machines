library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scales)
library(ggplot2)
library(gganimate)
library(tweener)
library(forcats)

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

# convenient helper fo tidy data frame names and make
# them "legal" and more convenient to use later on.
tidy_df_names <- function(df){
    # regular expression for first gsub() call
    # find spaces (if found) followed by any punctuation
    # characters if the pattern occurs at the end of the line
    nameCleanRegex = "\\s?[[:punct:]]+$"
    names(df) <- df %>% names %>%
        make.names %>% gsub(nameCleanRegex, "", .) %>%
        gsub("\\.", "_", .) %>% tolower
    return(df)
}

# import data and apply tidy_df_names() name cleaner function
all_gambling_df <- read_csv(url(url_Allgamblingdata)) %>% tidy_df_names
total_egm_df <-read_csv(url(url_TotalGamingMachineData)) %>% tidy_df_names
total_hotel_egm_df <-read_csv(url(url_TotalhotelEGMdata)) %>% tidy_df_names
total_club_egm_df <-read_csv(url(url_TotalclubEGMdata)) %>% tidy_df_names
lg_egm_df<-read_csv(url(url_LGAEGMdata))

## 3. Combining hotel and club sub-segments

# In theory, this data frame should sum up to the same overall
# values as "total_egm_df" above

# add segment identifier column
total_hotel_egm_df = total_hotel_egm_df %>% mutate(segment = "hotel")
total_club_egm_df = total_club_egm_df %>% mutate(segment = "club")

# create conbined egm data from individial segment subsets
# Date stamp uses last day of month as per data definition.
combined_egm_segment = full_join(total_hotel_egm_df,
    total_club_egm_df, all = T) %>%
    separate(col = month_year, into = c("month","year"), sep = " ") %>%
    mutate(month_idx = match(month, month.name),
           day = days_in_month(month_idx)) %>%
    mutate(date_stamp = dmy(paste(day, month, year))) %>%
    arrange(date_stamp)

# plot combined data segments

# a) creates ggplot object containing all data
combined_egm_segment %>% ggplot() +
    aes(x = date_stamp, y = metered_win, fill=segment) +
    geom_bar(stat = "identity")

# b) better option: identical plot to a) but creates
# only the variables needed for plot construction!
combined_egm_segment %>% select(date_stamp, metered_win, segment) %>%
    ggplot() +
    aes(x = date_stamp, y = metered_win, fill=segment) +
    geom_bar(stat = "identity")

# c) Two line plots for club vs hotel
combined_egm_segment %>%
  ggplot(aes(x = date_stamp, y = metered_win, colour = segment)) +
  geom_path() +
  scale_x_date(date_breaks = "1 years") +
  scale_y_continuous(breaks = seq(5000000, 150000000, 10000000),
                     labels = dollar)
# d) Win per machine by club vs hotel.
combined_egm_segment %>%
  mutate(win_per_machine = metered_win/operational_egms) %>%
    ggplot(aes(x = date_stamp, y = win_per_machine, colour = segment)) +
  geom_path() +
  scale_x_date(date_breaks = "1 years") +
  scale_y_continuous(labels = dollar)

# Interesting that takings per machine per month seems to be steadily increasing.
# Clubs dip a little recently. Night-time precint lockout laws in effect?
# Will be very interesting to look at this on a geographic basis.

# e) Are clubs and hotels diverging?
# This is plot c with a 3rd line representing hotel - club
# plotted beneath with a trend line. It looks like maybe there
# is some evidence of hotel takings growing faster than clubs

combined_egm_segment %>%
  group_by(month, year) %>%
  mutate(max_ = max(metered_win), min_ = min(metered_win),
         hotel_club_diff = max(metered_win) - min(metered_win)) %>%
  ggplot() +
  geom_path(aes(x = date_stamp, y = metered_win, colour = segment)) +
  geom_path(aes(x = date_stamp, y = hotel_club_diff)) +
  scale_x_date(date_breaks = "1 years") +
  scale_y_continuous(breaks = seq(5000000, 150000000, 10000000),
                       labels = dollar) +
  stat_smooth(aes(x = date_stamp, y = hotel_club_diff))

# TODO add legend for difference.

## NOTE:.... the following section below is purely optional for interest
## Aim: Check that the combined_egm_df data matches the contents of the
# total_egm_df that we obtained from the web source. This is the lazy-ish
# way to do this without inspecting the data (not always recommended).

# 1. summarize by month year and remove extra columns
# dplyr::funs is my new friend!
# h/t @ https://stackoverflow.com/a/24455439
check_total_egm_df = combined_egm_segment %>% group_by(month_year) %>%
    mutate(segment = NULL, date_stamp = NULL) %>%
    summarise_each(funs(sum))

# 2. sort both data frames by month year column
check_total_egm_df = check_total_egm_df %>% arrange(month_year)
total_egm_df = total_egm_df %>% arrange(month_year)

# 3. check that the data frames are equal
# doesn't return true because of the attribues... but content looks same
# happily, doesn't return row missmatch list
# had to coerce to data.frame to use this base function. all_equal() failed.
all.equal(as.data.frame(check_total_egm_df), as.data.frame(total_egm_df))

# 4. Nigel's animated plot
# Lofi version:
# Need gganimate
total_egm_clean <-
 total_egm_df %>%
  separate(month_year,
           into = c("month", "year"),
           sep = "\\s",
           convert = FALSE) %>%
  mutate(month_idx = match(month, month.name),
         taking_per_machine = metered_win/operational_egms) %>%
  filter(!(year %in% c("2004","2017")))

line_plot <-
    ggplot(total_egm_clean, aes(x = fct_reorder(month, month_idx) , y = taking_per_machine, group = year)) +
    geom_line(aes(frame = year)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(labels = month.abb) +
    ylab("Monthly Takings Per Poker Machine") +
    xlab("Month of the Year")

gganimate(line_plot)

# To have transition animations we're going to need to use
# tweenr: https://github.com/thomasp85/tweenr
# Should be fun!
# test tweenr
