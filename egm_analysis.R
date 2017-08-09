#Summary of all Qld gambling data
url_Allgamblingdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_All-gambling-data.csv'
a<- read.csv(url(url_Allgamblingdata))

#Summary data of total EGM in Qld
url_TotalGamingMachineData = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_Total-Queensland-EGM-data.csv'
b<-read.csv(url(url_TotalGamingMachineData))

#Summary data of total Hotel EGM in Qld
url_TotalhotelEGMdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_Total-hotel-EGM-data.csv'
c<-read.csv(url(url_TotalhotelEGMdata))

#Summary data of total Clubs EGM in Qld
url_TotalclubEGMdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_Total-club-EGM-data.csv'
d<-read.csv(url(url_TotalclubEGMdata))

#Summary data of LGA EGM in Qld
url_LGAEGMdata = 'http://data.justice.qld.gov.au/JSD/OLGR/20170518_OLGR_LGA-EGM-data.csv'
e<-read.csv(url(url_LGAEGMdata))
