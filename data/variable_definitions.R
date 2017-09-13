# Sources:
# https://data.qld.gov.au/dataset/gaming-machine-data-by-local-government-areas/resource/234afa8f-cb89-4274-89f7-356860432171
# https://data.qld.gov.au/dataset/total-queensland-gaming-machine-data/resource/ce543f52-a560-423c-8c0d-deaece839f73
# https://data.qld.gov.au/dataset/gaming-machine-data-by-statistical-area-4/resource/62d1bf51-002c-4a5f-b6dc-0f5ca1df03b7

# Definitions, explanations and disclaimer for the Open Data provided by the # Office of Liquor and Gaming Regulation (OLGR) 

variable_defs <- list(
`Month Year` =  "The month and year from which the gambling data is provided.",
`Approved Sites` = "The number of venues approved to operate electronic gaming machines.",
`LGA Region Name` = "The name of the Local Government Area",
`SA4 Region Name` = "The name of the Statistical Area 4. As defined in the Australian Statistical Geography Standard (1270.0.55.003), published by the Australian Bureau of Statistics.",
`Operational Sites` = "The number of venues that were operating electronic gaming machines on the last day of the relevant month.",
`Approved EGMs` = "The maximum number of electronic gaming machines the venue is approved to operate.",
`Operational EGMs` = "The number of electronic gaming machines operating at the venue on the last day of the revelant month.",
`Metered Win` = "The amount of money lost by players of eletronic gaming machines."
)

#For privacy reasons, monthly metered win totals for regions with less than 5 operating sites are not released.

# Note: Precautions have been taken to ensure that the information in this product is accurate. However, the Queensland Government accepts no liability for the accuracy of the information nor its use or the reliance placed on it. Queensland Government information in this product is subject to change without notice.

# For more information visit www.olgr.qld.gov.au
