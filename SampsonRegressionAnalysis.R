##########
# Fall 2017, Bellarmine University MSA 
# Logistic Regression and PCA Analysis Assignment
# Student: Adam Sampson
##########

library(data.table)
library(lubridate)

sol.dt <- fread("MarketingCampaignSolicitations.txt",sep="\t",header=TRUE)
geo.dt <- fread("MarketingCampaignGeo+LandArea.txt",sep="\t",header=TRUE)
geokey.dt <- fread("GeoKey.txt",sep="\t",header=FALSE)

summary(sol.dt)
  # MSRP from 300 to 6840
  # OfferPrice from 60 to 70
  # Two dates, Three Product Information Characters, and One record ID.

summary(geo.dt)
  # Census data is in counts per block group. 
  # Ages have overlapping categories

# Convert dates to date format
  dateVars <- c("MailDate","PurchaseDate")
  sol.dt[ , (dateVars) := lapply(.SD,parse_date_time,c('ymd HMS')), .SDcols = dateVars]
  rm(dateVars)

# Scale variables that should be scaled
  # print(sol.dt$MSRP[1])
  varToScale <- c("MSRP",
                  "OfferPrice")
  sol.dt[ , (varToScale) := lapply(.SD,scale), .SDcols = varToScale]
  rm(varToScale)
  # print(sol.dt$MSRP[1])
  
  # Create an unscale function in case we need it later.
  unscale <- function(in.vec) {
    return(in.vec * attr(in.vec, 'scaled:scale') + attr(in.vec,'scaled:center'))
  }
  # print(unscale(sol.dt$MSRP)[1])
  
# Convert RecordNumber to character in census data
  geo.dt[ , RecordNumber := as.character(RecordNumber)]

# Convert all numbers in census data to num and not a mix of num and int
  geo.dt[ , names(geo.dt)[-1] := lapply(.SD,as.numeric), .SDcols = names(geo.dt)[-1]]

# Convert census variables to percents
  # Columns 4: 14 are the age count columns
  geo.dt[ , names(geo.dt)[4:14] := .SD / ACSTTPOP0, .SDcols = names(geo.dt)[4:14]] 
    # geo.dt[1:10 , c(4,6:14)]
    # rm(ageVars)
  
  # Calculate male percent based on formula, but then can't use same formula for female
  geo.dt[ , ACSMALES := ACSMALES / (ACSMALES + ACSFEMALES)]
  geo.dt[ , ACSFEMALES := 1 - ACSMALES]
  
  # Columns 18:21 are marital status
  geo.dt[ , names(geo.dt)[18:21] := .SD / MARSTAT0, .SDcols = names(geo.dt)[18:21]]
  