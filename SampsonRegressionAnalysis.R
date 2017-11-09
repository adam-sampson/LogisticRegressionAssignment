##########
# Fall 2017, Bellarmine University MSA 
# Logistic Regression and PCA Analysis Assignment
# Student: Adam Sampson
##########

library(data.table)
library(lubridate)

sol.dt <- fread("MarketingCampaignSolicitations.txt",sep="\t",header=TRUE)
geo.dt <- fread("MarketingCampaignGeo+LandArea.txt",sep="\t",header=TRUE)
geokey.dt <- fread("GeoKey.txt",sep="\t",header=TRUE)

summary(sol.dt)
  # MSRP from 300 to 6840
  # OfferPrice from 60 to 70
  # Two dates, Three Product Information Characters, and One record ID.

summary(geo.dt)
  # 

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

