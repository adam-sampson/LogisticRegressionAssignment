##########
# Fall 2017, Bellarmine University MSA 
# Logistic Regression and PCA Analysis Assignment
# Student: Adam Sampson
##########

library(data.table)

sol.dt <- fread("MarketingCampaignSolicitations.txt",sep="\t",header=TRUE)
geo.dt <- fread("MarketingCampaignGeo+LandArea.txt",sep="\t",header=TRUE)
geokey.dt <- fread("GeoKey.txt",sep="\t",header=TRUE)

summary(sol.dt)
  # MSRP from 300 to 6840
  # OfferPrice from 60 to 70
  # Two dates, Three Product Information Characters, and One record ID.
summary(geo.dt)
  # 