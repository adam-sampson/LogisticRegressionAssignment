library(data.table)

sol.dt <- fread("MarketingCampaignSolicitations.txt",sep="\t",header=TRUE)
geo.dt <- fread("MarketingCampaignGeo+LandArea.txt",sep="\t",header=TRUE)

