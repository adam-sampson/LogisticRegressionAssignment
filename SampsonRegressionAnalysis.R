##########
# Fall 2017, Bellarmine University MSA 
# Logistic Regression and PCA Analysis Assignment
# Student: Adam Sampson
##########

library(data.table)
library(lubridate)
source("Functions.R")

start_time <- Sys.time()

sol.dt <- fread("MarketingCampaignSolicitations.txt",sep="\t",header=TRUE)
geo.dt <- fread("MarketingCampaignGeo+LandArea.txt",sep="\t",header=TRUE)
geokey.dt <- fread("GeoKey.txt",sep="\t",header=FALSE)

# summary(sol.dt)
  # MSRP from 300 to 6840
  # OfferPrice from 60 to 70
  # Two dates, Three Product Information Characters, and One record ID.

# summary(geo.dt)
  # Census data is in counts per block group. 
  # Ages have overlapping categories
  # Classes (num vs int) are mismatched

#---
# Convert data classes
#---
  # Convert for sol.dt
    # Convert dates to date format
    dateVars <- c("MailDate","PurchaseDate")
    sol.dt[ , (dateVars) := lapply(.SD,parse_date_time,c('ymd HMS')), .SDcols = dateVars]
    rm(dateVars)

  # Convert for geo.dt
    # Convert RecordNumber to character in census data
    geo.dt[ , RecordNumber := as.character(RecordNumber)]
    
    # Convert all numbers in census data to num and not a mix of num and int
    geo.dt[ , names(geo.dt)[-1] := lapply(.SD,as.numeric), .SDcols = names(geo.dt)[-1]]

#---
# Convert census variables to percents
# - This step is performed because each row has its own set of counts and the counts
#   from one row do not relate to another row. Howewever, percentages of each variable
#   are comparable. Example comparing 1400 males to 900 males seems like a large difference
#   but if we change that to percents we may end up with 59% males compared to 60% males if
#   the second number is in a more sparsly populated block group.
#---
  # Columns 4: 14 are the age count columns
  geo.dt[ , names(geo.dt)[4:14] := .SD / ACSTTPOP0, .SDcols = names(geo.dt)[4:14]] 
    # geo.dt[1:10 , c(4,6:14)]
    # 
  
  # Calculate male percent based on formula, but then can't use same formula for female
  geo.dt[ , ACSMALES := ACSMALES / (ACSMALES + ACSFEMALES)]
  geo.dt[ , ACSFEMALES := 1 - ACSMALES]
  
  # Columns 18:21 are marital status
  geo.dt[ , names(geo.dt)[18:21] := .SD / MARSTAT0, .SDcols = names(geo.dt)[18:21]]
  
  # Columns 23:31 are Race0
  geo.dt[ , c(23:31) := .SD / RACE0, .SDcols = c(23:31)]
  
  # Before adjusting FMHH0 and NFMHH0 they are used in calculations for whether married or single parent
  # Columns 34:36 are breakouts of FMHH0
  geo.dt[ , c(34:36) := .SD / FMHH0, .SDcols = c(34:36)]
  
  # Column SINGLE0 is a percentage of NOFMHH0
  geo.dt[ , SINGLE0 := SINGLE0 / NOFMHH0]
  
  # Columns FMHH0 and NFMHH0 add up to TOTHH0, but they are in a strange order
  setcolorder(geo.dt,c(1:33,37,34:36,38:101))
  geo.dt[ , FMHH0 := FMHH0 / TOTHH0]
  geo.dt[ , NOFMHH0 := 1 - FMHH0]
  
  
  
  # Columns 40:46 add up to EDUCAT0 column 39
  geo.dt[ , c(40:46) := .SD / EDUCAT0, .SDcols = c(40:46)]
  
  # HHINCOM0 col 47 is the sum of 48:63
  geo.dt[ , c(48:63) := .SD / HHINCOM0, .SDcols = c(48:63)]
  
  # ACSTOTHU col 68 is the sum of 69:71
  geo.dt[ , c(69:71) := .SD / ACSTOTHU, .SDcols = c(69:71)]
  
  # ACSAVGHHSZ is average household size. Needs to be scaled.
  geo.dt[ , ACSAVGHHSZ := scale(ACSAVGHHSZ)]
  
  # UNITS0 = 76:81
  geo.dt[ , c(76:81) := .SD / UNITS0, .SDcols = c(76:81)]
  
  # VALOWNR0 83 is counts of 84:90
  geo.dt[ , c(84:90) := .SD / VALOWNR0, .SDcols = c(84:90)]

  # GROSSRNT is 94:100
  geo.dt[ , c(94:100) := .SD / GROSSRNT0, .SDcols = c(94:100)]
  
#---
# If scaling is desired, scaling can be done here.
#
# - For regression (linear or logistic) the advantage to scaling is to *help* make
#   it easier to estimate the relative effect size of different variables by inspection.
#   Scaling also makes it possible to use some automated feature selection methods.
#
# - For PCA it is possible to either use the correlation or covariance matrix for the
#   computation. We can scale the data and then use the covariance, or we can use the raw
#   data and use the correlation matrix. Covariance should be used (methodically) if 
#   variables actually have the same units of measure. Example is if two variables are
#   in years to complete and one variable has a range or 2-5 years and the other has a 
#   range of 4-11 years. Using the correlation matrix would standardize both of these
#   so they have the same effect, but covariance matrix will capture that they are
#   measuring the same thing and the scale difference is in fact significant for the analysis.
#---
  # Scaling sol.dt features
    # varToScale <- c("MSRP",
    #                 "OfferPrice")
    # sol.dt[ , (varToScale) := lapply(.SD,scale), .SDcols = varToScale]
    # rm(varToScale)
    # # print(sol.dt$MSRP[1])
    # # print(unscale(sol.dt$MSRP)[1])
  
  # Scaling geo.dt features
  # For the variables going into PCA the only reason to scale and use the covariance matrix
  # is to maintain the difference in counts between households and population.
    # MEDVALO0 is median value of owner occupied so scale it
    # geo.dt[ , MEDVALO0 := scale(MEDVALO0)]
    
    # AGGVALO0 is avg value of owner occupied so scale it
    # geo.dt[ , AGGVALO0 := scale(AGGVALO0)]
    
    # MDGRSRN0 is median gross rent and needs to be scaled
    # geo.dt[ , MDGRSRN0 := scale(MDGRSRN0)]
    
    # MDYRBLT0 needs to be scaled, median year built
    # geo.dt[ , MDYRBLT0 := scale(MDYRBLT0)]
    
    # Scale land area
    # geo.dt[ , LANDAREA := scale(LANDAREA)]

#---
# Selecting variables for PCA from the Census data
# - Some variables are variations of other combinations of variable. Example 1: %Male can be
#   used to calculate %Female = 1-%Male. No new information is added by including both. 
#--- 
  # Exclude ACSAGE17Y0, but include the 0-4, 5-9, 10-14, 15-19 data which has the same basic information.
  # Exclude ACSFEMALE, because the same information is included in ACSMALES
  # Exclude MARSTAT0 because the variable only tells how many of the population are 15 or older, and that
  #      information is already included in the total population and the age groupings
  # Exclude RACE0 because it is the population variable over again (with possible adjustments for unknown)
  # Exclude EDUCAT0 because it is only the population 25 or older. 
  # Exclude HHINCOM0 because it is the same informatio as TOTHH0.
  # 
  
  
print(paste("Took",Sys.time()-start_time,"seconds to complete."))
