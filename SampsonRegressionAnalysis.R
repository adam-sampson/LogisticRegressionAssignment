##########
# Fall 2017, Bellarmine University MSA 
# Logistic Regression and PCA Analysis Assignment
# Student: Adam Sampson
##########

library(dplyr)
library(ROCR)
library(data.table)
library(lubridate)
source("Functions.R")

options(scipen = 999)

start_time <- Sys.time()

sol.dt <- fread("MarketingCampaignSolicitations.txt",sep="\t",header=TRUE)
geo.dt <- fread("MarketingCampaignGeo+LandArea.txt",sep="\t",header=TRUE)
geokey.dt <- fread("GeoKey.txt",sep="\t",header=FALSE)

# summary(sol.dt)
  # MSRP from 300 to 6840
  # OfferPrice from 60 to 70
  # Two dates, Three Product Information Characters, and One record ID.

# summary(geo.dt)
  # Warning: Some columns have NA's. Need to determine method to deal with them.
  # Warning: Some rows have all 0 values. Need to determine method to deal with them.
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
    
    sol.dt[ , RecordNumber:= as.character(RecordNumber)]

  # Convert for geo.dt
    # Convert RecordNumber to character in census data
    geo.dt[ , RecordNumber := as.character(RecordNumber)]
    
    # Convert all numbers in census data to num and not a mix of num and int
    geo.dt[ , names(geo.dt)[-1] := lapply(.SD,as.numeric), .SDcols = names(geo.dt)[-1]]

#---
# Calculate more useful values from dates in sol.dt
#---
  sol.dt[,isPurchase := !is.na(PurchaseDate)]
  sol.dt[,daysToPurchase := as.numeric(as_date(PurchaseDate) - as_date(MailDate))]
    
#---
# Clean census data for problematic rows
#---
  # Any row with NA values will have issues
  geo.dt <- na.omit(geo.dt)
    
  # If a denominator variable in the next step will be 0, a divide by zero error will occur
  # Denominator Cols: ACSTTPOP0, ACSMALES, ACSFEMALES, MARSTAT0, RACE0, FMHH0, TOTHH0, 
  #                   EDUCAT0, HHINCOM0, ACSTOTHU, UNITS0, VALOWNR0, GROSSRNT0
  geo.dt <- subset(geo.dt, subset = (ACSTTPOP0 != 0 &
                                         ACSMALES != 0 &
                                         ACSFEMALES != 0 &
                                         MARSTAT0 != 0 &
                                         RACE0 != 0 &
                                         FMHH0 != 0 &
                                         TOTHH0 != 0 &
                                         EDUCAT0 != 0 &
                                         HHINCOM0 != 0 &
                                         ACSTOTHU != 0 &
                                         UNITS0 != 0 &
                                         VALOWNR0 != 0 &
                                         GROSSRNT0 != 0))
    
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
  # The percentages should not be scaled or centered. However, the remaining variables
  # will then be too large to compare, so we need to adjust them to the same scale.
    summary(geo.dt)
    # Variables that have values larger than 1 still:
    #   LANDAREA, ACSTTPOP0, MARSTAT0, RACE0, TOTHH0, EDUCAT0, HHINCOM0, MEDHHIN0, AGGHHIN0, PCAPIN0
    #   MDFAMIN0, ACSTOTHU, ACSAVGHHSZ, ACSAVGSZOWN, ACSAVGSZRNT, UNITS0, MDYRBLT0, VALOWNR0, MEDVALO0,
    #   AGGVALO0, GROSSRNT0, MDGRSRN0
    scaleVars <- c('LANDAREA', 'ACSTTPOP0', 'MARSTAT0', 'RACE0', 'TOTHH0', 'EDUCAT0', 'HHINCOM0', 'MEDHHIN0', 'AGGHHIN0', 'PCAPIN0',
                   'MDFAMIN0', 'ACSTOTHU', 'ACSAVGHHSZ', 'ACSAVGSZOWN', 'ACSAVGSZRNT', 'UNITS0', 'MDYRBLT0', 'VALOWNR0', 'MEDVALO0',
                   'AGGVALO0', 'GROSSRNT0', 'MDGRSRN0')
    geo.dt[ , (scaleVars) := lapply(.SD,scale), .SDcols = scaleVars]
    rm(scaleVars)
    summary(geo.dt)

#---
# Selecting variables for PCA from the Census data
# - Some variables are variations of other combinations of variable. Example 1: %Male can be
#   used to calculate %Female = 1-%Male. No new information is added by including both. 
#--- 
  # Exclude ACSAGE17Y0, but include the 0-4, 5-9, 10-14, 15-19 data which has the same basic information.
  # Exclude ACSFEMALES, because the same information is included in ACSMALES
  # Consider whether to Exclude MARSTAT0 because the variable only tells how many of the population are 15 or older, and that
  #      information is already included in the total population and the age groupings. Will this skew PCA?
  # Consider whether to Exclude EDUCAT0 because it is only the population 25 or older. This information is already captured. Will this skew PCA?
  # RecordNumber is an ID value and not part of the PCA.

  # Subset will be: geo.dt[,c(2:4,6:15,17:101)]
  
#---
# Cleaning rows with errors
#---
  # Easiest way to clean missing data is to find rows with errors and remove/subset them
  summary(geo.dt)
  
  # Something in my functions is creating NA values...So let's remove them before PCA
  geo.dt <- na.omit(geo.dt)
  
#---
# Peform the PCA
#---
  geo.pca <- prcomp(geo.dt[,c(2:4,6:15,17:101)], center = FALSE, scale. = FALSE)
  # geo.pca <- prcomp(na.omit(geo.dt[,c(2:4,6:15,17:101)]), center = TRUE, scale. = TRUE)

  # Review the results
  summary(geo.pca)
    # Results are fairly reasonable. PC1 to PC11 have more than 1% of the variance each, and PC1 has 33.4% of the variance
    # By PC5, 79% of the variation is included. By PC9, 91% of the variation is included. By PC11, 95% ". By PC28, 99%.
  plot(geo.pca, type="l")
  abline(h = 1, col="red")
    # Plot shows a pivot point at PC5
  # Estimate eigenvalues by variance
  geo.pca$sdev^2
    # This estimate shows PC1 to PC5 have variance of > 1, so keep PC1 to PC5
  View(geo.pca$rotation[,1:5])
  # biplot(geo.pca, scale = 0)
  
  # Now using only variables in percent form.
  percentonly.pca <- prcomp(geo.dt[,c(4,6:15,18:21,23:31,33:38,40:46,48:63,69:71,76:81,84:90,94:100)], center = FALSE, scale. = FALSE)
  summary(percentonly.pca)
    # PC1 to PC5 have more than 1% each. PC1 has a whopping 80.76%.
  plot(percentonly.pca, type="l")
    # Plot shows a pivot point at PC2 and maybe again at PC5.
  # Estimate eigenvalues by variances
  percentonly.pca$sdev^2
    # This estimate shows only PC1 has variances of > 1, so the model above may have much more info.
  
#---
# Combine the PCA output to the remaining data
#---
  pcaFactors.dt <- cbind(geo.dt[,"RecordNumber"],geo.pca$x)
  
  # Only keep RecordName and PC1 to PC5
  pcaFactors.dt[,c(7:length(names(pcaFactors.dt))) := NULL]
  
  # Combine the PCA factors to the sol.dt dataset
  setkey(sol.dt,RecordNumber)
  setkey(pcaFactors.dt,RecordNumber)
  analyVars <- merge(sol.dt,pcaFactors.dt,all=FALSE)
  setkey(sol.dt,NULL)
  setkey(pcaFactors.dt,NULL)
  
  summary(analyVars)
    # There are many NA values in PurchaseData and daysToPurchase. but not going to use these anyway.
  
  # Check variables we are going to use for NA values
  analyVars <- na.omit(analyVars, 
                       cols = c("Brand", "MSRP", "ProductGroup", "ProductDetail", "OfferPrice", 
                                "isPurchase","PC1","PC2","PC3","PC4","PC5"))
    
#---
# Prepare variables for regression
#---
  # Review the classes before performing analysis
  # OfferPrice is essentially categorical (only 2 options)
  analyVars$OfferPrice <- factor(analyVars$OfferPrice)
  analyVars$Brand <- factor(analyVars$Brand)
  analyVars$ProductGroup <- factor(analyVars$ProductGroup)
  analyVars$ProductDetail <- factor(analyVars$ProductDetail)

  # Set the levels so that a good variable is the default
  # Brand Average Brand A is ok.
  # Product Group Air Conditioner isn't ideal, Washer is more common
  analyVars$ProductGroup <- relevel(analyVars$ProductGroup, ref = "Washer")
  
#---
# Split data into test set and training set
#---
  # use 80% (randomly selected) of the rows in analyVars for training data
  set.seed(42)
  train.dt <- copy(sample_n(analyVars,0.8 * length(analyVars[[1]])))
  test.dt <- anti_join(analyVars,train.dt)
  
#---
# Run first logistic model
#---
  logit.mod <- glm(isPurchase ~ Brand + ProductGroup + ProductDetail + MSRP + OfferPrice + PC1 + PC2 + PC3 + PC4 + PC5,
                   data=train.dt, family = binomial(link="logit"))
  (logit.mod)
  summary(logit.mod)
  # First thing to note is that 14 of the inputs are not defined because of singularities.
  #   PD Compactor, PD Dishwasher, PD Dryer Gas, PD Gas Cooktop, PD Ice Maker, PD Over the Range Microwave
  #   PD Range Hood, PD Refer Unknown, PD Slide in Range Gas, PD Stacked Laundry, PD Upright Freezer,
  #   PD Walloven, PD Washer Top Load, PD Wine Cooler
  
  # Before trying more complex methods, review model without Product Detail (only product Group)
  logit.mod2 <- glm(isPurchase ~ Brand + ProductGroup + MSRP + OfferPrice + PC1 + PC2 + PC3 + PC4 + PC5,
                   data=train.dt, family = binomial(link="logit"))
  summary(logit.mod2)
  # Check the odds ratios
  data.frame(OddsRatio = exp(coef(logit.mod2)))
  # confint(logit.mod)
  # exp(cbind(OddsRatio = coef(logit.mod), confint(logit.mod)))
  
  # Get fitted results for train.dt
  fitted.results <- predict(logit.mod2,newdata = train.dt,type='response')
  fitted.df <- data.frame(prediction = fitted.results,
                               predValue = ifelse(fitted.results > 0.5, 1, 0),
                               trueValue = as.numeric(train.dt$isPurchase))
    # Review the results
    summary(fitted.df)
    
    # # RMSE
    # rmse(fitted.df$trueValue, fitted.df$predValue)
    # # MAE
    # mae(fitted.df$trueValue, fitted.df$predValue)
    # # Mis-classifier
    # (misClasifyError <- mean(fitted.df$predValue != fitted.df$trueValue))
  
    # Review the ROCR performance
    pr <- prediction(fitted.results, train.dt$isPurchase)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    plot(prf)
    
    auc <- performance(pr, measure = "auc")
    auc <- auc@y.values[[1]]
    auc
    
    # Review the lift
    lift.perf <- performance(pr,"lift","rpp")
      # Subsample because that is a lot of data points
      lift.perf@x.values$num <- sample(lift.perf@x.values$num,length(lift.perf@x.values)/100,replace = FALSE)
      lift.perf@y.values$num <- sample(lift.perf@y.values$num,length(lift.perf@y.values)/100,replace = FALSE)
    plot(lift.perf, main="Lift Curve", ylim=c(0,5))
    abline(h = 1, col = "red")
    
  # Get fitted results for test.dt
  fitted.results <- predict(logit.mod,newdata = test.dt,type='response')
  fitted.df <- data.frame(prediction = fitted.results,
                               predValue = ifelse(fitted.results > 0.5, 1, 0),
                               trueValue = as.numeric(test.dt$isPurchase))
    # Review the results
    summary(fitted.df)
  
    # # RMSE
    # rmse(fitted.df$trueValue, fitted.df$predValue)
    # # MAE
    # mae(fitted.df$trueValue, fitted.df$predValue)
    # # Mis-classifier
    # (misClasifyError <- mean(fitted.df$predValue != fitted.df$trueValue))
    
    # Review the ROCR performance
    pr <- prediction(fitted.results, test.dt$isPurchase)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    plot(prf)
    auc <- performance(pr, measure = "auc")
    auc <- auc@y.values[[1]]
    auc
    
    # Review the lift
    lift.perf <- performance(pr,"lift","rpp")
      # Subsample because that is a lot of data points
      lift.perf@x.values$num <- sample(lift.perf@x.values$num,length(lift.perf@x.values)/100,replace = FALSE)
      lift.perf@y.values$num <- sample(lift.perf@y.values$num,length(lift.perf@y.values)/100,replace = FALSE)
    plot(lift.perf, main="Lift Curve", ylim=c(0,5))
    abline(h = 1, col = "red")
    
print(paste("Took",Sys.time()-start_time,"seconds to complete."))
