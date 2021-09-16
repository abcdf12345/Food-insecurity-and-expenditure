# I WILL PUT FSECURITY STUFF HERE AND 
# PUT FEXPEND IN THE ORIGINAL DATA ANALYSIS


rm(list = ls())

# THIS ALLOWS YOU TO RUN THE CODE SIMILAR TO A CLASS IN OTHER IDE's
source("Ryan_Code/data_cleaning.R")

# These are all of the libraries needed for creating a heatmap of areas in danger of food insecurity.
library(ggplot2)
library(dplyr)
library(DT)
library(rmapshaper)
library(leaflet)
library(htmltools)
library(tigris)
library(totalcensus)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(pscl)
library(boot)
library(reshape2)
library(RColorBrewer)
#These are for the zero-inflation model specifically
#install.packages('pscl')
#library(gamlss.dist)

cps_fsecurity <- cps[!is.na(cps$fsecurity),]

cps_fsecurity = subset(cps_fsecurity, select = -c(id, weight, fexpend))


ggplot(data = cps_fsecurity, aes (x = fsecurity))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + labs(x = "Food Insecure", y = "Number of Households")

# ANALYSIS OF DISABILITY VARIABLE

cps_fsecurity$disability_cat = ifelse(cps_fsecurity$disability > 0, "Yes", "No")
cps_fsecurity$disability_cat = as.factor(cps_fsecurity$disability_cat)

cps_fsecurity$fsecurity_cat = ifelse(cps_fsecurity$fsecurity > 0, "yes", "no")
cps_fsecurity$fsecurity_cat = as.factor(cps_fsecurity$fsecurity_cat)

ggplot(data = cps_fsecurity, aes(x = disability_cat, fill = disability_cat))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "Disabled Individual Living Within Household", y = "Number of Households", fill = "If Disabled") + scale_fill_brewer(palette = "Blues")

cps_disability <- cps_fsecurity %>% group_by(disability_cat) %>% summarise(Average = mean(fsecurity))

ggplot(aes(x = disability_cat, y = Average, fill = Average), data = cps_disability ) + geom_bar(stat = "Identity") +
  labs(x = "Disabled Individual Living Within Household", y = "Average Level of Food Insecurity", fill = "Average Level") +
  scale_fill_distiller(palette = "Blues")

ggplot() + geom_violin(aes(group = disability_cat, x = disability_cat, y = fsecurity, fill = disability_cat), data = cps_fsecurity) +
  labs(x = "Disabled Individual Living Within Household", y = "Level of Food Insecurity", fill = "If Disabled") + 
  scale_fill_brewer(palette = "Blues")

# ANALYSIS OF ELDERLY VARIABLE

ggplot(data = cps_fsecurity, aes(x = elderly, fill = elderly))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "Number of Elderly in Household", y = "Number of Households") + scale_fill_fermenter(palette = "Blues") 

cps_elderly <- cps_fsecurity %>% group_by(elderly) %>% summarise(meld = mean(fsecurity))

ggplot(aes(x = elderly, y = meld, fill = elderly), data = cps_elderly) + geom_bar(stat = "Identity") + 
  labs(x = "Number of Elderly in Household", y = "Average Level Of Food Insecurity", fill = "Number of Elderly") +
  scale_fill_distiller(palette = "Blues")

ggplot() + geom_violin(aes(group = elderly, x = elderly, y = fsecurity, fill = elderly), data = cps_fsecurity) + 
  scale_y_log10() + scale_fill_distiller(palette = "Blues") + labs(x = "Number of Elderly Within Household", y = "Food Security Level Per Household - Log Scale", fill = "Number of Elderly")


# ANALYSIS OF EDUCATION VARIABLE

ggplot(data = cps_fsecurity, aes(x = education))+ geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) +
  labs(x = "Number of Educated Individuals Within Household", y = "Number of Households")

cps_education <- cps_fsecurity %>% group_by(education) %>% summarise(med = mean(fsecurity))

ggplot(aes(x = education, y = med, fill = education), data = cps_education) + geom_bar(stat = "Identity") +
  labs(x = "Number of Educated Individuals Within Household", y = "Average Level of Food Insecurity", fill = "Number of Educated") +
  scale_fill_distiller(palette = "Blues")

ggplot() + geom_violin(aes(group = education, x = education, y = fsecurity, fill = education), data = cps_fsecurity) +
  scale_y_log10() + scale_fill_distiller(palette = "Blues") + labs(x = "Number of Individuals With Associates or Higher Within Household", y = "Food Security Level Per Household - log scale", fill = "Number of Educated")

# ANALYSIS OF EMPLOYED VARIABLE

ggplot(data = cps_fsecurity, aes(x = employed))+ geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Employed Individuals Within Household", y = "Number of Households")

cps_employed <- cps_fsecurity %>% group_by(employed) %>% summarise(memp = mean(fsecurity))

ggplot(aes(x = employed, y = memp, fill = employed), data = cps_employed) + geom_bar(stat = "Identity") +
  labs(x = "Number of Employed Individuals Within Household", y = "Average Level of Food Insecurity", fill = "Number of Employed") + scale_fill_distiller(palette = "Blues")

ggplot() + geom_boxplot(aes(group = employed, x = employed, y = fsecurity, fill = employed), data = cps_fsecurity) +
  scale_y_log10() + scale_fill_distiller(palette = "Blues") + labs(x = "Number of Employed Individuals Within Household", y = "Food Security Level Per Household - log scale", fill = "Number of Employed")

# ANALYSIS OF HHSIZE VARIABLE

ggplot(data = cps_fsecurity, aes(x = hhsize)) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Family Members Within Household", y = "Number of Households")

# THIS IS A BETTER WAY TO BIN BY ROUNDING THE HHSIZE VARIABLE, INSTEAD OF MAKING BINS MANUALLY, WHICH CAN BE TEDIOUS FOR LARGER 
# VARIATIONS IN VALUE.
ggplot(data = cps_fsecurity, aes(x = round(hhsize,0))) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Family Members Within Household", y = "Number of Households")

ggplot(data = cps_fexpend, aes(x = fexpend))+geom_histogram(binwidth = 5)+ labs(x = "Food Expense", y = "Numer of Households" )



# Create the Forrest

train.df = cps_fsecurity


# This will create a beginner forest, but we need to tune the forest so that we
# can determine the correct number of _________ (whatever mtry stands for, ntree doesn't
# change at all.)
# 
# fsecurity_forest = randomForest(fsecurity ~ female + kids + elderly + black + hispanic +
#                                   education + employed + married + disability + hhsize + urban_c, data = train.df,
#                                 ntree = 1000, mtry = 3, importance = T)
# 
# 
# # CREATE THE MTRY STUFF AND FOREST
# 
# # Set up mtry to be
# 
# mtry = c(1:(ncol(cps_fsecurity) - 1))
# 
# # Make room for B, OOB ERROR
# # Why is it ntree = rep and not m = rep? Is that because of the
# # difference in the type of response variable?
# keeps <- data.frame(m = rep(NA, length(mtry)),
#                     OOB_Err_Rate = rep(NA, length(mtry)))
# 
# for(idx in  1:length(mtry)){
#   print(paste0("Now testing mtry = ", mtry[idx]))
#   tempForest = randomForest(fsecurity ~female + kids + elderly + black + hispanic +
#   education + employed + married + disability + hhsize + urban_c,
#                             data = cps_fsecurity,
#                             mtry = mtry[idx])
# 
# keeps[idx, "m"] = mtry[idx]
# 
# # We do this since we are using a continuous response variable rather than a binary categorical
# # variable.
# keeps[idx, "OOB_Err_Rate"] = mean((predict(tempForest) - cps_fsecurity$fsecurity)^2)
# 
#   }
# 
# qplot(m, OOB_Err_Rate, geom = c("line", "point"), data = keeps) +
#   theme_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")
# 
# 
#
# OOB Error Rate is lowest at 2 it seems, so I'll go with 2.
# final_forest = randomForest(fsecurity ~ female + kids + elderly + black + hispanic +
#                               education + employed + married + disability + hhsize + urban_c, data = train.df,
#                             ntree = 1000, mtry = 2, importance = T)

#saveRDS(final_forest, "final_forest.RDS")
final_forest <- readRDS("final_forest.RDS")

varImpPlot(final_forest, type = 1)



# ZERO-INFLATED POISSON WITH REGARDS TO THE FSECURITY DATA

# DO THE VARIABLES NEED TO BE FACTORS? WHY?

# Negative binomial model vs logit part of model, which one goes where?
# I just made multiple models and then I plan to test them all against one another
# THE STRONGEST SHALL SURVIVE!!! (LOWEST AIC VALUE)

# THIS IS THE SELECTED MODEL, RIGHT NOW, WE WILL NOW INTERPRET THE COEFFICIENTS FOR THIS MODEL.
fsecurity.glm =  zeroinfl(fsecurity ~ disability + education | disability + education, data = cps_fsecurity)

fsecurity.glm2 = zeroinfl(fsecurity ~ disability + education + elderly | disability + education + elderly, data = cps_fsecurity)

fsecurity.glm3 = zeroinfl(fsecurity ~ disability + education + elderly + employed | disability + education + employed, data = cps_fsecurity)

fsecurity.glm4 = zeroinfl(fsecurity ~ disability + education +  elderly + employed + hhsize | disability + education + employed + hhsize, data = cps_fsecurity)

urbanicity.glm <- zeroinfl(fsecurity ~ disability + education + elderly + urban_c| disability + education + elderly + urban_c, data = cps_fsecurity)

fsecurity.glm2
beta_hat <- coef(fsecurity.glm2)
exp(beta_hat)
exp(confint(fsecurity.glm2))

# Disability Interpretation (Count Model): With all other variables held constant,for vulnerable individuals, the level of food insecurity increases by 
# a factor of exp(.2593606) = 1.2961011 if there is a disabled person living within the house.This means that if there is a disabled person
# living within the house, food insecurity of the house decreases from the mean by about 30%.

# Education Interpretation (Count Model): With all other variables held constant, for vulnerable individuals, the mean amount of food insecurity decreases by 
# a factor of exp(-.1114594) = .8945277 for every educated person within the house. This means that for every new educated individual
# living within the house, food insecurity of the house decreases from the mean by about 10.55%.

# Elderly Interpretation (Count Model): With all other variables held constant, for vulnerable individuals, the level of food insecurity decreases by a factor of
# exp(-.1748214) = .8396069 for every educated person within the house. This means that for every new elderly person living within
# the house, food insecurity of the house decreases from the mean by about 16.04%.

# Disability Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(-1.0453348) = 0.3515741. This means that the odds
# of a house with a disabled person being food secure is 64.84% less likely than in other households. 

# Education Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(0.7795625) = 2.1805181. This means that the odds 
# that a house is food secure increases by 118% for every person with an associates degree or higher. 

# Elderly Interpretation (Zero-Inflation Model): With all other variables held constant, the odds that a household is within the 
# "Certain Zero" Group, meaning that they are certain to be food secure is exp(6967225) = 2.0071634.  This means that the odds
# that a house is food secure increases by 100% for every elderly person within the house.

# THIS IS USED TO TEST BETWEEN MODELS
# THERE'S GOT TO BE A BETTER WAY, THIS WOULD TAKE TOO LONG, WHAT GOES INTO THE LOGIT AREA, 
# WHAT GOES INTO THE OTHER AREA, I FEEL LIKE I'M FORGETTING SOMETHING.
# vuong(fsecurity.glm, fsecurity.glm3), Don't use this, just use AIC values for comparison
# Vuong, which stands for Vuong's closeness test (I believe), which uses the Kullback - Leibler
# Information Criterion. 

# COLLECT AIC's INDIVIDUALLY

AIC(fsecurity.glm)
# AIC = 

AIC(fsecurity.glm2)
# AIC = 

AIC(fsecurity.glm3)
# AIC = 

AIC(fsecurity.glm4)
# AIC = 

# It seems that fsecurity.glm2 is the best

summary(fsecurity.glm)

summary(fsecurity.glm3)

final_forest = readRDS("final_forest.RDS")

acs$fsecurity_predictions <- predict(final_forest, acs, type = "response")

write.csv(acs, "Ryan_Data/final_acs_fsecurity.csv")

# BRING THIS INTO DASHBOARD AS IS, DONT FORGET TO RE-FACTORIZE THE URBANICITY METRIC
#acs_test = read.csv("Ryan_Data/final_acs_fsecurity.csv")


