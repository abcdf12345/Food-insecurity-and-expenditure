# I think I got this all to work? I might need to work on the acs file in order to get it to match up more with the
# cps file, not certain though. If not I can begin creating categorical variables and 
# data visualizations and then begin to create randomForest, ROCCurve,
# etc. 

rm(list = ls())

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

# ADD STATE AS A CATEGORICAL VARIABLE
# ADD REGION

# P-VALUE, NOT ALWAYS THAT IMPORTANT

# LOOKUP FOOD PANTRIES IN EACH STATE 

# POPULATION PER COUNTY COULD ADD A RURAL INDICATION LEVEL
# ADD IN BOTH ACS AND CPS

# ISSUES WITH THE CPS DATASET, SEEMS THAT THERE ARE ISSUES WITH HOW FEXPEND WAS
# COLLECTED. 

# NEED TO DO AN IF ELSE FOR urban_code
# CREATE A NEW VARIABLE? TECHNICALLY I DON'T NEED TO, I COULD
# JUST FIX THE ALREADY EXISTING URBAN VARIABLE

# This just cleans up the urbanicity variable and removes the NA's from the list while also
# making it a factorized variable not a numeric variable. 

#LRF: order is not what you wanted: turn this intoa n "ordered factor" then r realizes
#there is an inherent ordering, and it will respect that in plots, tables, etc...
#this code should do that: (attribute will follow variable urban_c through to new datasets)

#cps$fsecurity_f = ifelse(cps$fsecurity > 0, "yes", "no")
# CREATE SUB-DATASETS OF CPS FOR FEXPEND AND FSECURITY

cps_fexpend <- cps[!is.na(cps$fexpend),]

# REMOVE ID, Binary Fsecurity and, Factorized Fsecurity, what is weight?

cps_fexpend = subset(cps_fexpend, select = -c(id, weight, fsecurity))

# THIS DOES NOT WORK FORE ZERO INFLATION MODELS, WHY VUONG IS USED.
#anova(fsecurity.glm, test = "Chisq")


# WORK ON FEXPEND, THAT WILL BE THE NEXT SECTION
# IF I HAVE TO CREATE A FORREST I THINK ILL TRY TO USE MY GPU's
# ALONG IN THE PROCESS, THAT MIGHT HELP TIME WISE
# 
fexpend_train.df <- cps_fexpend
# 
# fexpend_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = 3, importance = TRUE)
# 
# keeps = data.frame(m = rep(NA, length(mtry)),
#                    OOB_Err_Rate = rep(NA, length(mtry)))
# 
# for(idx in 1:length(mtry)){
#   print(paste0("Now testing mtry = ", mtry[idx]))
#   fexpend_forest = randomForest(fexpend ~., data = fexpend_train.df, ntree = 1000, mtry = mtry[idx])
#   
#   keeps[idx, "m"] = mtry[idx]
#   
#   keeps[idx, "OOB_Err_Rate"] = mean((predict(fexpend_forest) - cps_fexpend$fexpend)^2)
# }
# 
# qplot(m, OOB_Err_Rate, geom = c("line", "point"), data = keeps) +
#   theme_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")
# 
# fexpend_final_forest = randomForest(fexpend ~ hhsize + urban_c + female + kids + elderly + black + hispanic + education
#                                     + employed + married + disability, data = fexpend_train.df, ntree = 1000, mtry = 2, importance = TRUE)
# 
#saveRDS(fexpend_final_forest, "fexpend_final_forest.RDS")
fexpend_final_forest <- readRDS("fexpend_final_forest.RDS")

varImpPlot(fexpend_final_forest, type = 1)


#  CREATE LOGIT LINKED NORMAL DISTRIBUTION, IF TIME PERMITS COMPARE IT TO ZERO ADJUSTED GAMMA - NEED TO 
# ADD 0.001 TO EVERYTHING AS GAMMA NEEDS DATA BETWEEN 0 AND INFINITY.

#ZAGA_Model = gamlss(formula = y~., sigma.formula = y~., nu.formula = y~., data = cps_fexpend, family = ZAGA())

cps_fexpend$fexpend_0 <- ifelse(cps_fexpend$fexpend == 0, 1, 0)

cps_fexpend_new <- subset(cps_fexpend, fexpend != 0)

fexpend.glm <- glm(fexpend ~ hhsize + elderly + employed + disability + education, data = cps_fexpend, family = gaussian(link = "identity"))

fexpend.glm2 <- glm(fexpend ~ hhsize + elderly + employed + disability + education, data = cps_fexpend_new, family = Gamma(link = "log"))

fexpend.glm3 <- glm(fexpend_0 ~ hhsize + elderly + employed + disability + education, data = cps_fexpend, family = binomial(link = "logit"))

fexpend.glm4 <- glm(fexpend ~ hhsize + elderly + employed + disability, data = cps_fexpend_new, family = Gamma(link = "log"))

fexpend.glm5 <- glm(fexpend ~ hhsize + elderly + employed, data = cps_fexpend_new, family = Gamma(link = "log"))

AIC(fexpend.glm)

AIC(fexpend.glm2)

AIC(fexpend.glm3)

AIC(fexpend.glm4)

AIC(fexpend.glm5)

fexpend.glm2

fexpend.glm3

beta_hat1 <- coef(fexpend.glm2)
exp(beta_hat1)
exp(confint(fexpend.glm2))

beta_hat2 <- coef(fexpend.glm3)
exp(beta_hat2)
exp(confint(fexpend.glm3))


# ANALYSIS FOR FEXPEND - Gamma Count Model

# HHSIZE INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(-.22794) = .7961747. This means that for every new person that lives within the house the amount of money used to
# buy food for each individual decreases by about 20.4% per person compared to the mean.

# ELDERLY INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(-.04794) = .9531918. This means that for every new elderly person within the household, the amount of money in U.S. dollars
# used to buy food for each individual decreases by about 4.68% per person compared to the mean.

# EMPLOYED INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(.06672) = 1.0690010. This means that for every new employed individual within the household, the amount of money in U.S. dollars
# used to buy food for each individual increase by about 6.9% per person compared to the mean.

# DISABILITY INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(-0.06584) = .9362819. This means that for every new employed individual within the household, the amount of money in U.S. dollars
# used to buy food for each individual decreases by about 6.37% per person compared to the mean.

# EDUCATION INTERPRETATION (Count Model): With all other variables held constant, for individuals who purchase food, the mean amount of food expenditure
# changes by a factor of exp(0.12464) = 1.1327362.This means that for every new educated individual within the household, the amount of money in U.S. dollars
# used to buy food for each individual increases by about 13.27% per person compared to the mean.

# HHSIZE INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of
# exp(-0.07890) = .9241349. This means that for every new person within the household, the odds of said household spending $0 on food per person 
# decreases by about 7.6%. 

# ELDERLY INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(-0.01478) = .9853277. This means that for every new elderly person within the household, the odds of said household spending $0 on food per person 
# decreases by about 1.47%. 

# EMPLOYED INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(-0.30615)= .7362754. This means that for every new employed person within the household, the odds of said household spending $0 on food per person
# decreases by about 26.37%.

# DISABILITY INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(0.25147) = 1.2859122. This means that for every new disabled person within the household, the odds of said household spending $0 on food per person
# increases by about 28.59%.

# EDUCATION INTERPRETATION (Zero-Inflation Model(Binary)): With all other variables held constant, the odds of spending $0 changes by a factor of 
# exp(-0.25484) = .7750404. This means that for every new educated person within the household, the odds of said household spending $0 on food per person
# decreases by about 22.50%.


# CREATE IMAGINARY FAMILIES TO DEMONSTRATE DIFFERENCES BETWEEN FAMILY. 

cps_fexpend_f <- cps_fexpend

cps_fexpend_f$disability <- ifelse(cps_fexpend_f$disability > 0, "Yes", "No")

breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8 ,9 ,10, 11, 12, 13)

tags <- c('0','1', '2', '3', '4', '5', '6', '7', '8' ,'9' ,'10', '11', '12')

cps_fexpend_f$hhsize_f <- cut(cps_fexpend_f$hhsize, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)

# ANALYSIS OF VARIABLES
ggplot(data = cps_fexpend, aes (x = fexpend, fill = fexpend))+geom_histogram(binwidth = 5) + labs(x = "Food Expense In USD", y = "Number of Households") +
  scale_x_continuous(labels=scales::dollar_format()) + scale_fill_brewer(palette = "BuGn")


# ANALYSIS OF DISABILITY VARIABLE

ggplot(data = cps_fexpend_f, aes (x = disability, fill = disability))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "If Disabled Individual Within Household", y = "Number of Households", fill = "If Disabled") + scale_fill_brewer(palette = "BuGn")

cps_fexpend_disability <- cps_fexpend_f %>% group_by(disability) %>% summarise(me = mean(fexpend))

ggplot(aes(x = disability, y = me, fill = me), data = cps_fexpend_disability) + geom_bar(stat = "Identity") +
  labs(x = "Number of Disabled Individuals in Household", y = "Average Level of Food Insecurity", fill = "Average Amount in USD") +
  scale_y_continuous(labels=scales::dollar_format()) + scale_fill_distiller(palette = "BuGn")

ggplot() + geom_boxplot(aes(group = disability, x = disability, y = fexpend, fill = disability), data = cps_fexpend_f) +
  labs(x = "If Disabled Person Lives Within Household", y = "Level of Food Expenditure Within Household", fill = "Disability Level") +
  scale_y_continuous(labels=scales::dollar_format()) + scale_fill_brewer(palette = "BuGn")

# ANALYSIS OF ELDERLY VARIABLE
# ADD scale_y_log10() to see how the 


ggplot(data = cps_fexpend, aes (x = elderly))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) + 
  labs(x = "Elderly In Household", y = "Number of Households") +
  scale_fill_brewer(palette = "BuGn")

cps_fexpend_elderly <- cps_fexpend %>% group_by(elderly) %>% summarise(mexp = mean(fexpend))

ggplot(aes(x = elderly, y = mexp, fill = mexp), data = cps_fexpend_elderly) + geom_bar(stat = "Identity") + 
  labs(x = "Number of Elderly in Household", y = "Average Amount of Food Expenditure in Household", fill = "Average Amount in USD") + 
  scale_y_continuous(labels=scales::dollar_format()) + scale_fill_distiller(palette = "BuGn")

ggplot() + geom_boxplot(aes(group = elderly, x = elderly, y = fexpend, fill = elderly), data = cps_fexpend) +
  labs(x = "Number of Elderly in Household", y = "Amount of Food Expenditure in Household - log scale", fill = "# of Elderly") +
  scale_y_continuous(labels=scales::dollar_format()) + scale_y_log10(labels = scales::dollar_format()) + 
  scale_fill_distiller(palette = "BuGn")

display.brewer.all(colorblindFriendly = T)

# ANALYSIS OF EDUCATION VARIABLE

ggplot(data = cps_fexpend, aes(x = education))+ geom_bar(aes(fill = education)) + geom_text(stat = 'count',aes(label=..count..), vjust = -1) +
  labs(x = "Number of Educated Individuals Within Household", y = "Number of Households") +
  scale_fill_distiller(palette = "BuGn")

cps_fexpend_education <- cps_fexpend %>% group_by(education) %>% summarise(med = mean(fexpend))

ggplot(aes(x = education, y = med, fill = med), data = cps_fexpend_education) + geom_bar(stat = "Identity") +
  labs(x = "Number of Educated in Household", y = "Average Amount of Food Expenditure in Household", fill = "Average Food Expense USD") +
  scale_y_continuous(labels=scales::dollar_format()) +  scale_fill_distiller(palette = "BuGn", labels=scales::dollar_format())

ggplot() + geom_boxplot(aes(group = education, x = education, y = fexpend, fill = education), data = cps_fexpend) +
  scale_y_log10(labels = scales::dollar_format()) + labs(x = "Number of Educated in Household", y = "Average Amount of Food Expenditure in Househod - log scale", fill = "Number of Educated") + scale_fill_distiller(palette = "BuGn")

# ANALYSIS OF EMPLOYED VARIABLE

ggplot(data = cps_fexpend, aes(x = employed))+ geom_bar() + geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of employed Individuals Within Household", y = "Number of Households")

cps_fexpend_employed <- cps_fexpend %>% group_by(employed) %>% summarise( Average = mean(fexpend))

ggplot(aes(x = employed, y = Average, fill = Average), data = cps_fexpend_employed) + geom_bar(stat = "Identity") +
  labs(x = "Number of Employed in Household", y = "Average Amount of Food Expenditure Per Household", fill = "Average Amount in USD") +
  scale_y_continuous(labels=scales::dollar_format()) + scale_fill_distiller(palette = "BuGn")

ggplot() + geom_boxplot(aes(group = employed, x = employed, y = fexpend, fill = employed), data = cps_fexpend) + 
  labs(x = "Number of Employed Within Household - log scale", y = "Food Expenditure Per Household", fill = "Number of Employed") +
  scale_y_log10(labels = scales::dollar_format()) + scale_fill_distiller(palette = "BuGn")
  

# ANALYSIS OF HHSIZE VARIABLE
ggplot(data = cps_fsecurity, aes(x = hhsize)) + geom_histogram(binwidth = 1) + 
  #geom_text(stat = 'count', aes(label = ..count..), vjust = -1) +
  labs(x = "Number of Family Members Household", y = "Number of Households")

cps_fexpend_employed <- cps_fexpend_f %>% group_by(hhsize_f) %>% summarise(Average = mean(fexpend))

ggplot(aes(x = hhsize_f, y = Average, fill = Average), data = cps_fexpend_employed) + geom_bar(stat = "Identity") +
  labs(x = "Number of Individuals Within Household", y = "Average Level of Food Insecurity") +
  scale_y_continuous(labels=scales::dollar_format()) + scale_fill_distiller(palette = "BuGn")

ggplot() + geom_boxplot(aes(group = round(hhsize,0), x = round(hhsize,0), y = fexpend), data = cps_fexpend)


ggplot(data = cps_fexpend, aes(x = hhsize, y = fexpend))+geom_jitter()+ labs(x = "Number of Family Members Within Household", y = "Food Expense" ) +
  scale_y_continuous(labels=scales::dollar_format())

# FOR LATER


# ADD Illustrations and add to glm specifically for different presentation

# CREATE PLOTS FOR urban_c 

ggplot(data = cps_fsecurity, aes(x = urban_c)) + 
  geom_bar() + 
  geom_text(stat = 'count', aes(label = ..count..), hjust = -1) +
  labs(x = "", y =  "Count") +#LRF: add labels 
  coord_flip() #the x axis is smooshed - thijs helps with that

#LRF: take a look at this - don't have to create all those temporary datasets
cps_fsecurity %>% 
  group_by(urban_c) %>% 
  summarise(Average = mean(fsecurity)) %>%#LRF: pipe the data right into ggplot!
ggplot(aes(x = urban_c, y = Average, fill = Average)) +
  geom_bar(stat = "Identity")  +
  coord_flip()+ #LRF: again, the x axis was smooshed
  labs(x = "", y = "Proportion of Food Insecure")#LRF: axis labels!

  
ggplot(data = cps_fexpend, aes(x = urban_c)) + 
  geom_bar() + 
  geom_text(stat = 'count', aes(label = ..count..), hjust = -1) +
  coord_flip()+
  labs(x = "", y = "Count")

#SEE HOW I'VE CHANGED THIS
cps_fexpend  %>% 
  group_by(urban_c) %>% 
  summarise(Average = mean(fexpend))%>% #LRF piping data right into plot
ggplot(aes(x = urban_c, y = Average, fill = Average)) + 
  geom_bar(stat = "Identity") +
  coord_flip()+ #LRF
  scale_y_continuous(labels=scales::dollar_format())+#LRF: GOOGLE "ggplot2 scale axis dollar"
labs(x = "", y = "Average Spent Per Week, Person") + #gLRF: ood labels
  theme(legend.position = "none") #LRF: legend not necessary here
#  labs(x = "Number of Individuals Within Household", y = "Average Level of Food Insecurity")


# CREATE GLM WITH URBANICITY INCLUDED FOR BOTH FOOD SECURITY AND EXPENDITURE

urbanicity.glm <- zeroinfl(fsecurity ~ disability + education + elderly + urban_c| disability + education + elderly + urban_c, data = cps_fsecurity)

urbanicity.glm2 <- glm(fexpend ~ hhsize + elderly + employed + disability + education + urban_c, data = cps_fexpend_new, family = Gamma(link = "log"))

urbanicity.glm

urbanicity.glm2

AIC(urbanicity.glm)

# CREATE Heatmap, other cluster based visualizations?


# I NEED TO GET MY PREDICTIONS ONTO ACS THEN I CAN MAKE A MAP OF THE PREDICTIONS 
final_fexpend_forest = readRDS("fexpend_final_forest.RDS")

# BEFORE I MAKE PREDICTIONS I NEED TO DIVIDE EVERYTHING BY HOUSEHOLDS, THIS WILL
# GIVE ME THE NUMBER OF EVERYTHING PER HOUSEHOLD IN EACH BLOCK, WHICH IS WHAT 
# I WANT WHEN MAKING PREDICTIONS USING A FOREST DERIVED FROM THE CPS PER HOUSEHOLD
# DATASET.
acs$fexpend_predictions <- predict(fexpend_final_forest, acs, type = "response")

write.csv(acs, "Ryan_Data/final_acs_fexpend.csv")

acs_test1 = read.csv("Ryan_Data/final_acs_fexpend.csv")

#acs$GEOID = as.character(paste0(acs$GEOID, substr(acs$X, 13, 13)))


#this is block groups w/in tracts
# ia_shp = block_groups(state = "IA")
# 
# county_list = unique(counties("Iowa")$NAME)
# county_list = county_list[order(county_list)]
# all_counties = block_groups(state = 'IA', county = county_list)
# 
# ia_shp_join = left_join(ia_shp, acs, by='GEOID') %>%
#   rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)
# 
# 
# pal_bin = colorBin(
#   palette = "YlOrRd", domain = ia_shp_join$elderly,
#   bins = seq(0, max(ia_shp_join$elderly, na.rm = TRUE), length.out = 9)
# )
# 
# leaflet(ia_shp_join, height = 500, width = 1000) %>%
#   addTiles() %>%
#   addPolygons(
#     fillColor =~ pal_bin(elderly),
#     color = "white",
#     stroke = FALSE,
#     fillOpacity = 0.6,
#     highlight = highlightOptions(
#       color = "black",
#       bringToFront = TRUE
#     )
#   )%>%
#   leaflet::addLegend(
#     pal = pal_bin, values=~elderly,
#     opacity = 0.7, title = "Iowa Elderly",
#     position = "bottomright"
#   )

# REPLACE BOXPLOTS WITH THIS
# geom_jitter(aes(x = your_x, y = your_numeric_y)) + geom_violin(aes(x = your_x, y = your_numeric_y), alpha = I(0.5))

# SHOULD MENTION ECONOMIES OF SCALE IN THE HOUSEHOLD SIZE ANALYSIS

