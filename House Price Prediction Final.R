library(caTools)
library(dplyr)
library(tidyverse)
library(mice) 
library(corrgram)
library(corrplot)
library(olsrr)
library(ggthemes) 

set.seed(1101)

setwd("C:/Users/mrrob/Desktop/Machine Learning York University/Course 1/Project 3 House Price Prediction")

df=read.csv("https://github.com/robinmath/houseprice_prediction_project/raw/main/houses_edited.csv", header = TRUE)
df_to=read.csv("https://github.com/robinmath/houseprice_prediction_project/raw/main/TO_Neighbourhood.csv", header = TRUE)

#df=read.csv("houses_edited.csv", header = TRUE)
#df_to=read.csv("TO_Neighbourhood.csv", header = TRUE)

df_to <- dplyr::select(df_to,-c("X","area_name","desc"))

any(is.na(df[,!(names(df) %in% ("sqft"))]))
any(is.na(df))

df <- dplyr::select(df,-c("index","title","list_price","description","mls","full_link","full_address","final_price_transformed","city_district","final_price_log"))

head(df[,names(df) %in% c("bedrooms","bedrooms_ag","bedrooms_bg")],50)

df <- dplyr::select(df,-c("bedrooms"))

df$type <- factor(df$type, ordered = FALSE)

# impute sqft with relevant variables
# mice_mids <-mice(df[df$type=="Detached",names(df) %in% c("sqft","final_price","bathrooms","parking","bedrooms_num","district_code")],method = 'rf')
# mice_df <- complete(mice_mids)
# df$sqft_mice[df$type=="Detached"] <- mice_df$sqft
# 
# mice_mids <-mice(df[!(df$type=="Detached"),names(df) %in% c("sqft","final_price","bathrooms","parking","type","bedrooms_num","district_code")],method = 'rf')
# mice_df <- complete(mice_mids)
# df$sqft_mice[!(df$type=="Detached")] <- mice_df$sqft

mice_mids <-mice(df[df$type=="Detached",names(df) %in% c("sqft","bathrooms","parking","bedrooms_bg","bedrooms_ag","district_code","final_price","mean_district_income")],method = 'rf')
mice_df <- complete(mice_mids)
df$sqft_mice[df$type=="Detached"] <- mice_df$sqft

mice_mids <-mice(df[!(df$type=="Detached"),names(df) %in% c("sqft","bathrooms","parking","type","bedrooms_bg","bedrooms_ag","district_code","final_price","mean_district_income")],method = 'rf')
mice_df <- complete(mice_mids)
df$sqft_mice[!(df$type=="Detached")] <- mice_df$sqft

# sanity check of imputation
head(df[df$type=="Detached" & is.na(df$sqft),names(df) %in% c("final_price","bathrooms","parking","type","bedrooms_ag","bedrooms_bg","sqft","sqft_mice")],50)
head(df[!(df$type=="Detached") & is.na(df$sqft),names(df) %in% c("final_price","bathrooms","parking","type","bedrooms_ag","bedrooms_bg","sqft","sqft_mice")],50)
summary(df)

df$sqft <- df$sqft_mice
df <- dplyr::select(df,-c("sqft_mice"))

df<-left_join(df,df_to, by= "district_code")

## correlations, check for interaction variables
corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

#remove weak features
df <- dplyr::select(df,-c("district_code"))

#checking for detached and non-detached separately
corrgram(df[df$type=="Detached",], order = TRUE, lower.panel = NULL, upper.panel = panel.pie, text.panel = panel.txt, main="Detached house variable correlations in PC2/PC1 Order")
corrgram(df[!(df$type=="Detached"),], order = TRUE, lower.panel = NULL, upper.panel = panel.pie, text.panel = panel.txt, main="Non-detached house variable correlations in PC2/PC1 Order")

ggplot(data = df, mapping = aes(x=final_price)) +
  geom_histogram(binwidth = 1000, color='purple', fill='blue', alpha=0.5) +
  xlab('final_price') + ylab('Frequency')

#1
model <- lm(final_price ~ ., data = df)
summary(model)
res <- residuals(model)
res <- as.data.frame(res)
ggplot(res,aes(res)) +  geom_histogram(binwidth = 10000, fill='blue',alpha=0.5)
plot(model)

ols_plot_cooksd_chart(model)

#2

detached_df <- dplyr::select(df[df$type=="Detached" & df$final_price<4000000,],-c("type","lat","shopping","health","diversity"))

model <- lm(final_price ~ ., data = detached_df)
summary(model)

detached_df$interaction_effect1 <- detached_df$sqft * detached_df$bathrooms
detached_df$interaction_effect2 <- detached_df$bedrooms_ag * detached_df$bathrooms
detached_df$interaction_effect3 <- detached_df$sqft * detached_df$mean_district_income

detached_df$nonlinear_effect1 <- (detached_df$bathrooms)^2
detached_df$nonlinear_effect2 <- (detached_df$mean_district_income)^0.5

detached_df <- dplyr::select(detached_df,-c("bedrooms_bg","employment"))

#detached_df <- detached_df[-c(932,2268),]

detached_df[,!(names(detached_df) %in% ("long"))] %>%
  gather(-final_price, key = "var", value = "value") %>%
  ggplot(aes(x = final_price, y = value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_few()

sample <- sample.split(detached_df$final_price, SplitRatio = 0.90)
traindetached_df <- subset(detached_df,sample==TRUE)
testdetached_df <- subset(detached_df,sample==FALSE)
model <- lm(final_price ~ ., data = traindetached_df)
final_price_predicted <- predict(model,testdetached_df)

testdetached_df$final_price_predicted <- final_price_predicted
head(testdetached_df[,names(testdetached_df) %in% c("final_price","final_price_predicted","sqft","bedrooms_ag","bathrooms","parking")],50)

mse<-mean((testdetached_df$final_price-testdetached_df$final_price_predicted)^2)
mse^0.5

RSS = sum((testdetached_df$final_price_predicted - testdetached_df$final_price)^2)
TSS = sum((mean(detached_df$final_price) - testdetached_df$final_price)^2)

R2 = 1 - RSS/TSS
R2

#3

nondetached_df <- dplyr::select(df[!(df$type=="Detached"),],-c("community","employment"))

model <- lm(final_price ~.+ sqft:mean_district_income + sqft:type + bedrooms_ag:bathrooms + sqft:bathrooms + I(mean_district_income^0.5), data = nondetached_df)

summary(model)

res <- residuals(model)
res <- as.data.frame(res)
ggplot(res,aes(res)) +  geom_histogram(binwidth = 10000, fill='blue',alpha=0.5)
plot(model)

ols_plot_cooksd_chart(model)
nondetached_df <- nondetached_df[-c(1949,9165),]

nondetached_df[,!(names(nondetached_df) %in% c("type","lat","long"))] %>%
  gather(-final_price, key = "var", value = "value") %>%
  ggplot(aes(x = final_price, y = value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_few()

sample <- sample.split(nondetached_df$final_price, SplitRatio = 0.80)
trainnondetached_df <- subset(nondetached_df,sample==TRUE)
testnondetached_df <- subset(nondetached_df,sample==FALSE)

model <- lm(final_price ~.+ sqft:mean_district_income + sqft:type + bedrooms_ag:bathrooms + sqft:bathrooms + I(mean_district_income^0.5), data = trainnondetached_df)
final_price_predicted <- predict(model,testnondetached_df)

testnondetached_df$final_price_predicted <- final_price_predicted
head(testnondetached_df[,names(testnondetached_df) %in% c("final_price","final_price_predicted","sqft","bedrooms_ag","bathrooms","parking","type")],50)

mse<-mean((testnondetached_df$final_price-testnondetached_df$final_price_predicted)^2)
mse^0.5

RSS = sum((testnondetached_df$final_price_predicted - testnondetached_df$final_price)^2)
TSS = sum((mean(nondetached_df$final_price) - testnondetached_df$final_price)^2)

R2 = 1 - RSS/TSS
R2

