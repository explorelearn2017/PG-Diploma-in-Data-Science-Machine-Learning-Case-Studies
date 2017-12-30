# Prediction algorithm for Automobile
#===========================================================================================

##################################################################################################################
# Loading the required libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(scales)
library(Hmisc)
library(MASS)
library(car)

###################################################################################################################
####### Data Understanding

 # 1. Set the working directory to right path
setwd("~/Google Drive ML/Machine Learning & Data Science/(a) Upgrad Course - Data Science, Machine Learning/04 Assignments/04 Automobile Case Study/Inputs")

 # 2. Load the dataset into R environment
auto <- read.csv("CarPrice_Assignment.csv", header = T, stringsAsFactors = FALSE)
#View(auto)

 # structure of data shows the data types
str(auto)
summary(colnames(auto)) # No. of Columns

 # 3. Checking the first few records 
head(auto)
auto1 <- auto # copy of original dataset

##################################################################################################################
# Understanding data in term of business understanding #####

## Car Insurance Characteristics

# symboling

## Car Charcteristics - All the rest of the variables (except price & symboling)

### Meta Data ##
# If you glance the meta data,you could find two different types of variables such as 
# 1. Variables related to car characteristics
# 2. Variable related to car insurance risk rating


# Business Objective
# The company wants to understand the factors affecting the pricing of cars in US 
# 1. Identify variables that helps in predicting car price
# 2. How close does the variables identified can predict car price
# Using this model, company would like to change in the design of the car accordingly and also, modify its strategy

#################################################################################################################

# Duplicates Check

count(unique.data.frame(auto)) == count(auto)

# Remove Car_Id Variable - It is an variable that helps to check duplicates apart from that not useful for prediction

auto <- auto[,-1] 

#################################################################################################################

# Treating invalid values, any variable having more than 15% of data points 
# missing is not eligible for imputation hence it makes sense to compute and drop those variables

missing_values <- auto %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

 # Result - No Missing values in any columns

#################################################################################################################   

# Count of NA values by column: just to verify if the NA values cleared

auto %>%
  summarise_all(funs(sum(is.na(.))))

sum(is.na(auto))  # Result - No "NA's"

#################################################################################################################   
######################################### Data Cleaning #########################################################

# symboling is generally associated to a price of the car. It is usually dependent on various independent variables of
# car characteristics to determine the riskiness of the car and hence, it couldn't be a useful variable for analysis

levels(as.factor(auto1$symboling))  # 6 Levels exists -2,-1,0,1,2,3
auto <- auto[,-1]  # Removing Symboling currently. Will append in the final model to check it has any impact on R2

# Let's extract the Car Manufacturing Company Name and store in the same variable as "CarName"

auto$CarName <- sapply(strsplit(auto$CarName, " "), "[", 1)
levels(as.factor(tolower(auto$CarName)))
auto$CarName <- ifelse(auto$CarName == "maxda", "mazda", ifelse(auto$CarName == "porcshce", "prosche", ifelse(auto$CarName == "toyouta", "toyota", ifelse(auto$CarName == "vokswagen", "volkswagen", ifelse(auto$CarName == "vw", "volkswagen", auto$CarName)))))
levels(as.factor(tolower(auto$CarName)))


# Let's convert all the character type of variables to factor

auto[sapply(auto, is.character)] <- lapply(auto[sapply(auto, is.character)],tolower)
auto[sapply(auto, is.character)] <- lapply(auto[sapply(auto, is.character)],as.factor)

# Let's convert all the integer type of variables to numeric

auto[sapply(auto, is.integer)] <- lapply(auto[sapply(auto, is.integer)],as.numeric)


# convert factors with 2 levels to numerical variables

levels(auto$fueltype) <- c(1,0) # Diesel = 1, Gas = 0
auto$fueltype <- as.numeric(levels(auto$fueltype))[auto$fueltype]

levels(auto$aspiration) <- c(1,0) # std = 1, turbo = 0
auto$aspiration <- as.numeric(levels(auto$aspiration))[auto$aspiration]

levels(auto$doornumber) <- c(1,0) # four = 1. two = 0
auto$doornumber <- as.numeric(levels(auto$doornumber))[auto$doornumber]

levels(auto$enginelocation) <- c(1,0) # front = 1, rear = 0
auto$enginelocation <- as.numeric(levels(auto$enginelocation))[auto$enginelocation]

# Create the dummy variables for Carbody, Drivewheel, Enginetype, Cylindernumber, Fuelsystem

 # Carbody has convertible, hardtop, hatchback, sedan, wagon values

Carbody1 <- data.frame(model.matrix( ~carbody, data = auto))
Carbody1 <- Carbody1[,-1]

 # Drivewheel has 4wd, fwd, rwd

drivewheel1 <- data.frame(model.matrix( ~drivewheel, data = auto))
drivewheel1 <- drivewheel1[,-1]

 # Engine Type has values - dohc, dohcv, l, ohc, ohcf, ohcv, rotor

enginetype1 <- data.frame(model.matrix( ~enginetype, data = auto))
enginetype1 <- enginetype1[,-1]

 # Cylindernumber has values - 8, 5, 4, 6, 3, 12, 2

cylindernumber1 <- data.frame(model.matrix( ~cylindernumber, data = auto))
cylindernumber1 <- cylindernumber1[,-1]

# Fuelsystem has values 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spf

fuelsystem1 <- data.frame(model.matrix( ~fuelsystem, data = auto))
fuelsystem1 <- fuelsystem1[,-1]

# Car company

carcompany1 <- data.frame(model.matrix( ~CarName, data = auto))
carcompany1 <- carcompany1[,-1]


# Combine the dummy variables and the numeric columns of housing dataset, in a new dataset called housing_1

addcolumns <- c(Carbody1, drivewheel1, enginetype1, cylindernumber1, fuelsystem1, carcompany1)
auto_1 <- cbind(auto, addcolumns)

removecolumns <- c("carbody", "drivewheel", "enginetype", "cylindernumber", "fuelsystem")
auto_1 <- auto_1[,!colnames(auto_1) %in% removecolumns] # The orginal variables from which dummy variables created

rm(Carbody1, cylindernumber1, drivewheel1, enginetype1, fuelsystem1, addcolumns, removecolumns, missing_values)

# Removing "CarName" column as the objective of prediction outcome is used to modify the design of the car as per US market
# Brand value, in general, will add goodwill as additional price addition beyond cost of the car
# However, here our focus is on design of the car

auto_1 <- auto_1[,-1]  # Removing CarName

# Let us check the structure of dataset

str(auto_1)

##############################################Univariate & Bivariate function#########################################

# Function for distribution of categorical variables (plotting bar charts)

univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    ) 
}

# facet for categorical vars
categorical_bivariate <- function(dataset, var, var_name){
  plot_bi = ggplot(dataset, aes(x=var))+geom_bar()
  return(plot_bi)
}

# facet for continuous vars
continuous_bivariate <- function(dataset, var, var_name){
  plot_cont = ggplot(dataset, aes(x = 1, y = var))+geom_boxplot()
  return(plot_cont)
}

############################################# EDA #####################################################################

## Checking for price distribution and outliers

continuous_bivariate(auto_1, auto_1$price, "price")
summary(auto_1$price)
quantile(auto_1$price, seq(0, 1, 0.01))
#replace1 <- quantile(auto_1$price, 0.75) + (IQR(auto_1$price)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$price[which(auto_1$price > 36809.6)] <- 36809.6  # Sharp Increase in values from 98% to 99%
 # Hence, Replace outliers of 99% and 100% with 98% value
continuous_bivariate(auto_1, auto_1$price, "car price")

## Checking for wheelbase distribution and outliers

continuous_bivariate(auto_1, auto_1$wheelbase, "Wheelbase")
# seems to contain outliers
summary(auto_1$wheelbase)
quantile(auto_1$wheelbase, seq(0, 1, 0.01)) 
replace1 <- quantile(auto_1$wheelbase, 0.75) + (IQR(auto_1$wheelbase)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$wheelbase[which(auto_1$wheelbase > replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$wheelbase, "Wheelbase")

## Checking for carlength,carheight,carwidth distribution and outliers

continuous_bivariate(auto_1, auto_1$carlength, "carlength")
summary(auto_1$carlength)
quantile(auto_1$carlength, seq(0, 1, 0.01))
replace1 <- quantile(auto_1$carlength, 0.25) - (IQR(auto_1$carlength)*1.5) # replacing outliers (with Q1 - IQR*1.5)
auto_1$carlength[which(auto_1$carlength < replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$wheelbase, "Car Length")

continuous_bivariate(auto_1, auto_1$carwidth, "carwidth")
summary(auto_1$carwidth)
quantile(auto_1$carwidth, seq(0, 1, 0.01))
replace1 <- quantile(auto_1$carwidth, 0.75) + (IQR(auto_1$carwidth)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$carwidth[which(auto_1$carwidth > replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$carwidth, "Car Width")

continuous_bivariate(auto_1, auto_1$carheight, "carheight")
summary(auto_1$carheight)
quantile(auto_1$carheight, seq(0, 1, 0.01)) # No Outliers

## Checking for curbweight distribution and outliers

continuous_bivariate(auto_1, auto_1$curbweight, "curbweight")
summary(auto_1$curbweight)
quantile(auto_1$curbweight, seq(0, 1, 0.01)) # No Outliers

# Checking for Enginesize distribution and outliers

continuous_bivariate(auto_1, auto_1$enginesize, "Engine Size")
summary(auto_1$enginesize)
quantile(auto_1$enginesize, seq(0, 1, 0.01))
# replace1 <- quantile(auto_1$enginesize, 0.75) + (IQR(auto_1$enginesize)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$enginesize[which(auto_1$enginesize > 209)] <- 209 # Replace with 96% value 
continuous_bivariate(auto_1, auto_1$enginesize, "Engine Size")

# Checking for boreration distribution and outliers

continuous_bivariate(auto_1, auto_1$boreratio, "Bore Ratio")
summary(auto_1$boreratio)
quantile(auto_1$boreratio, seq(0, 1, 0.01)) # No Outliers

# Checking for stroke distribution and outliers

continuous_bivariate(auto_1, auto_1$stroke, "Stroke")
summary(auto_1$stroke)
quantile(auto_1$stroke, seq(0, 1, 0.01))
#replace1 <- quantile(auto_1$stroke, 0.75) + (IQR(auto_1$stroke)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$stroke[which(auto_1$stroke > 3.64)] <- 3.64 
#replace1 <- quantile(auto_1$stroke, 0.25) - (IQR(auto_1$stroke)*1.5) # replacing outliers (with Q1 - IQR*1.5)
auto_1$stroke[which(auto_1$stroke < 2.64)] <- 2.64 
continuous_bivariate(auto_1, auto_1$stroke, "Stroke")


# Checking for compression ratio distribution and outliers

continuous_bivariate(auto_1, auto_1$compressionratio, "Compression ratio")
summary(auto_1$compressionratio)
quantile(auto_1$compressionratio, seq(0, 1, 0.01))
#replace1 <- quantile(auto_1$compressionratio, 0.75) + (IQR(auto_1$compressionratio)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$compressionratio[which(auto_1$compressionratio > 10.94)] <- 10.94 
#replace1 <- quantile(auto_1$compressionratio, 0.25) - (IQR(auto_1$compressionratio)*1.5) # replacing outliers (with Q1 - IQR*1.5)
#auto_1$compressionratio[which(auto_1$compressionratio < replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$compressionratio, "Compression Ratio")


# Checking for horsepower distribution and outliers

continuous_bivariate(auto_1, auto_1$horsepower, "Horse Power")
summary(auto_1$horsepower)
quantile(auto_1$horsepower, seq(0, 1, 0.01))
replace1 <- quantile(auto_1$horsepower, 0.75) + (IQR(auto_1$horsepower)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$horsepower[which(auto_1$horsepower > replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$horsepower, "Engine Size")

# Checking for citympg distribution and outliers

continuous_bivariate(auto_1, auto_1$citympg, "City Mileage")
summary(auto_1$citympg)
quantile(auto_1$citympg, seq(0, 1, 0.01))
replace1 <- quantile(auto_1$citympg, 0.75) + (IQR(auto_1$citympg)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$citympg[which(auto_1$citympg > replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$citympg, "City Mpg")

# Checking for highwaympg distribution and outliers

continuous_bivariate(auto_1, auto_1$highwaympg, "Highway Mileage")
summary(auto_1$highwaympg)
quantile(auto_1$highwaympg, seq(0, 1, 0.01))
replace1 <- quantile(auto_1$highwaympg, 0.75) + (IQR(auto_1$highwaympg)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$highwaympg[which(auto_1$highwaympg > replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$highwaympg, "Highway MPG")

# Checking for peakrpm distribution and outliers

continuous_bivariate(auto_1, auto_1$peakrpm, "Peak RPM")
summary(auto_1$peakrpm)
quantile(auto_1$peakrpm, seq(0, 1, 0.01))
replace1 <- quantile(auto_1$peakrpm, 0.75) + (IQR(auto_1$peakrpm)*1.5) # replacing outliers (with Q3 + IQR*1.5)
auto_1$peakrpm[which(auto_1$peakrpm > replace1)] <- replace1 
continuous_bivariate(auto_1, auto_1$peakrpm, "Peak RPM")

######################################################## EDA - Trend analysis ####################################################
##################################################################################################################################

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = wheelbase), colour="blue") + 
  geom_line(aes(y = horsepower), colour = "grey") + 
  geom_line(aes(y = enginesize), colour = "red") + 
  ylab(label="Wheelbase-Blue, HP - Grey, EngineSize - Red, Citympg - Orange") + 
  xlab("Price")

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = citympg), colour = "red") + 
  geom_line(aes(y = highwaympg), colour = "black") + 
  ylab(label="citympg & highwaympg") + 
  xlab("Price")

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = boreratio), colour = "black") + 
  geom_line(aes(y = compressionratio), colour = "brown") + 
  ylab(label="Bore Ratio") + 
  xlab("Price")

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = compressionratio), colour = "brown") + 
  ylab(label="Compression Ratio(Brown)") + 
  xlab("Price")

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = stroke), colour = "pink") + 
  ylab(label="Stroke") + 
  xlab("Price")

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = curbweight), colour = "pink") + 
  ylab(label="curbweight") + 
  xlab("Price")

ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = peakrpm), colour = "magenta") + 
  ylab(label="peakrpm") + 
  xlab("Price")

 # seems citympg, enginesize, horsepower, curbweight, boreratio, highwaympg, Car Volume, Compression ratio
 # has an impact on price but need to confirm

#######################################################################################################################   
######################################### Dervied Columns #############################################################

auto_1$carvolume <- auto_1$carlength * auto_1$carwidth * auto_1$carheight   # Volume of the overall car design
# Trend Analysis for Car Volume
ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = carvolume), colour = "magenta") + 
  ylab(label="carvolume") + 
  xlab("Price")

auto_1$bore_stroke_ratio <- auto_1$boreratio/auto_1$stroke
ggplot(auto_1, aes(x = price)) + 
  geom_line(aes(y = bore_stroke_ratio), colour = "magenta") + 
  ylab(label="Bore/Stroke Ratio") + 
  xlab("Price")

#######################################################################################################################   
######################################### Modelling ###################################################################

# separate training and testing data

set.seed(100)
trainindices= sample(1:nrow(auto_1), 0.7*nrow(auto_1))
train = auto_1[trainindices,]
test = auto_1[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
#######

# Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 


# Lets load the library in which stepAIC function exists
step <- stepAIC(model_1, direction="both")
step

# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables:
# doornumber, wheelbase, horsepower, citympg, highwaympg, carbody:Sedan, drive:rwd, enginetype-dohcv,rotor;cylinder - 3, 12, 2
# fulesystem - 4bbl,midi,mpfi,spfi

# Let's execute this model here, 
model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + carheight + curbweight + enginesize + boreratio + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + CarNamevolvo + carvolume + bore_stroke_ratio, data = train)

# Let us look at the summary of the model
summary(model_2)

## Let us check for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove the variables if they are statistically insignificant
library(car)
vif(model_2)

# Let's execute this model - After removing Variable - Carwidth
model_3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carheight + curbweight + enginesize + boreratio + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + CarNamevolvo + carvolume + bore_stroke_ratio, data = train)


# Let us look at the summary of the model
summary(model_3)
vif(model_3)


# Let's execute this model - After removing Variable - boreratio 
model_4 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carheight + curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + CarNamevolvo + carvolume + bore_stroke_ratio, data = train)

# Let us look at the summary of the model
summary(model_4)
vif(model_4)

# Let's execute this model - After removing Variable - bore stroke ratio
model_5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carheight + curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + CarNamevolvo + carvolume, data = train)

# Let us look at the summary of the model
summary(model_5)
vif(model_5)

# Let's execute this model - After removing Variable -  Carname Volvo
model_6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carheight + curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNametoyota + 
                CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_6)
vif(model_6)

# Let's execute this model - After removing Variable -  CarName Saab 
model_7 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carheight + curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl + CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNametoyota + 
                CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_7)
vif(model_7)

# Let's execute this model - After removing Variable - fuelsystem2bbl
model_8 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carheight + curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNametoyota + 
                CarNamevolkswagen + carvolume, data = train)


# Let us look at the summary of the model
summary(model_8)
vif(model_8)

# Let us find corelation between Carlength and Carvolume
cor(train$carvolume, train$carlength) # 0.94 - Higher Corelation exists

# Let's execute this model - After removing Variable -Carlength
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carheight + curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNametoyota + 
                CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_9)
vif(model_9)

# Let us find corelation between Carlength and Carvolume
cor(train$carvolume, train$carheight) # 0.66 - Good amount of Corelation exists

# Let's execute this model - After removing Variable  - carheight
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                curbweight + enginesize +  
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNametoyota + 
                CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_10)
vif(model_10)

# Let's execute this model - After removing Variable - drivewheelrwd 
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)


# Let us look at the summary of the model
summary(model_11)
vif(model_11)

# Lets check corelation between Curbweight and Enginesize
cor(test$curbweight, test$enginesize) # 0..8799 - Curbweight has higher p-value than enginesize

# Let's execute this model - After removing Variable -  Curbweight
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)


# Let us look at the summary of the model
summary(model_12)
vif(model_12)

# Let's execute this model - After removing Variable -  Carmercury
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_13)
vif(model_13)

# Let's execute this model - After removing Variable - CarPorsche
model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_14)
vif(model_14)

# Let's execute this model - After removing Variable -
model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamehonda + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_15)
vif(model_15)

# Let's execute this model - After removing Variable - Carhonda
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel + enginetypeohcf + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_16)
vif(model_16)

# Let's execute this model - After removing Variable - enginetypeohcf 
model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_17)
vif(model_17)


# Let's execute this model - After removing Variable - Car Nissan
model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNameplymouth + 
                 CarNamerenault + CarNametoyota + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_18)
vif(model_18)

# Let's execute this model - After removing Variable - Car Toyota
model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNameplymouth + 
                 CarNamerenault +  
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_19)
vif(model_19)

# Let's execute this model - After removing Variable - CarNameplymouth
model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi +  
                 CarNamerenault +  
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_20)
vif(model_20)

# Let's execute this model - After removing Variable - CarNamerenault 
model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi +  
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_21)
vif(model_21)

# Let's execute this model - After removing Variable - Mitshibushi
model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_22)
vif(model_22)

# Let's execute this model - After removing Variable - Cardodge
model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar + CarNamemazda + 
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_23)
vif(model_23)

# Let's execute this model - After removing Variable - Car Mazda
model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 CarNamevolkswagen + carvolume, data = train)

# Let us look at the summary of the model
summary(model_24)
vif(model_24)

# Let's execute this model - After removing Variable - Car Volkswagen
model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetypel +  
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 carvolume, data = train)

# Let us look at the summary of the model
summary(model_25)
vif(model_25)

# Let's execute this model - After removing Variable -  enginetypel
model_26 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon +   
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 carvolume, data = train)

# Let us look at the summary of the model
summary(model_26)
vif(model_26)


# Let's execute this model - After removing Variable - carbodysedan
model_27 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhardtop + carbodyhatchback +  
                 carbodywagon +   
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 carvolume, data = train)

# Let us look at the summary of the model
summary(model_27)
vif(model_27)

# Let's execute this model - After removing Variable - carbodyhardtop
model_28 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm + carbodyhatchback +  
                 carbodywagon +   
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 carvolume, data = train)

# Let us look at the summary of the model
summary(model_28)
vif(model_28)

# Let's execute this model - After removing Variable - carbodyhatchback
model_29 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm +   
                 carbodywagon +   
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 carvolume, data = train)

# Let us look at the summary of the model
summary(model_29)
vif(model_29)

# Let's execute this model - After removing Variable - Carbodywagon
model_30 <- lm(formula = price ~ aspiration + enginelocation + 
                 enginesize +  
                 peakrpm +   
                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                 CarNamebmw + CarNamebuick +  
                 CarNamejaguar +  
                 carvolume, data = train)

# Let us look at the summary of the model
summary(model_30) # rsquared = 94.8%
vif(model_30)

test1 <- test[,-18]
# predicting the results in test dataset
Predict_1 <- predict(model_30,test1)
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared  # test rsquared - 0.86 vs Trained data Rsquare - 0.94  ==> Seems to be decent prediction model

############################################## Final Model - MODEL 26 ################################################
######################################################################################################################
#model_30 <- lm(formula = price ~ aspiration + enginelocation + 
#                 enginesize +  
#                 peakrpm +   
#                 cylindernumberfive + cylindernumberfour + cylindernumbersix + 
#                 CarNamebmw + CarNamebuick +  
#                 CarNamejaguar +  
#                 carvolume, data = train)

#summary(model_26) # Provides Independent variable co-efficients

# Final Model indicate that overall car price is largely based on 
# a) "EngineSize - Impact car design as well directly impacts car price",
# b) "Peakrpm" - Performance of car impacts design and indireclty impacts car price
# c) "Cylinder 5,4,6 - Cylinders provides necessary power for car and propotional to car price 
# d) "Car Name - Jaguar / Buick / BMW -- Brand value of car
# f) "Car Volume" - overall weight of the car

# Co-efficients of above variable provide necessary linear regression equation