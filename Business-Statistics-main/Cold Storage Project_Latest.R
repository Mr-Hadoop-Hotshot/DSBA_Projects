setwd("C:/Users/user/Desktop/Data Science/FUNDAMENTAL OF BUSINESS STATISTICS/PROJECTS/")
getwd()

ColdStorage_Data = read.csv("Cold_Storage_Temp_Data.csv")
ColdStorage_Data_MAR = read.csv("Cold_Storage_Mar2018.csv")

#-------------------------------------------------------------------------------------------
# INSTALLING PACKAGES & CALLING THE LIBRARIES
install.packages("readr")
library(readr)

install.packages("ggplot2")
library(ggplot2)

install.packages("Rcmdr")
library(Rcmdr)

install.packages("BSDA")
library(BSDA)
#-------------------------------------------------------------------------------------------
attach(ColdStorage_Data) # Attaching Variables to the file



#SANITY CHECK
#-------------
str(ColdStorage_Data)     #Identyfing the dataset
summary(ColdStorage_Data) # Summary view on data distribution for each variable 

# MISSING VALUE IDENTFICATION
#----------------------------
anyNA(ColdStorage_Data)     # Checking for NA Values
sum(is.na(ColdStorage_Data)) # Output the number of NA values.

#ANALYSING THE FORMATTING ISSUE
#------------------------------

head(ColdStorage_Data,10)  # Display first 10 Rows data.
tail(ColdStorage_Data,10)  # Display last 10 Rows data.

#IDENTIFYING COLOUMNS
#--------------------
colnames(ColdStorage_Data) # Checking the Coloumn names and format
ColdStorage_Data$Date=as.factor(ColdStorage_Data$Date) # Converting Date Class 

#1.MEAN TEMPERATURE OF SUMMER, WINTER & RAINY SEASON
#---------------------------------------------------
summary(ColdStorage_Data) # Summary view on data distribution for each variable 


Seasonal_Mean=by(ColdStorage_Data$Temperature,INDICES=ColdStorage_Data$Season,
                 FUN=mean)# Calculating Mean for each season.
round(Seasonal_Mean,3) # Rounding up the Mean value to 3 decimal places.
table(Seasonal_Mean) # Output each season mean into table format.

#2. OVERALL MEAN
#---------------

Grand_Mean = mean(ColdStorage_Data$Temperature) # Calculating the overall Mean
round(Grand_Mean,3) # Rounding up the Mean value to 3 decimal places.
      


#3. OVERALL STANDARD DEVIATION
#---------------------
Grand_SD = sd(ColdStorage_Data$Temperature) # Calculating the overall STANDARD DEVIATION
Grand_SD          # Calling the variable Grand_SD       
round(Grand_SD,3) # Rounding up the STANDARD DEVIATION value to 3 decimal places.


#4.PROBABILITY OF TEMPERATURE LESS THAN 2 degree 
#-----------------------------------------------

Prob_2CLess=pnorm(2,mean=Grand_Mean,sd=Grand_SD) # Probability Calculation for P(x<2)
Prob_2CLess*100                # Calling the variable Prob_2CLess (%)  
round(Prob_2CLess*100,3)      # Rounding up the P(x<2) value to 3 decimal places.      


#5.PROBABILITY OF TEMPERATURE MORE THAN 4 degree 
#-----------------------------------------------

Prob_4CMore=pnorm(4,mean=Grand_Mean,sd=Grand_SD,lower.tail = FALSE) # Probability Calculation for P(x > 4)
Prob_4CMore*100                 # Calling the variable Prob_4CMore (%)
round(Prob_4CMore*100,3)        # Rounding up the P(x > 4) value to 3 decimal places.


#6. THE PENALTY FOR THE amc COMPANY
#-----------------------------------

Tot_Prob_of_Leakage = Prob_2CLess+Prob_4CMore # Total Probability of temperature outside the region.
round(Tot_Prob_of_Leakage*100,3)   # Rounding up the Penalty value to 3 decimal places.

#7. ONE WAY ANOVA TEST
#---------------------

dim(ColdStorage_Data) #Dimention of the ColdStorage_Data set


#Assumption - 1 -Normality Test
#--------------------------------

boxplot(ColdStorage_Data$Temperature,horizontal=TRUE,col="purple",
        main="Boxplot-Temperature",xlab="Temperature") # Boxplot on Tempreature Data

boxplot(Temperature~Season,data=ColdStorage_Data,horizontal=TRUE,col=c("Red","Yellow","Green"),
        main="Boxplot-Season Vs Temperature",xlab="Temperature") # Boxplot on Tempreature Data Seasonal comparison.

hist(ColdStorage_Data$Temperature,col="Blue",main="Histogram for all 4 Season Temperature",
     xlab="Temperature Ranges",ylab="Frequency of Occurence",breaks=10) # Histogram plot on Temperature Data Distribution.

View(ColdStorage_Data) # Open the Dataset

ggsave("Histogram for all 4 Season Temperature.jpeg",width=9,height=6) # Save the Plot



shapiro.test (ColdStorage_Data$Temperature) #Shapiro-Wilk normality test


# ASSUMPTION - 2 - Homogenity of Variance
#------------------------------------------

leveneTest(Temperature~Season, ColdStorage_Data) #Levene's Test for Homogeneity of Variance 
round(leveneTest(Temperature~Season, ColdStorage_Data),3)  # Rounding up the Levene's Test value to 3 decimal places.


Season_Table=table(ColdStorage_Data$Season) # Sample Number based on Season
Season_Table # Calling the Season_Table Variable.


# ANOVA Table for for Seasonal Temperature
------------------------------------------

AOV_Temp = aov( Temperature~ Season, data=ColdStorage_Data) # ONEWAY ANOVA TEST
summary(AOV_Temp) # Display  ONEWAY ANOVA TEST


round(tapply(ColdStorage_Data$Temperature,ColdStorage_Data$Season,mean),3) # Output of Mean all season and rounded to 3 decimal places
round(tapply(ColdStorage_Data$Temperature,ColdStorage_Data$Season,var),3)   # Output of Variance all season and rounded to 3 decimal places

plot(AOV_Temp) # Q-Q plot and Residual Plot

Tukey_test_posthoc = TukeyHSD(x=AOV_Temp, conf.level=0.95) #TukeyHSD test

round(Tukey_test_posthoc$Season,2) #Rounding the TukeyHSD test to 3 decimal places

par(oma=c(0,3,1,0)) # adjust the margins plot
plot(Tukey_test_posthoc,las=1, col = "red") # Pairwise Difference plot with 95% confidence interval

#---------------------------------------------------------------------------------------------------------------
#PROBLEM - 2
#---------------------------------------------------------------------------------------------------------------

#SANITY CHECK
#-------------
str(ColdStorage_Data_MAR)#Identyfing the dataset
summary(ColdStorage_Data_MAR) # Summary view on data distribution for each variable 

# MISSING VALUE IDENTFICATION
#----------------------------
anyNA(ColdStorage_Data_MAR)# Output the number of NA values.
sum(is.na(ColdStorage_Data_MAR))# Checking for NA Values


#ANALYSING THE FORMATTING ISSUE
#------------------------------


head(ColdStorage_Data,10) # Display first 10 Rows data.
tail(ColdStorage_Data,10) # Display last 10 Rows data.

#IDENTIFYING COLOUMNS
#--------------------
colnames(ColdStorage_Data) # Checking the Coloumn names and format
dim(ColdStorage_Data_MAR) # Dimention of the ColdStorage_Data_MAR



#2. STATE HYPOTHESIS 
#--------------------------

hist(ColdStorage_Data_MAR$Temperature,col='purple',breaks=sq) # Histogram plot on the 35 datas fromColdStorage_Data_MAR 
Mean_MAR=mean(ColdStorage_Data_MAR$Temperature) # Mean of the ColdStorage_Data_MAR Dataset
SD_Mar=sd(ColdStorage_Data_MAR$Temperature)    # Mean of the STANDARD DEVIATION Dataset



t.test(ColdStorage_Data_MAR$Temperature,y=NULL,alternative="greater",conf.level = 0.99) #ttest





                            
                               