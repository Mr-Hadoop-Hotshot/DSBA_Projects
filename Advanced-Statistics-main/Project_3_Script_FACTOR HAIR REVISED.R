#Name : Karthik
#Project: Project 3 Product Service Management.

#------------------------------------------------------------------------------------

setwd("C:/Users/user/Desktop/Data Science/FUNDAMENTAL OF BUSINESS STATISTICS/PRINCIPAL COMPONENT - PROJECT/")
getwd()

Customer_Feedback = read.csv("Factor-Hair-Revised.csv")

#------------------------------------------------------------------------------------

#Installing Library PackProdQual to Read & Write .csv files
install.packProdQuals("readr")
library(readr)
#Installing Library PackProdQual to work with plots
install.packProdQuals("ggplot2")
library(ggplot2)
#Calling Library PackProdQuals to work with Table
library(rpivotTable)
#Calling Library PackProdQuals to Work with data manipulation
library(dplyr)
#Calling Library PackProdQuals to Work with Correlation
library(corrplot) 
#Calling Library For Correlation Graph Analysis
library(PerformanceAnalytics)
#Calling Library For Exporting to .csv
library(rio)
#Calling Library to do Varriance Inflation Factor Analysis
library("car")

library(nFactors)
library(psych)
#------------------------------------------------------------------------------------
#SANITY CHECK
#------------------------------------------------------------------------------------

str(Customer_Feedback)
summary(Customer_Feedback) 

#Dimentions 
dim(Customer_Feedback)
## MISSING VALUE IDENTIFICATION
anyNA(Customer_Feedback)      # Outputs if any Missing values were spotted
sum(is.na(Customer_Feedback)) # Number of Missing Values

# ANALYSING FORMATTING ISSUES
head(Customer_Feedback,10)
tail(Customer_Feedback,10)

  
# Removing Unwanted Coloumns

Customer_Rating = -select(Customer_Feedback,-ID)
Customer_Rating = Customer_Rating*-1
View(Customer_Rating)
summary(Customer_Rating)

#UNIVARIATE ANALYSIS
#Product Quality
names(Customer_Rating)
range(Customer_Rating$ProdQual)

boxplot(Customer_Rating$ProdQual,horizontal = TRUE,col="Blue",
        main = "ProdQual Distribution", xlab= "ProdQual")

# OUTLIER CALCULATION
mean(Customer_Rating$ProdQual)
median(Customer_Rating$ProdQual)
ProdQual_IQR = IQR(Customer_Rating$ProdQual)
ProdQual_IQR

#OUTLIERS - Q3
Q3_ProdQual = quantile(Customer_Rating$ProdQual,0.75)
Q3_ProdQual = unname(Q3)
Q3_ProdQual

Q1_ProdQual = unname(quantile(Customer_Rating$ProdQual,0.5))
Q1_ProdQual

#OUTLIERS - Q1
Q1_ProdQual = unname(quantile(Customer_Rating$ProdQual,0.25))
Q1_ProdQual
Max_ProdQual_IQR = Q3_ProdQual+1.5*ProdQual_IQR # Positive Outlier Calculation
Max_ProdQual_IQR
Min_ProdQual_IQR = Q1_ProdQual-1.5*ProdQual_IQR # Negative Outlier Calculation
Min_ProdQual_IQR
# No Of OUTLIERS
sum(Customer_Rating$ProdQual > Max_ProdQual_IQR)
sum(Customer_Rating$ProdQual < Min_ProdQual_IQR)

# Print all Outliers
Customer_Rating$ProdQual[Customer_Rating$ProdQual > Max_ProdQual_IQR]
Customer_Rating$ProdQual[Customer_Rating$ProdQual < Min_ProdQual_IQR]
# Histogram

hist(Customer_Rating$ProdQual,col="Blue",xlim=c(1,12),
     main="Product Quality Rating",xlab="Rating 1- 10")


qplot(Customer_Rating$ProdQual,data=Customer_Rating,geom ="histogram",binwidth=1)+
  labs(title = "Prod Qual")+labs(x="Ratings")+labs(y="Frequency")+
  scale_y_continuous( breaks=c(1:20),minor_breaks=NULL)+
  scale_x_continuous( breaks=c(1:10),minor_breaks=NULL)+
  geom_vline(xintercept = mean(Customer_Rating$ProdQual),show.legend=TRUE,col="red")+
  geom_vline(yintercept=median(Customer_Rating$ProdQual),show.legend=TRUE,col="blue")
#===================================================================================
qplot(Customer_Rating$ProdQual,data=Customer_Rating,geom ="histogram",binwidth=0.2)+
  labs(title = "Prod Qual")+labs(x="Ratings")+labs(y="Frequency")+
  scale_y_continuous( breaks=c(1:5),minor_breaks=NULL)+
  scale_x_continuous( breaks=c(1:10),minor_breaks=NULL)+
  geom_vline(xintercept = mean(Customer_Rating$ProdQual),show.legend=TRUE,col="red")+
  geom_vline(xintercept=median(Customer_Rating$ProdQual),show.legend=TRUE,col="blue")
#===================================================================================

#UNIVARIATE of Ecom
range(Customer_Rating$Ecom)

boxplot(Customer_Rating$Ecom,horizontal = TRUE,col="Blue",
        main = "Ecom Distribution", xlab= "Ecom")
# OUTLIER CALCULATION
mean(Customer_Rating$Ecom)
median(Customer_Rating$Ecom)
Ecom_IQR = IQR(Customer_Rating$Ecom)
Ecom_IQR
#OUTLIERS - Q3
Q3_Ecom = quantile(Customer_Rating$Ecom,0.75)
Q3_Ecom = unname(Q3_Ecom)
Q3_Ecom
#OUTLIERS - Q1
Q1_Ecom = unname(quantile(Customer_Rating$Ecom,0.25))
Q1_Ecom
Max_Ecom_IQR = Q3_Ecom+1.5*Ecom_IQR # Positive Outlier Calculation
Max_Ecom_IQR
Min_Ecom_IQR = Q1-1.5*Ecom_IQR # Negative Outlier Calculation
Min_Ecom_IQR
# No Of OUTLIERS
sum(Customer_Rating$Ecom > Max_Ecom_IQR)
sum(Customer_Rating$Ecom < Min_Ecom_IQR)

# Print all Outliers
Customer_Rating$Ecom[Customer_Rating$Ecom > Max_Ecom_IQR]
Customer_Rating$Ecom[Customer_Rating$Ecom < Min_Ecom_IQR]
# Histogram

hist(Customer_Rating$Ecom,col="Green",xlim=c(1,12),
     main="Product Quality Rating",xlab="Rating 1- 10")

#===================================================================================

#UNIVARIATE of TechSup
names(Customer_Rating)
range(Customer_Rating$TechSup)

boxplot(Customer_Rating$TechSup,horizontal = TRUE,col="Blue",
        main = "TechSup Distribution", xlab= "TechSup")
# OUTLIER CALCULATION
mean(Customer_Rating$TechSup)
median(Customer_Rating$TechSup)
TechSup_IQR = IQR(Customer_Rating$TechSup)
TechSup_IQR
#OUTLIERS - Q3
Q3_TechSup = quantile(Customer_Rating$TechSup,0.75)
Q3_TechSup = unname(Q3_TechSup)
Q3_TechSup
#OUTLIERS - Q1
Q1_TechSup = unname(quantile(Customer_Rating$TechSup,0.25))
Q1_TechSup
Max_TechSup_IQR = Q3_TechSup+1.5*TechSup_IQR # Positive Outlier Calculation
Max_TechSup_IQR
Min_TechSup_IQR = Q1-1.5*TechSup_IQR # Negative Outlier Calculation
Min_TechSup_IQR
# No Of OUTLIERS
sum(Customer_Rating$TechSup > Max_TechSup_IQR)
sum(Customer_Rating$TechSup < Min_TechSup_IQR)

# Print all Outliers
Customer_Rating$TechSup[Customer_Rating$TechSup > Max_TechSup_IQR]
Customer_Rating$TechSup[Customer_Rating$TechSup < Min_TechSup_IQR]
# Histogram

hist(Customer_Rating$TechSup,col="red",xlim=c(1,12),
     main="Product Quality Rating",xlab="Rating 1- 10")
names(Customer_Rating)

#===================================================================================

#UNIVARIATE of CompRes
names(Customer_Rating)
range(Customer_Rating$CompRes)

boxplot(Customer_Rating$CompRes,horizontal = TRUE,col="Blue",
        main = "CompRes Distribution", xlab= "CompRes")
# OUTLIER CALCULATION
mean(Customer_Rating$CompRes)
median(Customer_Rating$CompRes)
CompRes_IQR = IQR(Customer_Rating$CompRes)
CompRes_IQR

#OUTLIERS - Q3
Q3_CompRes = quantile(Customer_Rating$CompRes,0.75)
Q3_CompRes = unname(Q3_CompRes)
Q3_CompRes
#OUTLIERS - Q1
Q1_CompRes = unname(quantile(Customer_Rating$CompRes,0.25))
Q1_CompRes
Max_CompRes_IQR = Q3_CompRes+1.5*CompRes_IQR # Positive Outlier Calculation
Max_CompRes_IQR
Min_CompRes_IQR = Q1-1.5*CompRes_IQR # Negative Outlier Calculation
Min_CompRes_IQR
# No Of OUTLIERS
sum(Customer_Rating$CompRes > Max_CompRes_IQR)
sum(Customer_Rating$CompRes < Min_CompRes_IQR)

# Print all Outliers
Customer_Rating$CompRes[Customer_Rating$CompRes > Max_CompRes_IQR]
Customer_Rating$CompRes[Customer_Rating$CompRes < Min_CompRes_IQR]
# Histogram

hist(Customer_Rating$CompRes,col="Green",xlim=c(1,12),
     main="Product Quality Rating",xlab="Rating 1- 10")

#===================================================================================

#UNIVARIATE of Advertising
names(Customer_Rating)
range(Customer_Rating$Advertising)

boxplot(Customer_Rating$Advertising,horizontal = TRUE,col="Blue",
        main = "Advertising Distribution", xlab= "Advertising")
# OUTLIER CALCULATION
mean(Customer_Rating$Advertising)
median(Customer_Rating$Advertising)
Advertising_IQR = IQR(Customer_Rating$Advertising)
Advertising_IQR
#OUTLIERS - Q3
Q3_Advertising = quantile(Customer_Rating$Advertising,0.75)
Q3_Advertising = unname(Q3_Advertising)
Q3_Advertising
#OUTLIERS - Q1
Q1_Advertising = unname(quantile(Customer_Rating$Advertising,0.25))
Q1_Advertising
Max_Advertising_IQR = Q3_Advertising+1.5*Advertising_IQR # Positive Outlier Calculation
Max_Advertising_IQR
Min_Advertising_IQR = Q1-1.5*Advertising_IQR # Negative Outlier Calculation
Min_Advertising_IQR
# No Of OUTLIERS
sum(Customer_Rating$Advertising > Max_Advertising_IQR)
sum(Customer_Rating$Advertising < Min_Advertising_IQR)

# Print all Outliers
Customer_Rating$Advertising[Customer_Rating$Advertising > Max_Advertising_IQR]
Customer_Rating$Advertising[Customer_Rating$Advertising < Min_Advertising_IQR]

# Histogram

hist(Customer_Rating$Advertising,col="brown",xlim=c(1,12),
     main="Advertising Rating",xlab="Rating 1- 10")
#===================================================================================

#UNIVARIATE of ProdLine
names(Customer_Rating)
range(Customer_Rating$ProdLine)

boxplot(Customer_Rating$ProdLine,horizontal = TRUE,col="Yellow",
        main = "ProdLine Distribution", xlab= "ProdLine")
# OUTLIER CALCULATION
mean(Customer_Rating$ProdLine)
median(Customer_Rating$ProdLine)
ProdLine_IQR = IQR(Customer_Rating$ProdLine)
ProdLine_IQR
#OUTLIERS - Q3
Q3_ProdLine = quantile(Customer_Rating$ProdLine,0.75)
Q3_ProdLine = unname(Q3_ProdLine)
Q3_ProdLine
#OUTLIERS - Q1
Q1_ProdLine = unname(quantile(Customer_Rating$ProdLine,0.25))
Q1_ProdLine
Max_ProdLine_IQR = Q3_ProdLine+1.5*ProdLine_IQR # Positive Outlier Calculation
Max_ProdLine_IQR
Min_ProdLine_IQR = Q1-1.5*ProdLine_IQR # Negative Outlier Calculation
Min_ProdLine_IQR
# No Of OUTLIERS
sum(Customer_Rating$ProdLine > Max_ProdLine_IQR)
sum(Customer_Rating$ProdLine < Min_ProdLine_IQR)

# Print all Outliers
Customer_Rating$ProdLine[Customer_Rating$ProdLine > Max_ProdLine_IQR]
Customer_Rating$ProdLine[Customer_Rating$ProdLine < Min_ProdLine_IQR]
# Histogram

hist(Customer_Rating$ProdLine,col="Green",xlim=c(1,12),
     main="ProdLine Rating",xlab="Rating 1- 10")

#===================================================================================

#UNIVARIATE of ComPricing
names(Customer_Rating)
range(Customer_Rating$ComPricing)

boxplot(Customer_Rating$ComPricing,horizontal = TRUE,col="pink",
        main = "Competive Pricing Distribution", xlab= "ComPricing")
# OUTLIER CALCULATION
mean(Customer_Rating$ComPricing)
median(Customer_Rating$ComPricing)
ComPricing_IQR = IQR(Customer_Rating$ComPricing)
ComPricing_IQR
#OUTLIERS - Q3
Q3_ComPricing = quantile(Customer_Rating$ComPricing,0.75)
Q3_ComPricing = unname(Q3_ComPricing)
Q3_ComPricing
#OUTLIERS - Q1
Q1_ComPricing = unname(quantile(Customer_Rating$ComPricing,0.25))
Q1_ComPricing
Max_ComPricing_IQR = Q3_ComPricing+1.5*ComPricing_IQR # Positive Outlier Calculation
Max_ComPricing_IQR
Min_ComPricing_IQR = Q1-1.5*ComPricing_IQR # Negative Outlier Calculation
Min_ComPricing_IQR
# No Of OUTLIERS
sum(Customer_Rating$ComPricing > Max_ComPricing_IQR)
sum(Customer_Rating$ComPricing < Min_ComPricing_IQR)

# Print all Outliers
Customer_Rating$ComPricing[Customer_Rating$ComPricing > Max_ComPricing_IQR]
Customer_Rating$ComPricing[Customer_Rating$ComPricing < Min_ComPricing_IQR]
# Histogram

hist(Customer_Rating$ComPricing,col="purple",xlim=c(1,12),
     main="Competitve Pricing Rating",xlab="Rating 1- 10")

#===================================================================================

#UNIVARIATE of SalesFImage
names(Customer_Rating)
range(Customer_Rating$SalesFImage)

boxplot(Customer_Rating$SalesFImage,horizontal = TRUE,col="grey",
        main = "Sales Force Image Distribution", xlab= "SalesFImage")
# OUTLIER CALCULATION
mean(Customer_Rating$SalesFImage)
median(Customer_Rating$SalesFImage)
SalesFImage_IQR = IQR(Customer_Rating$SalesFImage)
SalesFImage_IQR
#OUTLIERS - Q3
Q3_SalesFImage = quantile(Customer_Rating$SalesFImage,0.75)
Q3_SalesFImage = unname(Q3_SalesFImage)
Q3_SalesFImage
#OUTLIERS - Q1
Q1_SalesFImage = unname(quantile(Customer_Rating$SalesFImage,0.25))
Q1_SalesFImage
Max_SalesFImage_IQR = Q3_SalesFImage+1.5*SalesFImage_IQR # Positive Outlier Calculation
Max_SalesFImage_IQR
Min_SalesFImage_IQR = Q1-1.5*SalesFImage_IQR # Negative Outlier Calculation
Min_SalesFImage_IQR
# No Of OUTLIERS
sum(Customer_Rating$SalesFImage > Max_SalesFImage_IQR)
sum(Customer_Rating$SalesFImage < Min_SalesFImage_IQR)

# Print all Outliers
Customer_Rating$SalesFImage[Customer_Rating$SalesFImage > Max_SalesFImage_IQR]
Customer_Rating$SalesFImage[Customer_Rating$SalesFImage < Min_SalesFImage_IQR]
# Histogram

hist(Customer_Rating$SalesFImage,col="Green",xlim=c(1,12),
     main="Sales Force Image Rating",xlab="Rating 1- 10")

qplot(Customer_Rating$SalesFImage,data=Customer_Rating,geom ="histogram",binwidth=0.2)+
  labs(title = "Prod Qual")+labs(x="Ratings")+labs(y="Frequency")+
  scale_y_continuous(breaks=c(1:5),minor_breaks=NULL)+
  scale_x_continuous(breaks=c(1:10),minor_breaks=NULL)+
  geom_vline(xintercept = mean(Customer_Rating$SalesFImage),show.legend=TRUE,col="red")+
  geom_vline(xintercept=median(Customer_Rating$SalesFImage),show.legend=TRUE,col="blue")
#===================================================================================

#UNIVARIATE of WartyClaim
names(Customer_Rating)
range(Customer_Rating$WartyClaim)

boxplot(Customer_Rating$WartyClaim,horizontal = TRUE,col="brown",
        main = "Warranty Claims Distribution", xlab= "WartyClaim")
# OUTLIER CALCULATION
mean(Customer_Rating$WartyClaim)
median(Customer_Rating$WartyClaim)
WartyClaim_IQR = IQR(Customer_Rating$WartyClaim)
WartyClaim_IQR
#OUTLIERS - Q3
Q3_WartyClaim = quantile(Customer_Rating$WartyClaim,0.75)
Q3_WartyClaim = unname(Q3_WartyClaim)
Q3_WartyClaim
#OUTLIERS - Q1
Q1_WartyClaim = unname(quantile(Customer_Rating$WartyClaim,0.25))
Q1_WartyClaim
Max_WartyClaim_IQR = Q3_WartyClaim+1.5*WartyClaim_IQR # Positive Outlier Calculation
Max_WartyClaim_IQR
Min_WartyClaim_IQR = Q1-1.5*WartyClaim_IQR # Negative Outlier Calculation
Min_WartyClaim_IQR
# No Of OUTLIERS
sum(Customer_Rating$WartyClaim > Max_WartyClaim_IQR)
sum(Customer_Rating$WartyClaim < Min_WartyClaim_IQR)

# Print all Outliers
Customer_Rating$WartyClaim[Customer_Rating$WartyClaim > Max_WartyClaim_IQR]
Customer_Rating$WartyClaim[Customer_Rating$WartyClaim < Min_WartyClaim_IQR]
# Histogram

hist(Customer_Rating$WartyClaim,col="Green",xlim=c(1,12),
     main="Warranty Claims Rating",xlab="Rating 1- 10")


qplot(Customer_Rating$WartyClaim,data=Customer_Rating,geom ="histogram",binwidth=0.2)+
  labs(title = "Prod Qual")+labs(x="Ratings")+labs(y="Frequency")+
  scale_y_continuous(breaks=c(1:5),minor_breaks=NULL)+
  scale_x_continuous(breaks=c(1:10),minor_breaks=30)+
  geom_vline(xintercept = mean(Customer_Rating$WartyClaim),show.legend=TRUE,col="red")+
  geom_vline(xintercept=median(Customer_Rating$WartyClaim),show.legend=TRUE,col="blue")


#===================================================================================

#UNIVARIATE of OrdBilling
names(Customer_Rating)
range(Customer_Rating$OrdBilling)

boxplot(Customer_Rating$OrdBilling,horizontal = TRUE,col="Purple",
        main = "Warranty Claims Distribution", xlab= "OrdBilling")
# OUTLIER CALCULATION
mean(Customer_Rating$OrdBilling)
median(Customer_Rating$OrdBilling)
OrdBilling_IQR = IQR(Customer_Rating$OrdBilling)
OrdBilling_IQR
#OUTLIERS - Q3
Q3_OrdBilling = quantile(Customer_Rating$OrdBilling,0.75)
Q3_OrdBilling = unname(Q3_OrdBilling)
Q3_OrdBilling
#OUTLIERs - Q1
Q1_OrdBilling = unname(quantile(Customer_Rating$OrdBilling,0.25))
Q1_OrdBilling
Max_OrdBilling_IQR = Q3_OrdBilling+1.5*OrdBilling_IQR # Positive Outlier Calculation
Max_OrdBilling_IQR
Min_OrdBilling_IQR = Q1_OrdBilling-1.5*OrdBilling_IQR # Negative Outlier Calculation
Min_OrdBilling_IQR
# No Of OUTLIERS
sum(Customer_Rating$OrdBilling > Max_OrdBilling_IQR)
sum(Customer_Rating$OrdBilling < Min_OrdBilling_IQR)

# Print all Outliers
Customer_Rating$OrdBilling[Customer_Rating$OrdBilling > Max_OrdBilling_IQR]
Customer_Rating$OrdBilling[Customer_Rating$OrdBilling < Min_OrdBilling_IQR]
# Histogram

hist(Customer_Rating$OrdBilling,col="Green",xlim=c(1,12),
     main="Warranty Claims Rating",xlab="Rating 1- 10")

qplot(Customer_Rating$OrdBilling,data=Customer_Rating,geom ="histogram",binwidth=0.2)+
  labs(title = "Order & Billing")+labs(x="Ratings")+labs(y="Frequency")+
  scale_y_continuous(breaks=c(1:5),minor_breaks=NULL)+
  scale_x_continuous(breaks=c(1:10),minor_breaks=30)+
  geom_vline(xintercept = mean(Customer_Rating$OrdBilling),show.legend=TRUE,col="red")+
  geom_vline(xintercept=median(Customer_Rating$OrdBilling),show.legend=TRUE,col="blue")


#===================================================================================

#UNIVARIATE of DelSpeed
names(Customer_Rating)
range(Customer_Rating$DelSpeed)

boxplot(Customer_Rating$DelSpeed,horizontal = TRUE,col="Orange",
        main = "Warranty Claims Distribution", xlab= "DelSpeed")
# OUTLIER CALCULATION
mean(Customer_Rating$DelSpeed)
median(Customer_Rating$DelSpeed)
DelSpeed_IQR = IQR(Customer_Rating$DelSpeed)
DelSpeed_IQR
#OUTLIERS - Q3
Q3_DelSpeed = quantile(Customer_Rating$DelSpeed,0.75)
Q3_DelSpeed = unname(Q3_DelSpeed)
Q3_DelSpeed
#OUTLIERs - Q1
Q1_DelSpeed = unname(quantile(Customer_Rating$DelSpeed,0.25))
Q1_DelSpeed
Max_DelSpeed_IQR = Q3_DelSpeed+1.5*DelSpeed_IQR # Positive Outlier Calculation
Max_DelSpeed_IQR
Min_DelSpeed_IQR = Q1_DelSpeed-1.5*DelSpeed_IQR # Negative Outlier Calculation
Min_DelSpeed_IQR
# No Of OUTLIERS
sum(Customer_Rating$DelSpeed > Max_DelSpeed_IQR)
sum(Customer_Rating$DelSpeed < Min_DelSpeed_IQR)

# Print all Outliers
Customer_Rating$DelSpeed[Customer_Rating$DelSpeed > Max_DelSpeed_IQR]
Customer_Rating$DelSpeed[Customer_Rating$DelSpeed < Min_DelSpeed_IQR]
# Histogram

hist(Customer_Rating$DelSpeed,col="Green",xlim=c(1,12),
     main="Delivery Speed",xlab="Rating 1- 10")

#===================================================================================

#UNIVARIATE of Satisfaction
names(Customer_Rating)
range(Customer_Rating$Satisfaction)

boxplot(Customer_Rating$Satisfaction,horizontal = TRUE,col="Yellow",
        main = "Satisfaction Ratings Distribution", xlab= "Satisfaction")
# OUTLIER CALCULATION
mean(Customer_Rating$Satisfaction)
median(Customer_Rating$Satisfaction)
Satisfaction_IQR = IQR(Customer_Rating$Satisfaction)
Satisfaction_IQR
#OUTLIERS - Q3
Q3_Satisfaction = quantile(Customer_Rating$Satisfaction,0.75)
Q3_Satisfaction = unname(Q3_Satisfaction)
Q3_Satisfaction
#OUTLIERs - Q1
Q1_Satisfaction = unname(quantile(Customer_Rating$Satisfaction,0.25))
Q1_Satisfaction
Max_Satisfaction_IQR = Q3_Satisfaction+1.5*Satisfaction_IQR # Positive Outlier Calculation
Max_Satisfaction_IQR
Min_Satisfaction_IQR = Q1_Satisfaction-1.5*Satisfaction_IQR # Negative Outlier Calculation
Min_Satisfaction_IQR
# No Of OUTLIERS
sum(Customer_Rating$Satisfaction > Max_Satisfaction_IQR)
sum(Customer_Rating$Satisfaction < Min_Satisfaction_IQR)

# Print all Outliers
Customer_Rating$Satisfaction[Customer_Rating$Satisfaction > Max_Satisfaction_IQR]
Customer_Rating$Satisfaction[Customer_Rating$Satisfaction < Min_Satisfaction_IQR]
# Histogram

hist(Customer_Rating$Satisfaction,col="Green",xlim=c(1,12),
     main="Satisfaction Rating",xlab="Rating 1- 10")
rm(Satisfaction.c)

names(Customer_Rating)
attach (Customer_Rating)

boxplot(ProdQual,Ecom,TechSup,CompRes,Advertising,ProdLine,SalesFImage,
        ComPricing,WartyClaim,OrdBilling,DelSpeed,Satisfaction,
        horizontal = TRUE,
        
        col=c("Yellow","Red","Grey","Green","Blue",
              "Orange","Light Blue","Brown","White",
              "purple","Dark Green","Blue"),
        
        main = "Univariate Comparision of Ratings", 
        xlab= "Rating",ylab="Attributes",
       # names=c("1","2","3","4","5","6","7","8","9","10","11","12"))
        names=c("ProdQual","Ecom","TechSup","CompRes",
             "Advertising","ProdLine","SalesFImage",
               "ComPricing","WartyClaim","OrdBilling",
              "DelSpeed","Satisfaction"))
names(Customer_Rating)

#-------------------------------------------------------------------------
#MULTI-COLINEARITY
names(Customer_Rating)
Correlation_Test=cor(Customer_Rating[1:12])
table(Correlation_Test)
Correlation_Test= as.data.frame(Correlation_Test)
View(Correlation_Test)

install.packages("xlsx")
library(xlsx)
write.xlsx(Correlation_Test, "c:/Correlation_Test.xlsx")


# Pairwise correlation 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#(1) Correlation: Alcohol and Fixed Acidity 
chart.Correlation(cbind(ProdQual,Satisfaction))

install.packages("PerformanceAnalytics")
#---------------------
# EXPORTING TO .CSV
library(rio)
Correlation_Test= as.data.frame(Correlation_Test)
export(Correlation_Test, "Correlation.csv")
#-----------------------
corrplot(cor(Customer_Rating[,1:12]))
SLM= lm(Satisfaction~ProdQual+Ecom+TechSup+CompRes+Advertising+ProdLine+SalesFImage+
          ComPricing+WartyClaim+OrdBilling+DelSpeed,,data=Customer_Rating)


vif(SLM)

#---------------------------------------------------------------------------------------
#SIMPLE LINEAR REGRESSION BETWEEN DEPENDENT VARIABLE & EVERY INDEPENDENT VARIABLE

#ProdQual
attach(Customer_Rating)
cor(Satisfaction,ProdQual)
plot(ProdQual,Satisfaction,col="red",
     main="Line of Best Fit for Satisfaction & Product Quality",
     abline(lm(Satisfaction~ProdQual,col="Blue")))
SLM_ProdQual = lm(Satisfaction~ProdQual,data=Customer_Rating)
summary(SLM_ProdQual)


#CompRes
cor(Satisfaction,CompRes)
plot(CompRes,Satisfaction,col="red",
     main="Line of Best Fit for Satisfaction & Complaint Resolution",
     abline(lm(Satisfaction~CompRes,col="Blue")))
SLM_CompRes = lm(Satisfaction~CompRes,data=Customer_Rating)
summary(SLM_CompRes)
#ProdLine
cor(Satisfaction,ProdLine)
plot(ProdLine,Satisfaction,col="red",
     main="Line of Best Fit for Satisfaction & Product Line",
     abline(lm(Satisfaction~ProdLine,col="Blue")))
SLM_ProdLine = lm(Satisfaction~ProdLine,data=Customer_Rating)
summary(SLM_ProdLine)
#SalesFImage,
cor(Satisfaction,SalesFImage)
plot(SalesFImage,Satisfaction,col="red",
     main="Line of Best Fit for Satisfaction & SalesForce Image",
     abline(lm(Satisfaction~SalesFImage,col="Blue")))
SLM_SalesFImage = lm(Satisfaction~SalesFImage,data=Customer_Rating)
summary(SLM_SalesFImage)

#WartyClaim 

#DelSpeed

#---------------------------------------------------------------------------------------
#PCA / FA
install.packages("nFactors")


ev=eigen(cor(Customer_Rating)) # Obtaining the Eigen Values
print(ev,digits=5)
ev
EigenValue=ev$values
EigenValue
Factor = c(1,2,3,4,5,6,7,8,9,10,11,12)
Scree = data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot",col="Blue",ylim=c(0,4))
lines(Scree,col="Red")

Unrotate=principal(Customer_Rating,nfactors=4,rotate="none")
print(Unrotate,digits=3)

UnrotatedProfile = plot(Unrotate,row.names(Unrotae$loadings))

Rotate=principal(Customer_Rating,nfactors=4,rotate="varimax")
print(Rotate,digits=4)
Rotate$scores
#---------------------------------------------------------------------------------------
#MULTI REGRESSION ANALYSIS
Model=lm(Satisfaction~ProdQual+Ecom+TechSup+CompRes+
                       Advertising+ProdLine+SalesFImage+
                       ComPricing+WartyClaim+OrdBilling+
                       DelSpeed+Satisfaction,data=Customer_Rating)
summary(Model)








