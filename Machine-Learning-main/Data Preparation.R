setwd("C:/Users/user/Desktop/Data Science/MACHINE LEARNING/Project/")
getwd()
#---------------------------------------------------------------------------------
#INSTALLING PACKAGES
library(readr)        
library(readxl)
install.packages("mice")
library (mice)
library(corrplot)
#---------------------------------------------------------------------------------
#IMPORTING THE DATASET
Customer_Details = read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx",
                              sheet="Bank_Personal_Loan_Modelling") 
Dataset_Abbreviations = read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx",
                                   sheet="ReadMe")
# Importing Specific dataset using sheet options

anyNA(Customer_Details)     # Checks for the Missing Values 
sum(is.na(Customer_Details))# Counts the number of Missing Values
# Entry of missing values have been confirmed and there are 18 entries can be spotted.
colSums(is.na(Customer_Details)) # Tabulating the missing value entry
md.pattern(Customer_Details) # Graphical Representation of Missing Values
# Evidently only Family members attribute contains missing values.

# Before using R developed software to treat the missing values. Customer ID needs to be removed.
#Customer_Details = Customer_Details_ID[,-1]
colnames(Customer_Details)
# "mice" function is used to treat the missing values.

colnames(Customer_Details)=make.names(colnames(Customer_Details)) # Omiting the space left in the coloumn headers
init.impute = mice(Customer_Details, m = 5, 
                   method = "pmm", seed = 123)
Customer_Details_Treated = complete(init.impute, 2)
md.pattern(Customer_Details_Treated) 
sum(is.na(Customer_Details_Treated))
# The missing value treatment is confirmed with the 
# numerical search and graphical representation of the presence of missing values.
#------------------------------------------------------------------------------------
# CORRELATION MATRIX GENERATION
View(Customer_Details_Treated)
Customer_Details_Treated$Experience..in.years.= replace(Customer_Details_Treated$Experience..in.years.,Customer_Details_Treated$Experience..in.years.<0,0)
range(Customer_Details_Treated$Experience..in.years.)

attach(Customer_Details_Treated)

Customer_Details_Treated$ID = as.factor(Customer_Details_Treated$ID)
Customer_Details_Treated$ZIP.Code = as.factor(Customer_Details_Treated$ZIP.Code)
Customer_Details_Treated$Education = as.factor(Customer_Details_Treated$Education)
Customer_Details_Treated$Personal.Loan = as.factor(Customer_Details_Treated$Personal.Loan)
Customer_Details_Treated$Securities.Account = as.factor(Customer_Details_Treated$Securities.Account)
Customer_Details_Treated$CD.Account = as.factor(Customer_Details_Treated$CD.Account)
Customer_Details_Treated$Online = as.factor(Customer_Details_Treated$Online)
Customer_Details_Treated$CreditCard=as.factor(Customer_Details_Treated$CreditCard)

str(Customer_Details_Treated)


# DATA SPLIT - RANDOM FOREST
library(caTools)
set.seed(1000) #Input any random number
spl = sample.split(Customer_Details_Treated$Personal.Loan, SplitRatio = 0.7)
Customer_Train = subset(Customer_Details_Treated, spl == T)
dim(Customer_Train)
Customer_Test = subset(Customer_Details_Treated, spl == F)
dim(Customer_Test)

# DATA SPLIT - CART

library(caTools)
set.seed(1000) #Input any random number
spl = sample.split(Customer_Details_Treated$Personal.Loan, SplitRatio = 0.7)
CART_Train = subset(Customer_Details_Treated, spl == T)
dim(CART_Train)
CART_Test = subset(Customer_Details_Treated, spl == F)
dim(CART_Test)

