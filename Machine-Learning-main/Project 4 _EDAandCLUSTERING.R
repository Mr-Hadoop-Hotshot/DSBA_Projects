  

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

#---------------------------------------------------------------------------------
#SANITY CHECK
str(Customer_Details)
summary(Customer_Details)
# Summary outputs tells us "Family Memebers" attribute contains some "Missing Values"
# Let's confirm using missing value check code.


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
str(Customer_Details_Treated)
corrplot(cor(Customer_Details_Treated[,(1:13)]))

corrplot(cor(Customer_Details_Treated[,1:12]))

corData=cor(Customer_Details_Treated)
corrplot.mixed(corData,tl.pos = "lt")

summary(Customer_Details_Treated)

#----------------------------------------------------------------------------
# Univariate Analysis

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


# It can be observed that there some entries in the professional experience attribute has been entered 
# as a negative value. Realistically it's not possible. Therefore exsisting negative entry in professional experience attribute
# will be treated as "ZERO" years of expericence.

View(Customer_Details_Treated)

Customer_Details_Treated$Experience..in.years.= replace(Customer_Details_Treated$Experience..in.years.,Customer_Details_Treated$Experience..in.years.<0,0)
range(Customer_Details_Treated$Experience..in.years.)

summary(Customer_Details_Treated)
# -----------------------------------------------------------------------------------
#  Analysis on  CUSTOMER'S AGE attribute
#  -------------------------------------
range(Customer_Details_Treated$Age..in.years.)

# Histogram
hist(Customer_Details_Treated$Age..in.years.,col="Green",xlim=c(23,68),
     main="Customer Age Distribution",xlab="Age",breaks = 10)
boxplot(Customer_Details_Treated$Age..in.years.,
        horizontal = TRUE,col="Blue",main = "Customer's Age Distribution", xlab= "Age")

# OUTLIER CALCULATION
mean(Customer_Details_Treated$Age..in.years.)
median(Customer_Details_Treated$Age..in.years.)
Age_IQR = IQR(Customer_Details_Treated$Age..in.years.)
Age_IQR

#OUTLIERS - Q3
Q3 = quantile(Customer_Details_Treated$Age..in.years.,0.75)
Q3 = unname(Q3)
Q3

#OUTLIERs - Q1
Q1 = unname(quantile(Customer_Details_Treated$Age..in.years.,0.25))
Q1

# IQR CALCULATION
Max_Age_IQR = Q3+1.5*Age_IQR # Positive Outlier Calculation
Max_Age_IQR
Min_Age_IQR = Q1-1.5*Age_IQR # Negative Outlier Calculation
Min_Age_IQR

# No Of OUTLIERS
sum(Customer_Details_Treated$Age..in.years. > Max_Age_IQR)

# Print all Outliers
Customer_Details_Treated$Age..in.years.[Customer_Details_Treated$Age..in.years. > Max_Age_IQR]
Customer_Details_Treated$Age..in.years.[Customer_Details_Treated$Age..in.years. < Min_Age_IQR]


# -----------------------------------------------------------------------------------
#  Analysis on  CUSTOMER'S EXPERIENCE attribute
#  --------------------------------------------
range(Customer_Details_Treated$Experience..in.years.)

# Histogram
hist(Customer_Details_Treated$Experience..in.years.,col="Blue",xlim=c(0,43),
     main="Customer Experience Distribution",xlab="Age",breaks = 10)
boxplot(Customer_Details_Treated$Experience..in.years.,
        horizontal = TRUE,col="pink",main = "Customer's Experience Distribution", xlab= "Age")

# OUTLIER CALCULATION
mean(Customer_Details_Treated$Experience..in.years.)
median(Customer_Details_Treated$Experience..in.years.)
Experience_IQR = IQR(Customer_Details_Treated$Experience..in.years.)
Experience_IQR

#OUTLIERS - Q3
Q3 = quantile(Customer_Details_Treated$Experience..in.years.,0.75)
Q3 = unname(Q3)
Q3

#OUTLIERs - Q1
Q1 = unname(quantile(Customer_Details_Treated$Experience..in.years.,0.25))
Q1

# IQR CALCULATION
Max_Experience_IQR = Q3+1.5*Experience_IQR # Positive Outlier Calculation
Max_Experience_IQR
Min_Experience_IQR = Q1-1.5*Experience_IQR # Negative Outlier Calculation
Min_Experience_IQR

# No Of OUTLIERS
sum(Customer_Details_Treated$Experience..in.years. > Max_Experience_IQR)
sum(Customer_Details_Treated$Experience..in.years. < Max_Experience_IQR)

# Print all Outliers
Customer_Details_Treated$Experience..in.years.[Customer_Details_Treated$Experience..in.years. > Max_Experience_IQR]
Customer_Details_Treated$Experience..in.years.[Customer_Details_Treated$Experience..in.years. < Min_Experience_IQR]



# -----------------------------------------------------------------------------------
#  Analysis on  ZIP CODE attribute
#  --------------------------------------------
summary(Customer_Details_Treated)
ZIP_Code_Table=table(Customer_Details_Treated$ZIP.Code)
ZIP_Code_Table
pie(ZIP_Code_Table,col=rainbow(10))

# -----------------------------------------------------------------------------------
#  Analysis on  FAMILY MEMBERS attribute
#  --------------------------------------------
range(Customer_Details_Treated$Family.members)
colnames(Customer_Details_Treated)
# Histogram
hist(Customer_Details_Treated$Family.members,col="green",xlim=c(1,5),
     main="Family Members",xlab="Number of Members",breaks = 10)
boxplot(Customer_Details_Treated$Family.members,
        horizontal = TRUE,col="blue",main = "Family Members", xlab= "Number of Members")

# OUTLIER CALCULATION
mean(Customer_Details_Treated$Family.members)
median(Customer_Details_Treated$Family.members)
Family_IQR = IQR(Customer_Details_Treated$Family.members)
Family_IQR

#OUTLIERS - Q3
Q3 = quantile(Customer_Details_Treated$Family.members,0.75)
Q3 = unname(Q3)
Q3

#OUTLIERs - Q1
Q1 = unname(quantile(Customer_Details_Treated$Family.members,0.25))
Q1

# IQR CALCULATION
Max_Family_IQR = Q3+1.5*Family_IQR # Positive Outlier Calculation
Max_Family_IQR
Min_Family_IQR = Q1-1.5*Family_IQR # Negative Outlier Calculation
Min_Family_IQR

# No Of OUTLIERS
sum(Customer_Details_Treated$Family.members > Max_Family_IQR)
sum(Customer_Details_Treated$Family.members < Min_Family_IQR)

# Print all Outliers
Customer_Details_Treated$Family.members[Customer_Details_Treated$Family.members > Max_Family_IQR]
Customer_Details_Treated$Family.members[Customer_Details_Treated$Family.members < Min_Family_IQR]


# -----------------------------------------------------------------------------------
#  Analysis on  CCAVG attribute
#  --------------------------------------------

range(Customer_Details_Treated$CCAvg)
colnames(Customer_Details_Treated)
# Histogram
hist(Customer_Details_Treated$CCAvg,col="Red",xlim=c(0,10),
     main="CCAVg in $1,000s",xlab="CCAVg in $1,000s",breaks = 11)
boxplot(Customer_Details_Treated$CCAvg,
        horizontal = TRUE,col="blue",main = "CCAVg in $1,000s", xlab= "CCAVg in $1,000s")

# OUTLIER CALCULATION
mean(Customer_Details_Treated$CCAvg)
median(Customer_Details_Treated$CCAvg)
CCAVG_IQR = IQR(Customer_Details_Treated$CCAvg)
CCAVG_IQR 

#OUTLIERS - Q3
Q3 = quantile(Customer_Details_Treated$CCAvg,0.75)
Q3 = unname(Q3)
Q3

#OUTLIERs - Q1
Q1 = unname(quantile(Customer_Details_Treated$CCAvg,0.25))
Q1

# IQR CALCULATION
Max_CCAVG_IQR = Q3+1.5*CCAVG_IQR  # Positive Outlier Calculation
Max_CCAVG_IQR
Min_CCAVG_IQR = Q1-1.5*CCAVG_IQR  # Negative Outlier Calculation
Min_CCAVG_IQR

# No Of OUTLIERS
sum(Customer_Details_Treated$CCAvg > Max_CCAVG_IQR)
sum(Customer_Details_Treated$CCAvg < Min_CCAVG_IQR)

# Print all Outliers
Customer_Details_Treated$CCAvg[Customer_Details_Treated$CCAvg > Max_CCAVG_IQR]
Customer_Details_Treated$CCAvg[Customer_Details_Treated$CCAvg < Min_CCAVG_IQR]


# -----------------------------------------------------------------------------------
#  Analysis on  EDUCATION attribute
#  --------------------------------------------
EDUCATION_Table=table(Customer_Details_Treated$Education)
EDUCATION_Table
pie(EDUCATION_Table,col=rainbow(10))

# -----------------------------------------------------------------------------------
#  Analysis on  MORTAGE attribute
#  --------------------------------------------

range(Customer_Details_Treated$Mortgage)
colnames(Customer_Details_Treated)
# Histogram
hist(Customer_Details_Treated$Mortgage,col="Yellow",xlim=c(0,635),
     main="Mortgage in $1,000s",xlab="Mortgage in $1,000s",breaks = 20)
boxplot(Customer_Details_Treated$Mortgage,
        horizontal = TRUE,col="blue",main = "Mortgage in $1,000s", xlab= "Mortgage in $1,000s")

# OUTLIER CALCULATION
mean(Customer_Details_Treated$Mortgage)
median(Customer_Details_Treated$Mortgage)
Mortgage_IQR = IQR(Customer_Details_Treated$Mortgage)
Mortgage_IQR 

#OUTLIERS - Q3
Q3 = quantile(Customer_Details_Treated$Mortgage,0.75)
Q3 = unname(Q3)
Q3

#OUTLIERs - Q1
Q1 = unname(quantile(Customer_Details_Treated$Mortgage,0.25))
Q1

# IQR CALCULATION
Max_Mortgage_IQR = Q3+1.5*Mortgage_IQR  # Positive Outlier Calculation
Max_Mortgage_IQR
Min_Mortgage_IQR = Q1-1.5*Mortgage_IQR  # Negative Outlier Calculation
Min_Mortgage_IQR

# No Of OUTLIERS
sum(Customer_Details_Treated$Mortgage > Max_Mortgage_IQR)
sum(Customer_Details_Treated$Mortgage < Min_Mortgage_IQR)

# Print all Outliers
Customer_Details_Treated$Mortgage[Customer_Details_Treated$Mortgage > Max_Mortgage_IQR]
Customer_Details_Treated$Mortgage[Customer_Details_Treated$Mortgage < Min_Mortgage_IQR]


# -----------------------------------------------------------------------------------
#  Analysis on  FAMILY MEMBERS attribute
#  --------------------------------------------
range(Customer_Details_Treated$Family.members)
colnames(Customer_Details_Treated)
# Histogram
hist(Customer_Details_Treated$Family.members,col="Yellow",xlim=c(0,635),
     main="Family.members",xlab="Family.members",breaks = 20)
boxplot(Customer_Details_Treated$Family.members,
        horizontal = TRUE,col="blue",main = "Family.members", xlab= "Family.members")

# OUTLIER CALCULATION
mean(Customer_Details_Treated$Family.members)
median(Customer_Details_Treated$Family.members)
Family_Members_IQR = IQR(Customer_Details_Treated$Family.members)
Family_Members_IQR 

#OUTLIERS - Q3
Q3 = quantile(Customer_Details_Treated$Family.members,0.75)
Q3 = unname(Q3)
Q3

#OUTLIERs - Q1
Q1 = unname(quantile(Customer_Details_Treated$Family.members,0.25))
Q1

# IQR CALCULATION
Max_Family.members_IQR = Q3+1.5*Family.members_IQR  # Positive Outlier Calculation
Max_Family.members_IQR
Min_Family.members_IQR = Q1-1.5*Family.members_IQR  # Negative Outlier Calculation
Min_Family.members_IQR

# No Of OUTLIERS
sum(Customer_Details_Treated$Family.members > Max_Family.members_IQR)
sum(Customer_Details_Treated$Family.members < Min_Family.members_IQR)

# Print all Outliers
Customer_Details_Treated$Family.members[Customer_Details_Treated$Family.members > Max_Family.members_IQR]
Customer_Details_Treated$Family.members[Customer_Details_Treated$Family.members < Min_Family.members_IQR]

# -----------------------------------------------------------------------------------
#  Analysis on  PERSONAL LOAN attribute
#  --------------------------------------------

Per_Loan_Table=table(Customer_Details_Treated$Personal.Loan)
Per_Loan_Table
pie(Per_Loan_Table,col=blues9)
# -----------------------------------------------------------------------------------
#  Analysis on  SECURITIES ACCOUNT attribute
#  --------------------------------------------
Securities_Account_Table = table(Customer_Details_Treated$Securities.Account)
Securities_Account_Table
pie(Securities_Account_Table,col=rainbow(5))

# -----------------------------------------------------------------------------------
#  Analysis on  CD ACCOUNT attribute
#  --------------------------------------------

CD_Account_Table = table(Customer_Details_Treated$CD.Account)
CD_Account_Table
pie(CD_Account_Table,col=blues9)
colnames(Customer_Details_Treated)
# -----------------------------------------------------------------------------------
#  Analysis on  ONLINE attribute
#  --------------------------------------------
Online_Table = table(Customer_Details_Treated$Online)
Online_Table
pie(Online_Table,col=rainbow(9))
colnames(Customer_Details_Treated)

# -----------------------------------------------------------------------------------
#  Analysis on  CREDIT CARD attribute
#  --------------------------------------------
CreditCard_Table = table(Customer_Details_Treated$CreditCard)
CreditCard_Table
pie(CreditCard_Table,col=rainbow(9))

colnames(Customer_Details_Treated)

# -----------------------------------------------------------------------------------
#  MULTI_VARIATE ANALYSIS
#  ----------------

colnames(Customer_Details_Treated)

str(Customer_Details_Treated)



library(rpivotTable)

colnames(Customer_Details_Treated)
rpivotTable(Customer_Details_Treated)
range(Customer_Details_Treated$Experience..in.years.)


by(Customer_Details_Treated,INDICES = Customer_Details_Treated$Personal.Loan,FUN=summary)
?by


# -----------------------------------------------------------------------------------
#  K MEANS CLUSTERING
#  ----------------
library(dplyr)
# Scaling the attributes to improve the normalisation of the dataset.
Customer_Details_Treated.Scaled = scale(dplyr::select_if(Customer_Details_Treated,is.numeric))
print(Customer_Details_Treated.Scaled1,head=10)

seed=1000
set.seed(seed) #since kmeans uses a randomized starting point for cluster centroids

# Random cluster number 10 is chossen and iterated for 5 times to calculate 
clust2 = kmeans(x=Customer_Details_Treated.Scaled, centers = 2, nstart = 5)
print(clust2)


install.packages("cluster")
library(cluster)
clusplot(Customer_Details_Treated.Scaled, clust2$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1) # Clusplot generation code.

library(ggplot2) 
install.packages("factoextra")
library(factoextra) # Required for choosing optimum K 
set.seed(seed) # function to compute total within-cluster sum of squares 
fviz_nbclust(Customer_Details_Treated.Scaled[,1:5], kmeans, method = "wss", k.max = 25) + 
  theme_minimal() + ggtitle("The Elbow Method")



# function to perform k-means clustering 
kmeans.clus = kmeans(Customer_Details_Treated.Scaled[, 1:5], 
                     centers = 22, nstart = 25)
install.packages("fpc")
library(fpc) # Plots the clusters 
plotcluster( Customer_Details_Treated.Scaled[, 1:5], 
             kmeans.clus$cluster) # Uses a projection method for plotting
View(Customer_Details_Treated.Scaled)
library(NbClust)
NuClust = NbClust(data = Customer_Details_Treated.Scaled[, c(1,2,3,4,5)], distance="euclidean", 
                  min.nc = 2, max.nc = 5, method = "kmeans")
#-----------------------------------------------------------------------------------------


# NOT YET
table(NuClust$Best.n[1,])
set.seed(seed) #since kmeans uses a randomized starting point for cluster centroids

clust3 = kmeans(x=custSpendData.Scaled, centers = 3, nstart = 5)
print(clust3)


clusplot(custSpendData.Scaled, clust3$cluster, color=TRUE, shade=TRUE, labels=2, lines=1)


#Adding the cluster numbers back to the dataset

custSpendData$Clusters = clust3$cluster
print(custSpendData)


## Aggregate columns 3:7 for each cluster by their means
custProfile = aggregate(Customer_Details_Treated[,c(1,2,3,6,8)],list(Customer_Details_Treated_Scaled$Cluster),FUN="mean")
print(custProfile)


