
1. Use the given link below and locate the bank marketing dataset. Data Set Link
Perform the below operations:

library(stringr)
library(reshape2)
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(cluster)
library(HSAUR)
library(fpc)
library(lattice)
library(rpart)
library(kernlab)
library(randomForest)


setwd("F:/AcadGild/Workings")   

library(readr)   

#bank full data
Bank_full<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank/bank-full.csv",delim=";",
                      escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
View(Bank_full)
str(Bank_full)
class(Bank_full)

#bank data
Bank<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank/bank.csv",delim=";",
                 escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)
View(Bank)
str(Bank)
class(Bank)


#bank additional full data
bank_addl_full<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank-additional/bank-additional/bank-additional-full.csv",delim=";",
                           escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)    

head(bank_addl_full)
str(bank_addl_full)
names(bank_addl_full)

#bank additional data
bank_addl<-read_delim("F:/AcadGild/Files/Assignment11thdata/bank-additional/bank-additional/bank-additional.csv",delim=";",
                      escape_double = FALSE, trim_ws = TRUE, col_names = TRUE)    
head(bank_addl)
str(bank_addl)


  (a). Is there any association between Job and default?
  
# Selecting observations to determine cluster parameters(charecter to numeric)
JD <- data.frame(as.numeric(as.factor(bank_addl_full$job)),
                   as.numeric(as.factor(bank_addl_full$default)))
JD

# Rename the columns
colnames(EJ) <- c("education", "job")

# Reduce the amount of dataset records for legibility within clusters
JD1 <- JD[sample(nrow(JD),500),]
JD1

# Kmeans clustering to create 5 clusters
set.seed(12345)
JD2 <- kmeans(JD1, centers=5)
JD2

library(dplyr)
library(MASS)
#chisqtest
df<-table(JD$default,JD$job)
df
chisq.test(df)

(b). Is there any significant difference in duration of last call between people having housing loan or not?
  
# Selecting observations to determine cluster parameters(charecter to numeric)
HL <- data.frame(as.numeric(as.factor(bank_addl_full$housing)),
                   as.numeric(as.factor(bank_addl_full$loan)))
HL

# Rename the columns
colnames(HL) <- c("education", "job")

# Reduce the amount of dataset records for legibility within clusters
HL1 <- HL[sample(nrow(HL),500),]
HL1

# Kmeans clustering to create 5 clusters
set.seed(12345)
HL2 <- kmeans(HL1, centers=5)
HL2

library(dplyr)
library(MASS)
#chisqtest
df<-table(HL$default,HL$job)
df
chisq.test(df) 


  
  (c). Is there any association between consumer price index and consumer?
# Selecting observations to determine cluster parameters(charecter to numeric)
CPA <- data.frame(as.numeric(as.factor(bank_addl_full$cons.price.inx)),
                   as.numeric(as.factor(bank_addl_full$age)))
CPA

# Rename the columns
colnames(CPA) <- c("consumer.price.index", "age")

# Reduce the amount of dataset records for legibility within clusters
CPA1 <-CPAL[sample(nrow(CPA),500),]
CPA1

# Kmeans clustering to create 5 clusters
set.seed(12345)
CPA2 <- kmeans(CPA1, centers=5)
CPA2

library(dplyr)
library(MASS)
#chisqtest
df<-table(CPA$consumer_price_index,CPA$age)
df
chisq.test(df) 
  
  
  
  (d). Is the employment variation rate consistent across job types?
  
# Selecting observations to determine cluster parameters(charecter to numeric)
EVJ <- data.frame(as.numeric(as.factor(bank_addl_full$emp.var.rate)),
                   as.numeric(as.factor(bank_addl_full$job)))
EVJ

# Rename the columns
colnames(EVJ) <- c("emp.var.rate", "job")

# Reduce the amount of dataset records for legibility within clusters
EVJ1 <- EVJ[sample(nrow(EVJ),500),]
EVJ1

# Kmeans clustering to create 5 clusters
set.seed(12345)
EVJ2 <- kmeans(EVJ1, centers=5)
EVJ2

library(dplyr)
library(MASS)
#chisqtest
df<-table(EVJ$default,EVJ$job)
df
chisq.test(df) 

  
  
  (e). Is the employment variation rate same across education?
  
# Selecting observations to determine cluster parameters(charecter to numeric)
EVE <- data.frame(as.numeric(as.factor(bank_addl_full$emp.var.rate)),
                    as.numeric(as.factor(bank_addl_full$education)))
EVE

# Rename the columns
colnames(EVE) <- c("emp.var.rate", "education")

# Reduce the amount of dataset records for legibility within clusters
EVE1 <- EVE[sample(nrow(EVE),500),]
EVE1

# Kmeans clustering to create 5 clusters
set.seed(12345)
EVE2 <- kmeans(EVE1, centers=5)
EVE2

library(dplyr)
library(MASS)
#chisqtest
df<-table(EVE$default,EVE$job)
df
chisq.test(df) 

  
  
  (f). Which group is more confident?
