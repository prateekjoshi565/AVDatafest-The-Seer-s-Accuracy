library(data.table)

seer = fread("C:/Users/Prateek/Desktop/Prateek GEN/Analytics Vidhya/The Seers Accuracy/raw/train.csv")

length(unique(seer$Client_ID)) # 417107 unique customers are there
length(unique(seer$Store_ID)) # 298 stores are there

table(seer$Gender)
seer$Gender = as.factor(seer$Gender)
levels(seer$Gender) = c("M","M","F","M")

seer$Transaction_Date = as.Date(seer$Transaction_Date, "%d-%b-%y") # changed date format
seer$DOB = as.Date(seer$DOB, "%d-%b-%y") # changed date format
seer$DOB = format(seer$DOB, "19%y-%m-%d")

seer$DOB_year = year(seer$DOB)  # created a new variable, the year of birth of the customer
sort(table(seer$DOB_year))

# new variable indicating age categories of the clients
seer$age_class = ifelse(seer$DOB_year<1956, "Old_age", 
                        ifelse(seer$DOB_year>=1956 & seer$DOB_year<1966,"Mid_age", "Young"))

seer$Referred_Friend = as.factor(seer$Referred_Friend)
levels(seer$Referred_Friend) = c("NO", "NO", "YES")

# imputing missing values with median
seer$Payment_Mode = as.factor(seer$Payment_Mode)
levels(seer$Payment_Mode) = c("Credit/Debit Card", "Cash", "Cheque", "Credit/Debit Card", "Other")

library(caret)

seer$Purchased_in_Sale = as.factor(seer$Purchased_in_Sale)
seer$Sales_Executive_Category = as.factor(seer$Sales_Executive_Category)
seer$Lead_Source_Category = as.factor(seer$Lead_Source_Category)
seer$Product_Category = as.factor(seer$Product_Category)


seer$age_class = as.factor(seer$age_class)
seer$Age = year(seer$Transaction_Date)-seer$DOB_year
seer$DOB_year = NULL

seer$Transaction_Amount = seer$Transaction_Amount/120000
seer$Transaction_Amount[seer$Transaction_Amount > 1]=1

seer$client_category = as.factor(substr(as.character(seer$Client_ID), 1, 4))
seer$store_type = as.factor(substr(seer$Store_ID,1, 5))
seer$trans_type = as.factor(substr(seer$Transaction_ID,1,6))

seer$mask = paste(seer$Var1, seer$Var2, seer$Var3, sep = "")
seer$mask = as.factor(seer$mask)

seer$trans_year = year(seer$Transaction_Date)
seer$trans_year = ifelse(seer$trans_year == 2003, 0, ifelse(seer$trans_year == 2004, 1, ifelse(seer$trans_year ==2005, 2, 3)))

# making dummy variables
dmy = dummyVars("~ Purchased_in_Sale + Gender + Referred_Friend + Sales_Executive_Category + Lead_Source_Category + Payment_Mode + Product_Category + age_class + 
                client_category + store_type + trans_type + mask", data = seer, fullRank = T)
trsf <- data.frame(predict(dmy, newdata = seer))

seer = cbind(seer, trsf)
rm(trsf)


############################################################

training = seer[seer$trans_year < 3]
data_2006 = seer[seer$trans_year == 3]
data_2006 = unique(data_2006, by = 9)

a = match(training$Client_ID, data_2006$Client_ID)

# creating dependent variable
training$dep = a 
training$dep[is.na(training$dep)] = 0
training$dep[training$dep!= 0] = 1  

rm(data_2006)

#######################################################################
seer = as.data.frame(seer)
training = as.data.frame(training)

predictors = c("Transaction_Amount","Age","trans_year",
               "Purchased_in_Sale.Y", "Gender.F", "Referred_Friend.YES", "Sales_Executive_Category.B", 
               "Sales_Executive_Category.C", "Sales_Executive_Category.D", "Sales_Executive_Category.E", 
               "Lead_Source_Category.Other", "Lead_Source_Category.Reference", 
               "Lead_Source_Category.Walkin", "Payment_Mode.Cash", "Payment_Mode.Cheque", 
               "Payment_Mode.Other", "Product_Category.Cat.B", "Product_Category.Cat.C", 
               "Product_Category.Cat.D", "Product_Category.Cat.E", "Product_Category.Cat.F", 
               "Product_Category.Cat.G", "Product_Category.Cat.H", "age_class.Old_age", 
               "age_class.Young", "client_category.3457", "client_category.3458", 
               "client_category.3459", "client_category.3460", "client_category.3461", 
               "client_category.3462", "client_category.3463", "client_category.3464", 
               "client_category.3465", "client_category.3466", "store_type.STO13", 
               "store_type.STO14", "store_type.STO15", "store_type.STO16", "store_type.STO17", 
               "trans_type.TRA988", "trans_type.TRA989", "trans_type.TRA990", 
               "trans_type.TRA991", "trans_type.TRA992", "trans_type.TRA993", 
               "trans_type.TRA994", "trans_type.TRA995", "trans_type.TRA996", 
               "trans_type.TRA997", "trans_type.TRA998", "trans_type.TRA999", 
               "mask.112", "mask.121", "mask.122", "mask.211", "mask.212", "mask.221", 
               "mask.222", "mask.311", "mask.312", "mask.321", "mask.322")

glmFit = glm(training$dep ~ .,  data = training[,predictors], family = "binomial")

seer$pred = predict(glmFit, seer[,predictors], type = 'response')

library(dplyr)

submit = seer %>%
  group_by(Client_ID) %>%
  summarise(Cross_Sell = mean(pred))

write.csv(submit, "C:/Users/Prateek/Desktop/Prateek GEN/Analytics Vidhya/The Seers Accuracy/submissions/18th submission.csv", row.names = F)