#Get data
fin <- read.csv("Future-500-Dataset.csv", stringsAsFactors = T, na.strings = c(""))

### DATA PREPARATION

#Factorizing ID and Inception columns
fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

### Cleaning characters from numeric columns 
#Cleaning Expenses column
fin$Expenses <- gsub("\\D", "", fin$Expenses)
fin$Expenses <- as.numeric(fin$Expenses)

#Cleaning Revenue column
fin$Revenue <- gsub("[$,]", "", fin$Revenue)
fin$Revenue <- as.numeric(fin$Revenue)

#Cleaning Growth column
fin$Growth <- gsub("%", "", fin$Growth)
fin$Growth <- as.numeric(fin$Growth)

### Missing Data
#all rows containing missing data (to be delt with)
fin[!complete.cases(fin),]

#backup
fin_backup <- fin
fin <- fin_backup

#Removing rows where Industry is empty
#Industry is central to our analysis, rows where we don't know which industry the company belongs to are useless
fin <- fin[!is.na(fin$Industry),] #2 rows deleted

#Resetting dataframe index
rownames(fin) <- NULL

#Rows with missing States
fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"

#Replacing missing data by the industry's median
#Employees & Growth column
med_empl_retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm=T)
fin[is.na(fin$Employees) & fin$Industry=="Retail", "Employees"] <- med_empl_retail

med_empl_fin <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = T)
fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees"] <- med_empl_fin

med_growth_constr <- median(fin[fin$Industry == "Construction","Growth"], na.rm = T)
fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] <- med_growth_constr


#Revenue
med_rev_constr <- median(fin[fin$Industry == "Construction","Revenue"], na.rm = T)
fin[fin$Industry == "Construction" & is.na(fin$Revenue), "Revenue"] <- med_rev_constr

#Expenses
#row where we have revenue & profit, just substract
fin[is.na(fin$Expenses),"Expenses"]<- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]
fin[is.na(fin$Profit), "Profit"] <- fin[fin$Revenue & is.na(fin$Profit), "Revenue"] - fin[fin$Expenses & is.na(fin$Profit), "Expenses"]

#for the rest, use industry median
med_exp_constr <- median(fin[fin$Industry == "Construction","Expenses"], na.rm = T)
fin[fin$Industry == "Construction" & is.na(fin$Expenses) & is.na(fin$Profit), "Expenses"] <- med_exp_constr

### DATA VISUALIZATION
# We are going to produce the following:
# 1. A scatterplot classified by industry showing revenue, expenses, profit
# 2. A scatterplot that includes industry trends for the expenses~revenue relationship
# 3. BoxPlots showing growth by industry

#install.packages("ggplot2")
library(ggplot2)

# 1. industry showing revenue, expenses, profit
p <- ggplot(data=fin)
p + geom_point(aes(x=Revenue, y=Expenses,
                   color=Industry, size=Profit))


# 2.industry trends for the expenses~revenue relationship
q <- ggplot(data=fin, aes(x=Revenue, y=Expenses,
                          color=Industry))
q + geom_point() +
  geom_smooth(fill=NA, size=1.2)

# 3. boxplot
r <- ggplot(data=fin, aes(x=Industry, y=Growth,
            color=Industry))
r + geom_boxplot() +
  geom_jitter(size=1, alpha=0.6)
