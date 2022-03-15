library (tidyr)
library(tidyverse)
library(dplyr)
library(tidymodels)
library(ggplot2)
library(workflows)
library(tibble)
library(readxl)
library(stringr)
library(spatstat)
library(ranger)
library(corrplot)
library(ggcorrplot)
library(randomForest)
library(smotefamily)
library(ggfortify)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(2022)
#load csv data
transactions <- read_csv("transactions.csv")
customers <- read_csv("customers.csv")
geo <-read_csv("geo.csv")
solution <- read_csv("private_test_set.csv")
#####################MASTER CONSTRUCTION########################################

#------------------get transaction table ready for joining----------------------
#cast customer ID to numeric in new column
transactions$CUSTOMER_PARSED <- parse_number(transactions$CUSTOMER)
#fix unidentifiable customers in test and remove others
transactions[transactions$SO_ID == 'a030N00001OOYcGQAX', ]$SALES_LOCATION <- 'Tours Ouest'
transactions[transactions$SO_ID == 'a030N00001OP8z6QAD', ]$SALES_LOCATION <- 'Poitiers Sud Ouest'
transactions <- transactions[!is.na(transactions$SALES_LOCATION),]
#cast SALES_LOCATION to factor
transactions$SALES_LOCATION <- as.factor(transactions$SALES_LOCATION)

#-------------------------------------------------------------------------------

#------------------get customers table ready for joining------------------------
#build New_ID
customers <- customers %>% mutate(New_ID = case_when(
  endsWith(COUNTRY, "d") ~ paste("CH", CUSTOMER, sep = ""), 
  endsWith(COUNTRY, "e") ~ paste("FR", CUSTOMER, sep = "")))
#-------------------------------------------------------------------------------

#-----------Building Exchange Rate Dataset and joining with Customers-----------
a <- c("Euro", "US Dollar", "Chinese Yuan", "Pound Sterling")
b <- c(1, 1.1632, 7.6277, 0.8458)
currencies <- as.data.frame(cbind(a, b))
currencies$CURRENCY <- currencies$a
currencies$Exchange_Rate <- currencies$b
currencies$a <- NULL
currencies$b <- NULL
customers <- left_join(customers, currencies)
rm(currencies, a, b)
#-------------------------------------------------------------------------------

#--------------------Mean imputing Revenues in Customer Dataset-----------------
#Setting negative revenue to zero
customers$CURRENCY <- as.factor(customers$CURRENCY)
customers$Exchange_Rate <- as.numeric(customers$Exchange_Rate)
customers[customers$REV_CURRENT_YEAR.1 < 0, ]$REV_CURRENT_YEAR.1 <- 0
customers[customers$REV_CURRENT_YEAR.2 < 0, ]$REV_CURRENT_YEAR.2 <- 0
#Computing Revenues in EUR
customers$REV_1_EUR <- customers$REV_CURRENT_YEAR.1 / customers$Exchange_Rate
customers$REV_2_EUR <- customers$REV_CURRENT_YEAR.2 / 1
#Mean Imputation for zero revenue data
govcustomers1 <- customers[customers$OWNERSHIP == 'Governmental',]
govcustomers1 <- govcustomers1[govcustomers1$REV_1_EUR > 0,]
govmean1 <- mean(govcustomers1$REV_1_EUR)
govcustomers2 <- customers[customers$OWNERSHIP == 'Governmental',]
govcustomers2 <- govcustomers2[govcustomers2$REV_2_EUR > 0,]
govmean2 <- mean(govcustomers2$REV_2_EUR)
firmcustomers1 <- customers[customers$OWNERSHIP == 'Privately Owned/Publicly Traded',]
firmcustomers1 <- firmcustomers1[firmcustomers1$REV_1_EUR > 0,]
firmmean1 <- mean(firmcustomers1$REV_1_EUR)
firmcustomers2 <- customers[customers$OWNERSHIP == 'Privately Owned/Publicly Traded',]
firmcustomers2 <- firmcustomers2[firmcustomers2$REV_2_EUR > 0,]
firmmean2 <- mean(firmcustomers2$REV_2_EUR)
ncustomers1 <- customers[customers$OWNERSHIP == 'No information',]
ncustomers1 <- ncustomers1[ncustomers1$REV_1_EUR > 0,]
nmean1 <- mean(ncustomers1$REV_1_EUR)
ncustomers2 <- customers[customers$OWNERSHIP == 'No information',]
ncustomers2 <- ncustomers2[ncustomers2$REV_2_EUR > 0,]
nmean2 <- mean(ncustomers2$REV_2_EUR)
remove(ncustomers1)
remove(ncustomers2)
remove(govcustomers1)
remove(govcustomers2)
remove(firmcustomers1)
remove(firmcustomers2)
customers$mean_impute <- 0
#Mean Imputing Firm Customers
customers[customers$REV_1_EUR <= 0.1 & customers$REV_2_EUR <= 0.1 & customers$OWNERSHIP == 'Privately Owned/Publicly Traded', ]$mean_impute <- 1
#Mean Imputing Government Agencies
customers[customers$REV_2_EUR <= 0.1 & customers$REV_1_EUR <= 0.1 & customers$OWNERSHIP == 'Governmental', ]$mean_impute <- 1
#Mean Imputing Non-Information 
customers[customers$REV_2_EUR <= 0.1 & customers$REV_1_EUR <= 0.1 & customers$OWNERSHIP == 'No information', ]$mean_impute <- 1
customers[customers$OWNERSHIP == 'Governmental' & customers$mean_impute == 1,]$REV_1_EUR <- govmean1
customers[customers$OWNERSHIP == 'Governmental' & customers$mean_impute == 1,]$REV_2_EUR <- govmean2
customers[customers$OWNERSHIP == 'No information' & customers$mean_impute == 1,]$REV_1_EUR <- nmean1
customers[customers$OWNERSHIP == 'No information' & customers$mean_impute == 1,]$REV_2_EUR <- nmean2
customers[customers$OWNERSHIP == 'Privately Owned/Publicly Traded' & customers$mean_impute == 1,]$REV_1_EUR <- firmmean1
customers[customers$OWNERSHIP == 'Privately Owned/Publicly Traded' & customers$mean_impute == 1,]$REV_2_EUR <- firmmean2
customers$mean_impute <- NULL
customers$REV_CURRENT_YEAR <- NULL
customers$REV_CURRENT_YEAR.1 <- NULL
customers$REV_CURRENT_YEAR.2 <- NULL
remove(govmean1)
remove(govmean2)
remove(firmmean1)
remove(firmmean2)
remove(nmean1)
remove(nmean2)
#-------------------------------------------------------------------------------

#-----------------Computing Annual Growth of Customers--------------------------
customers$GROWTH <- (customers$REV_1_EUR - customers$REV_2_EUR)/customers$REV_2_EUR
customers[is.nan(customers$GROWTH), ]$GROWTH <- 0
customers[is.infinite(customers$GROWTH), ]$GROWTH <- 0
customers[customers$REV_1_EUR < 1,]$GROWTH <- 0
customers[customers$OWNERSHIP == "Individual Person", ]$GROWTH <- 0
customers$CUSTOMER <- NULL
customers$COUNTRY <- NULL
customers$Exchange_Rate <- NULL
#-------------------------------------------------------------------------------

#----------------------joining GEO and transactions-----------------------------
#casts type of SALES_LOCATION from character to factor
geo$SALES_LOCATION<- as.factor(geo$SALES_LOCATION)
#Replaces the NA-value with Paris 
geo$SALES_OFFICE[is.na(geo$SALES_OFFICE)] = 'Paris'
#omits the row of NAs
geo<-na.omit(geo)
transactions <- left_join(transactions, geo, by = "SALES_LOCATION")
remove(geo)
#-------------------------------------------------------------------------------

#------------------joining transactions and customer tables---------------------
transactions$CUSTOMER_PARSED <- as.factor(transactions$CUSTOMER_PARSED)
#mutate data frame to get new unique identifier
transactions <- transactions %>% mutate(New_ID = case_when(
  is.na(CUSTOMER_PARSED) ~ "NA", 
  !(is.na(CUSTOMER_PARSED)) ~ paste(COUNTRY, CUSTOMER_PARSED, sep = "")
))
transactions$CUSTOMER <- NULL
transactions$CUSTOMER_PARSED <- NULL
master <- left_join(transactions, customers, by = "New_ID")
remove(customers)
remove(transactions)
#-------------------------------------------------------------------------------

#--------------------creation of ISIC industry identifier-----------------------
#Read in ISIC data (https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_4_english_structure.Txt)
ISICs <- read_xlsx("ISIC.xlsx")
master$ISIC_Factor <- as.character(master$ISIC)
#Isolate non NA, non-zero 3-digit codes and add 0.
for(i in 1:nrow(master)) {
  if(master$ISIC[i] < 1000 && !is.na(master$ISIC[i]) && master$ISIC[i]!= 0) {
    master$ISIC_Factor[i] <- paste("0", master$ISIC_Factor[i], sep = "")
  }
}
master$Code <- master$ISIC_Factor
master$ISIC_Factor <- NULL
master <- left_join(master, ISICs, by = "Code")
#Get first two digits of Codes
master$Two_Digit_ISIC <- substr(master$Code, 1, 2)
master$Two_Digit_ISIC <- as.factor(master$Two_Digit_ISIC)
master$Code <- NULL
master$ISIC <-NULL
#-------------------------------------------------------------------------------

#---------------Fix Service and Product Cost, END_CUSTOMER Columns--------------
master[master$SERVICE_COST < 0, ]$SERVICE_COST <- 0
master[master$COSTS_PRODUCT_B < 0, ]$COSTS_PRODUCT_B <- 0
#Modification of END_CUSTOMER: Either counterpart is endcustomer (yes) or not (no) or NA
for(i in 1:nrow(master)) {
  if(master$END_CUSTOMER[i] == substr(master$New_ID[i], 3, length(master$New_ID[i])) && !is.na(master$END_CUSTOMER[i])) {
    master$END_CUSTOMER[i] <- "Yes"
  }
  else if(master$END_CUSTOMER[i] != "Yes" && !is.na(master$END_CUSTOMER[i])) {
    master$END_CUSTOMER[i] <- "No"
  }
}
#If End Customer = NA, we assume that customer is an end customer.
master$END_CUSTOMER <- replace_na(master$END_CUSTOMER, "Yes")
master$END_CUSTOMER <- as.factor(master$END_CUSTOMER)
#-------------------------------------------------------------------------------

#------------------Building Duration of customer relationship-------------------
#Reformating Creation Date, getting duration of client relationship and saving in variable duration
master$CREATION_YEAR <- (substr(gsub("/", ".", master$CREATION_YEAR), 7, 11))
master$DURATION <- 2022 - as.numeric(master$CREATION_YEAR)
master$DURATION <- replace_na(master$DURATION, 0)
#-------------------------------------------------------------------------------

#------------creating difference between main-and sub offer time----------------
#Reformating MO_Created_Date, SO_Created_Date
#Splitting into two data frames by date format
a <- master[nchar(master$MO_CREATED_DATE) < 17, ]
b <- master[nchar(master$MO_CREATED_DATE) > 16, ]
#Computing Correct Dates
a$correct_MO_Date <- parse_datetime(a$MO_CREATED_DATE, format = "%d.%m.%Y %H:%M")
b$correct_MO_Date <- parse_datetime(b$MO_CREATED_DATE, format = "%Y-%m-%d %H:%M:%S")
a$correct_SO_Date <- parse_datetime(a$SO_CREATED_DATE, format = "%d.%m.%Y %H:%M")
b$correct_SO_Date <- parse_datetime(b$SO_CREATED_DATE, format = "%Y-%m-%d %H:%M:%S")
#Merging Split Datasets and selecting relevant columns
a_and_b <- rbind(a, b)
a_and_b <- a_and_b[, c("SO_ID", "correct_MO_Date", "correct_SO_Date")]
#Joining with Master Data
master <- left_join(master, a_and_b, by = "SO_ID")
master$MO_CREATED_DATE <- NULL
master$SO_CREATED_DATE <- NULL
#Computing Difference
master$Difference_Min <- (master$correct_MO_Date - master$correct_SO_Date)/60
#-------------------------------------------------------------------------------

#------------------Fixing OFFER_STATUS column-----------------------------------
#Unification of the values and conversion to binary values 1 = Win /  0 = Lose 
master$OFFER_STATUS <- tolower(master$OFFER_STATUS)
i <- 1
while(i!=nrow(master)+1){
  if(grepl("w",master$OFFER_STATUS[i], fixed=TRUE)==TRUE && is.na(master$OFFER_STATUS[i])==FALSE){
    master$OFFER_STATUS[i]<-"1"
  }else{
    if(is.na(master$OFFER_STATUS[i])==FALSE){
      master$OFFER_STATUS[i]<-"0"
    }
  }
  i<-i+1
}
master$OFFER_STATUS <- as.numeric(master$OFFER_STATUS)
#-------------------------------------------------------------------------------

#--------------------factorise character columns--------------------------------
master$TECH <- as.factor(master$TECH)
master$OFFER_TYPE <- as.factor(master$OFFER_TYPE)
master$BUSINESS_TYPE <- as.factor(master$BUSINESS_TYPE)
master$COUNTRY <- as.factor(master$COUNTRY)
master$SALES_OFFICE <- as.factor(master$SALES_OFFICE)
master$SALES_BRANCH <- as.factor(master$SALES_BRANCH)
master$New_ID <- as.factor(master$New_ID)
master$OWNERSHIP[is.na(master$OWNERSHIP)] = "No information"
master$OWNERSHIP <- as.factor(master$OWNERSHIP)
master$PRICE_LIST <- as.factor(master$PRICE_LIST)
#-------------------------------------------------------------------------------

#--------------------remove unnecessary columns from master---------------------
master$CREATION_YEAR <- NULL
master$Description <- NULL
rm(a, a_and_b, b, ISICs)
#-------------------------------------------------------------------------------

#-----------creating TOTAL_COST and MARGIN and DISCOUNT columns-----------------
master <- master %>% mutate(TOTAL_COST = MATERIAL_COST + SERVICE_COST)
master <- master %>% mutate(PROFIT = OFFER_PRICE - TOTAL_COST)
master <- master %>% mutate(MARGIN = (OFFER_PRICE / TOTAL_COST) - 1)
#Computing Discount Numbers
master$DISCOUNT <- (master$SERVICE_LIST_PRICE - master$SERVICE_COST)/master$SERVICE_LIST_PRICE
for(i in 1:nrow(master)) {
  if(master$SERVICE_LIST_PRICE[i] <= 0.01 || is.na(master$SERVICE_LIST_PRICE[i])) {
    master$DISCOUNT[i] <- 0
  }
  else {
    master$DISCOUNT[i] <- (master$SERVICE_LIST_PRICE[i] - master$SERVICE_COST[i])/master$SERVICE_LIST_PRICE[i]
  }
}
#Capping Discounts
master[master$DISCOUNT < - 10, ]$DISCOUNT <- -10
#-------------------------------------------------------------------------------

#-----Final adjustments to missing values (mean imputation and no info)---------
rev_1_mean <- mean(master$REV_1_EUR, na.rm = TRUE)
rev_2_mean <- mean(master$REV_2_EUR, na.rm = TRUE)
master[is.na(master$REV_1_EUR), ]$REV_1_EUR <- rev_1_mean
master[is.na(master$REV_2_EUR), ]$REV_2_EUR <- rev_2_mean
remove(rev_1_mean)
remove(rev_2_mean)
remove(i)
master$GROWTH <- (master$REV_1_EUR - master$REV_2_EUR)/master$REV_2_EUR
master[is.nan(master$GROWTH), ]$GROWTH <- 0
master[is.infinite(master$GROWTH), ]$GROWTH<- 0
master[master$REV_1_EUR < 1,]$GROWTH <- 0
#Hard Coding Growth Zero for individual persons
master[master$OWNERSHIP == "Individual Person", ]$GROWTH <- 0
#Capping growth rates at 10
master[master$GROWTH >= 10, ]$GROWTH <- 10
#Turning ISIC NAs into 00
levels(master$Two_Digit_ISIC) <- c(levels(master$Two_Digit_ISIC), "00")
master[is.na(master$Two_Digit_ISIC), ]$Two_Digit_ISIC <- '00'
#Introducing "No Information" factor for missing currencies
levels(master$CURRENCY) <- c(levels(master$CURRENCY), "No Information")
master[is.na(master$CURRENCY), ]$CURRENCY <- 'No Information'
#remove multiple suboffers
multiple_subs <- master %>% count(MO_ID)
multiple_subs <- multiple_subs[multiple_subs$n > 1,]
multiple_subs <- merge(master, multiple_subs)
multiple_subs <- distinct(multiple_subs, MO_ID, .keep_all = TRUE)
#-------------------------------------------------------------------------------

#-------------------convert factors into dummies--------------------------------
#END_CUSTOMER
master$END_CUSTOMER <- ifelse(master$END_CUSTOMER == 'Yes', 1, 0)
#PRICE_LIST
master$LIST_SFT <- ifelse(master$PRICE_LIST == 'SFT Standard', 1, 0)
master$LIST_CMT_INST <- ifelse(master$PRICE_LIST == 'CMT Installer', 1, 0)
master$LIST_CMT_END <- ifelse(master$PRICE_LIST == 'CMT End Customer', 1, 0)
master$PRICE_LIST <- NULL
#TECH
master$TECH_BP <- ifelse(master$TECH == 'BP', 1, 0)
master$TECH_C <- ifelse(master$TECH == 'C', 1, 0)
master$TECH_E <- ifelse(master$TECH == 'E', 1, 0)
master$TECH_F <- ifelse(master$TECH == 'F', 1, 0)
master$TECH_FP <- ifelse(master$TECH == 'FP', 1, 0)
master$TECH_S <- ifelse(master$TECH == 'S', 1, 0)
master$TECH <- NULL
#OFFER_TYPE
master$OFFER_A <- ifelse(master$OFFER_TYPE == 'A', 1, 0)
master$OFFER_CI <- ifelse(master$OFFER_TYPE == 'CI', 1, 0)
master$OFFER_CP <- ifelse(master$OFFER_TYPE == 'CP', 1, 0)
master$OFFER_CPP <- ifelse(master$OFFER_TYPE == 'CPP', 1, 0)
master$OFFER_CS <- ifelse(master$OFFER_TYPE == 'CS', 1, 0)
master$OFFER_CST <- ifelse(master$OFFER_TYPE == 'CST', 1, 0)
master$OFFER_D <- ifelse(master$OFFER_TYPE == 'D', 1, 0)
master$OFFER_DCC <- ifelse(master$OFFER_TYPE == 'DCC', 1, 0)
master$OFFER_DCF <- ifelse(master$OFFER_TYPE == 'DCF', 1, 0)
master$OFFER_ED <- ifelse(master$OFFER_TYPE == 'ED', 1, 0)
master$OFFER_EH <- ifelse(master$OFFER_TYPE == 'EH', 1, 0)
master$OFFER_EN <- ifelse(master$OFFER_TYPE == 'EN', 1, 0)
master$OFFER_EV <- ifelse(master$OFFER_TYPE == 'EV', 1, 0)
master$OFFER_FD <- ifelse(master$OFFER_TYPE == 'FD', 1, 0)
master$OFFER_FDD <- ifelse(master$OFFER_TYPE == 'FDD', 1, 0)
master$OFFER_FDI <- ifelse(master$OFFER_TYPE == 'FDI', 1, 0)
master$OFFER_FED <- ifelse(master$OFFER_TYPE == 'FED', 1, 0)
master$OFFER_FEI <- ifelse(master$OFFER_TYPE == 'FEI', 1, 0)
master$OFFER_FIB <- ifelse(master$OFFER_TYPE == 'FIB', 1, 0)
master$OFFER_FIR <- ifelse(master$OFFER_TYPE == 'FIR', 1, 0)
master$OFFER_GA <- ifelse(master$OFFER_TYPE == 'GA', 1, 0)
master$OFFER_GAM <- ifelse(master$OFFER_TYPE == 'GAM', 1, 0)
master$OFFER_MSYS <- ifelse(master$OFFER_TYPE == 'MSYS', 1, 0)
master$OFFER_PAT <- ifelse(master$OFFER_TYPE == 'PAT', 1, 0)
master$OFFER_STD <- ifelse(master$OFFER_TYPE == 'STD', 1, 0)
master$OFFER_SU <- ifelse(master$OFFER_TYPE == 'SU', 1, 0)
master$OFFER_SYN<- ifelse(master$OFFER_TYPE == 'SYN', 1, 0)
master$OFFER_V <- ifelse(master$OFFER_TYPE == 'V', 1, 0)
master$OFFER_XCPS <- ifelse(master$OFFER_TYPE == 'XCPS', 1, 0)
master$OFFER_TYPE <- NULL
#BUSINESS_TYPE
master$B_C <- ifelse(master$BUSINESS_TYPE == 'C', 1, 0)
master$B_Exp <- ifelse(master$BUSINESS_TYPE == 'Exp', 1, 0)
master$B_F <- ifelse(master$BUSINESS_TYPE == 'F', 1, 0)
master$B_M <- ifelse(master$BUSINESS_TYPE == 'M', 1, 0)
master$B_Mig <- ifelse(master$BUSINESS_TYPE == 'Mig', 1, 0)
master$B_N <- ifelse(master$BUSINESS_TYPE == 'N', 1, 0)
master$B_New <- ifelse(master$BUSINESS_TYPE == 'New', 1, 0)
master$B_R <- ifelse(master$BUSINESS_TYPE == 'R', 1, 0)
master$B_S <- ifelse(master$BUSINESS_TYPE == 'S', 1, 0)
master$B_T <- ifelse(master$BUSINESS_TYPE == 'T', 1, 0)
master$BUSINESS_TYPE <- NULL
#SALES_LOCATION
master$S_Aix <- ifelse(master$SALES_LOCATION == 'Aix Sud-Est', 1, 0)
master$S_Basel_Central <- ifelse(master$SALES_LOCATION == 'Basel Central', 1, 0)
master$S_Bern_Central <- ifelse(master$SALES_LOCATION == 'Bern Central', 1, 0)
master$S_Bern_EPS <- ifelse(master$SALES_LOCATION == 'Bern EPS', 1, 0)
master$S_Besancon <- ifelse(master$SALES_LOCATION == 'BesanÃ§on Centre-Est', 1, 0)
master$S_Bezons <- ifelse(master$SALES_LOCATION == 'Bezons Grand Paris', 1, 0)
master$S_Bord <- ifelse(master$SALES_LOCATION == 'Bordeaux Sud Ouest', 1, 0)
master$S_Cler <- ifelse(master$SALES_LOCATION == 'Brest Ouest', 1, 0)
master$S_Cle <- ifelse(master$SALES_LOCATION == 'Clermont Centre-Est', 1, 0)
master$S_Dij <- ifelse(master$SALES_LOCATION == 'Dijon Centre-Est', 1, 0)
master$S_Exp <- ifelse(master$SALES_LOCATION == 'Export Grand Paris', 1, 0)
master$S_Gen <- ifelse(master$SALES_LOCATION == 'Geneva West', 1, 0)
master$S_Gra <- ifelse(master$SALES_LOCATION == 'Granges-Paccot West', 1, 0)
master$S_Gre <- ifelse(master$SALES_LOCATION == 'Grenoble Centre-Est', 1, 0)
master$S_Laus1 <- ifelse(master$SALES_LOCATION == 'Lausanne East', 1, 0)
master$S_Laus2 <- ifelse(master$SALES_LOCATION == 'Lausanne EPS', 1, 0)
master$S_Laus3 <- ifelse(master$SALES_LOCATION == 'Lausanne West', 1, 0)
master$S_Lil <- ifelse(master$SALES_LOCATION == 'Lille Nord FR', 1, 0)
master$S_Lim <- ifelse(master$SALES_LOCATION == 'Limoges Sud Ouest', 1, 0)
master$S_Luz <- ifelse(master$SALES_LOCATION == 'Luzern Central', 1, 0)
master$S_Lyo <- ifelse(master$SALES_LOCATION == 'Lyon Centre-Est', 1, 0)
master$S_Met <- ifelse(master$SALES_LOCATION == 'Metz Grand Est', 1, 0)
master$S_Mona <- ifelse(master$SALES_LOCATION == 'Monaco Sud-Est', 1, 0)
master$S_Mont <- ifelse(master$SALES_LOCATION == 'Montpellier Sud Ouest', 1, 0)
master$S_Nantes <- ifelse(master$SALES_LOCATION == 'Nantes Ouest', 1, 0)
master$S_Nic <- ifelse(master$SALES_LOCATION == 'Nice Sud-Est', 1, 0)
master$S_Others <- ifelse(master$SALES_LOCATION == 'Others Functions Enterprise Business France', 1, 0)
master$S_Pau <- ifelse(master$SALES_LOCATION == 'Pau Sud Ouest', 1, 0)
master$S_Poi <- ifelse(master$SALES_LOCATION == 'Poitiers Sud Ouest', 1, 0)
master$S_Rou <- ifelse(master$SALES_LOCATION == 'Rouen Nord FR', 1, 0)
master$S_Sio <- ifelse(master$SALES_LOCATION == 'Sion West', 1, 0)
master$S_StB <- ifelse(master$SALES_LOCATION == 'St. Blaise West', 1, 0)
master$S_StG <- ifelse(master$SALES_LOCATION == 'St. Gallen East', 1, 0)
master$S_Stei1 <- ifelse(master$SALES_LOCATION == 'Steinhausen Central', 1, 0)
master$S_Str <- ifelse(master$SALES_LOCATION == 'Strasbourg Grand Est', 1, 0)
master$S_Tic <- ifelse(master$SALES_LOCATION == 'Ticino Central', 1, 0)
master$S_Tou <- ifelse(master$SALES_LOCATION == 'Toulouse Sud Ouest', 1, 0)
master$S_Tours <- ifelse(master$SALES_LOCATION == 'Tours Ouest', 1, 0)
master$S_VelizyG <- ifelse(master$SALES_LOCATION == 'Velizy Grand Paris', 1, 0)
master$S_VelizyS <- ifelse(master$SALES_LOCATION == 'Velizy SI', 1, 0)
master$S_Ver <- ifelse(master$SALES_LOCATION == 'Vertical Market Enterprise Business France', 1, 0)
master$S_Zu1 <- ifelse(master$SALES_LOCATION == 'Zürich East', 1, 0)
master$S_Zu2 <- ifelse(master$SALES_LOCATION == 'Zürich EPS', 1, 0)
master$SALES_LOCATION <- NULL
#OWNERSHIP
master$gov <- ifelse(master$OWNERSHIP == 'Governmental', 1, 0)
master$firm <- ifelse(master$OWNERSHIP == 'Privately Owned/Publicly Traded', 1, 0)
master$person <- ifelse(master$OWNERSHIP == 'Individual Person', 1, 0)
master$OWNERSHIP <- NULL
#ISIC
master$IS_A <- ifelse(master$Two_Digit_ISIC %in% c('01', '02'), 1, 0)
master$IS_B <- ifelse(master$Two_Digit_ISIC %in% c('05', '06', '07', '08', '09'), 1, 0)
master$IS_C <- ifelse(master$Two_Digit_ISIC %in% c('10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '25', '26', '27', '28', '29', '30', '31', '32', '33'), 1, 0)
master$IS_D <- ifelse(master$Two_Digit_ISIC %in% c('35'), 1, 0)
master$IS_E <- ifelse(master$Two_Digit_ISIC %in% c('36', '37', '38', '39'), 1, 0)
master$IS_F <- ifelse(master$Two_Digit_ISIC %in% c('41', '42', '43'), 1, 0)
master$IS_G <- ifelse(master$Two_Digit_ISIC %in% c('45', '46', '47'), 1, 0)
master$IS_H <- ifelse(master$Two_Digit_ISIC %in% c('49', '50', '51', '52', '53'), 1, 0)
master$IS_I <- ifelse(master$Two_Digit_ISIC %in% c('55', '56'), 1, 0)
master$IS_J <- ifelse(master$Two_Digit_ISIC %in% c('58', '59', '60', '61', '62', '63'), 1, 0)
master$IS_K <- ifelse(master$Two_Digit_ISIC %in% c('64', '65', '66'), 1, 0)
master$IS_L <- ifelse(master$Two_Digit_ISIC %in% c('68'), 1, 0)
master$IS_M <- ifelse(master$Two_Digit_ISIC %in% c('69', '70', '71', '72', '73', '74', '75'), 1, 0)
master$IS_N <- ifelse(master$Two_Digit_ISIC %in% c('77', '78', '79', '80', '81', '82'), 1, 0)
master$IS_O <- ifelse(master$Two_Digit_ISIC %in% c('84'), 1, 0)
master$IS_P <- ifelse(master$Two_Digit_ISIC %in% c('85'), 1, 0)
master$IS_Q <- ifelse(master$Two_Digit_ISIC %in% c('86','87','88'), 1, 0)
master$IS_R <- ifelse(master$Two_Digit_ISIC %in% c('90', '91', '92', '93'), 1, 0)
master$IS_S <- ifelse(master$Two_Digit_ISIC %in% c('94', '95', '96'), 1, 0)
master$IS_T <- ifelse(master$Two_Digit_ISIC %in% c('97', '98'), 1, 0)
master$IS_U <- ifelse(master$Two_Digit_ISIC %in% c('99'), 1, 0)
master$Two_Digit_ISIC <- NULL
master$OFFER_STATUS <- as.factor(master$OFFER_STATUS)
#-------------------------------------------------------------------------------

#---------------removing columns unsuitable for training------------------------
master$SO_ID <- NULL
master$New_ID <- NULL
master$COUNTRY <- NULL
master$SALES_BRANCH <- NULL
master$SALES_OFFICE <- NULL
master$correct_MO_Date <- NULL
master$correct_SO_Date <- NULL
master$CURRENCY <- NULL
master$TOTAL_COST <- NULL
master$REV_2_EUR <- NULL
master$SERVICE_LIST_PRICE <- NULL
#-------------------------------------------------------------------------------

#------------------------converting difftime to numeric-------------------------
master$Difference_Min <- - 1 * as.numeric(master$Difference_Min)
#-------------------------------------------------------------------------------

#---------renaming master variables for better understanding--------------------
master$OFFER_CREATION_TIME <- master$Difference_Min
master$Difference_Min <- NULL
master$CUSTOMER_REVENUE <- master$REV_1_EUR
master$REV_1_EUR <- NULL
#-----------split master into test, artificial test and training data-----------
test_master <- filter(master, is.na(TEST_SET_ID) == FALSE)
training_master <- filter(master, is.na(TEST_SET_ID) == TRUE)
training_master$TEST_SET_ID <- NULL
training_master <- anti_join(training_master, multiple_subs, by = "MO_ID")
test_master$MO_ID <- NULL
training_master$MO_ID <- NULL
picked = sample(seq_len(nrow(training_master)), size = 4675)
test_master_artificial <- training_master[picked,]
training_master <- training_master [-picked,]
remove(picked)
remove(multiple_subs)
remove(master)
#-------------------------------------------------------------------------------

#----------Getting training sets with 50/50 split of accepted/not accepted------
smotedata <- SMOTE(training_master[setdiff(names(training_master), "OFFER_STATUS")], training_master$OFFER_STATUS, K = 5, dup_size = 3)
smotedata <- smotedata$data
smotedata$OFFER_STATUS <- as.factor(smotedata$class)
smotedata$class <- NULL
cutdata <- training_master[as.numeric(training_master$OFFER_STATUS) == 2,]
cutdata <- cutdata[sample(1:nrow(cutdata), 3698),]
cutdata2 <- training_master[as.numeric(training_master$OFFER_STATUS) == 1,]
cutdata <- rbind(cutdata, cutdata2)
overdata <- training_master
overdata <- rbind(overdata, cutdata2)
overdata <- rbind(overdata, cutdata2)
overdata <- rbind(overdata, cutdata2)
remove(cutdata2)
#-------------------------------------------------------------------------------

#--------------------rounding dummies in smote----------------------------------
smotedata$END_CUSTOMER <- round(smotedata$END_CUSTOMER)
smotedata$LIST_SFT <- round(smotedata$LIST_SFT)
smotedata$LIST_CMT_END <- round(smotedata$LIST_CMT_END)
smotedata$LIST_CMT_INST <- round(smotedata$LIST_CMT_INST)
smotedata$TECH_BP <- round(smotedata$TECH_BP)
smotedata$TECH_E <- round(smotedata$TECH_E)
smotedata$TECH_C <- round(smotedata$TECH_C)
smotedata$TECH_F <- round(smotedata$TECH_F)
smotedata$TECH_FP <- round(smotedata$TECH_FP)
smotedata$TECH_S <- round(smotedata$TECH_S)
smotedata$OFFER_A <- round(smotedata$OFFER_A)
smotedata$OFFER_CI <- round(smotedata$OFFER_CI)
smotedata$OFFER_CP <- round(smotedata$OFFER_CP)
smotedata$OFFER_CPP <- round(smotedata$OFFER_CPP)
smotedata$OFFER_CS <- round(smotedata$OFFER_CS)
smotedata$OFFER_CST <- round(smotedata$OFFER_CST)
smotedata$OFFER_D <- round(smotedata$OFFER_D)
smotedata$OFFER_DCC <- round(smotedata$OFFER_DCC)
smotedata$OFFER_DCF <- round(smotedata$OFFER_DCF)
smotedata$OFFER_ED <- round(smotedata$OFFER_ED)
smotedata$OFFER_EH <- round(smotedata$OFFER_EH)
smotedata$OFFER_EN <- round(smotedata$OFFER_EN)
smotedata$OFFER_EV <- round(smotedata$OFFER_EV)
smotedata$OFFER_FD <- round(smotedata$OFFER_FD)
smotedata$OFFER_FDD <- round(smotedata$OFFER_FDD)
smotedata$OFFER_FDI <- round(smotedata$OFFER_FDI)
smotedata$OFFER_FED <- round(smotedata$OFFER_FED)
smotedata$OFFER_FEI <- round(smotedata$OFFER_FEI)
smotedata$OFFER_FIB <- round(smotedata$OFFER_FIB)
smotedata$OFFER_FIR <- round(smotedata$OFFER_FIR)
smotedata$OFFER_GA <- round(smotedata$OFFER_GA)
smotedata$OFFER_GAM <- round(smotedata$OFFER_GAM)
smotedata$OFFER_MSYS <- round(smotedata$OFFER_MSYS)
smotedata$OFFER_PAT <- round(smotedata$OFFER_PAT)
smotedata$OFFER_STD <- round(smotedata$OFFER_STD)
smotedata$OFFER_SU <- round(smotedata$OFFER_SU)
smotedata$OFFER_SYN <- round(smotedata$OFFER_SYN)
smotedata$OFFER_V <- round(smotedata$OFFER_V)
smotedata$OFFER_XCPS <- round(smotedata$OFFER_XCPS)
smotedata$B_C <- round(smotedata$B_C)
smotedata$B_Exp<- round(smotedata$B_Exp)
smotedata$B_F <- round(smotedata$B_F)
smotedata$B_M <- round(smotedata$B_M)
smotedata$B_Mig <- round(smotedata$B_Mig)
smotedata$B_N <- round(smotedata$B_N)
smotedata$B_New <- round(smotedata$B_New)
smotedata$B_R <- round(smotedata$B_R)
smotedata$B_S <- round(smotedata$B_S)
smotedata$B_T <- round(smotedata$B_T)
smotedata$S_Basel_Central <- round(smotedata$S_Basel_Central)
smotedata$S_Bern_Central <- round(smotedata$S_Bern_Central)
smotedata$S_Bern_EPS <- round(smotedata$S_Bern_EPS)
smotedata$S_Besancon <- round(smotedata$S_Besancon)
smotedata$S_Bezons <- round(smotedata$S_Bezons)
smotedata$S_Bord <- round(smotedata$S_Bord)
smotedata$S_Cler <- round(smotedata$S_Cler)
smotedata$S_Cle <- round(smotedata$S_Cle)
smotedata$S_Dij <- round(smotedata$S_Dij)
smotedata$S_Exp <- round(smotedata$S_Exp)
smotedata$S_Gen <- round(smotedata$S_Gen)
smotedata$S_Gra <- round(smotedata$S_Gra)
smotedata$S_Gre <- round(smotedata$S_Gre)
smotedata$S_Laus1 <- round(smotedata$S_Laus1)
smotedata$S_Laus2 <- round(smotedata$S_Laus2)
smotedata$S_Laus3 <- round(smotedata$S_Laus3)
smotedata$S_Lil <- round(smotedata$S_Lil)
smotedata$S_Lim <- round(smotedata$S_Lim)
smotedata$S_Luz <- round(smotedata$S_Luz)
smotedata$S_Lyo <- round(smotedata$S_Lyo)
smotedata$S_Met <- round(smotedata$S_Met)
smotedata$S_Mona <- round(smotedata$S_Mona)
smotedata$S_Mont <- round(smotedata$S_Mont)
smotedata$S_Nantes <- round(smotedata$S_Nantes)
smotedata$S_Nic <- round(smotedata$S_Nic)
smotedata$S_Others <- round(smotedata$S_Others)
smotedata$S_Poi <- round(smotedata$S_Poi)
smotedata$S_Pau <- round(smotedata$S_Pau)
smotedata$S_Rou <- round(smotedata$S_Rou)
smotedata$S_Sio <- round(smotedata$S_Sio)
smotedata$S_StB <- round(smotedata$S_StB)
smotedata$S_Stei1 <- round(smotedata$S_Stei1)
smotedata$S_Str <- round(smotedata$S_Str)
smotedata$S_Tic <- round(smotedata$S_Tic)
smotedata$S_Tou <- round(smotedata$S_Tou)
smotedata$S_Tours <- round(smotedata$S_Tours)
smotedata$S_VelizyG <- round(smotedata$S_VelizyG)
smotedata$S_VelizyS <- round(smotedata$S_VelizyS)
smotedata$S_Zu1 <- round(smotedata$S_Zu1)
smotedata$S_Zu2 <- round(smotedata$S_Zu2)
smotedata$gov <- round(smotedata$gov)
smotedata$firm <- round(smotedata$firm)
smotedata$person <- round(smotedata$person)
smotedata$IS_A <- round(smotedata$IS_A)
smotedata$IS_B <- round(smotedata$IS_B)
smotedata$IS_C <- round(smotedata$IS_C)
smotedata$IS_D <- round(smotedata$IS_D)
smotedata$IS_E <- round(smotedata$IS_E)
smotedata$IS_F <- round(smotedata$IS_F)
smotedata$IS_G <- round(smotedata$IS_G)
smotedata$IS_H <- round(smotedata$IS_H)
smotedata$IS_I <- round(smotedata$IS_I)
smotedata$IS_J <- round(smotedata$IS_J)
smotedata$IS_K <- round(smotedata$IS_K)
smotedata$IS_L <- round(smotedata$IS_L)
smotedata$IS_M <- round(smotedata$IS_M)
smotedata$IS_N <- round(smotedata$IS_N)
smotedata$IS_O <- round(smotedata$IS_O)
smotedata$IS_P <- round(smotedata$IS_P)
smotedata$IS_Q <- round(smotedata$IS_Q)
smotedata$IS_R <- round(smotedata$IS_R)
smotedata$IS_S <- round(smotedata$IS_S)
smotedata$IS_T <- round(smotedata$IS_T)
smotedata$IS_U <- round(smotedata$IS_U)
#-------------------------------------------------------------------------------

#-----------------data visualization using pca----------------------------------
#1 raw
pcaraw <- training_master
pcaraw$OFFER_STATUS <- NULL
pca_raw_res <- prcomp(pcaraw, scale. = TRUE)
autoplot(pca_raw_res, data = training_master, colour = 'OFFER_STATUS', size = 0.0000000000001) +
  labs(title = 'Original Data', x = '', y = '', color = 'Offer Status') +
  scale_colour_manual(labels = c('Rejected', 'Accepted'), values=c("#FF3300","#0000FF")) + 
  theme(plot.title = element_text(size = 50, hjust = 0.5, colour = 'white'),text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), legend.text = element_blank(), legend.title = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'),
        axis.ticks = element_line(color = "white"), axis.line = element_line(color = "white"))
#2 undersampling
pcacut <- cutdata
pcacut$OFFER_STATUS <- NULL
pcacut <- pcacut[ , which(apply(pcacut, 2, var) != 0)]
pca_cut_res <- prcomp(pcacut, scale. = TRUE)
autoplot(pca_cut_res, data = cutdata, colour = 'OFFER_STATUS', size = 0.0000000000001) +
  labs(title = 'Undersampled Data', x = '', y = '', color = '') +
  scale_colour_manual(labels = c('', ''), values=c("#FF3300","#0000FF")) + 
  scale_colour_manual(labels = c('Rejected', 'Accepted'), values=c("#FF3300","#0000FF")) +
  theme(plot.title = element_text(size = 50, hjust = 0.5, colour = 'white'),text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), legend.text = element_blank(), legend.title = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'),
        axis.ticks = element_line(color = "white"), axis.line = element_line(color = "white"))
#3 oversampling
pcaover <- overdata
pcaover$OFFER_STATUS <- NULL
#pcaover <- pcaover[ , which(apply(pcaover, 2, var) != 0)]
pca_over_res <- prcomp(pcaover, scale. = TRUE)
autoplot(pca_over_res, data = overdata, colour = 'OFFER_STATUS', size = 0.0000000000001) +
  labs(title = 'Oversampled Data', x = '', y = '', color = '') +
  scale_colour_manual(labels = c('', ''), values=c("#FF3300","#0000FF")) + 
  scale_colour_manual(labels = c('Rejected', 'Accepted'), values=c("#FF3300","#0000FF")) +
  theme(plot.title = element_text(size = 50, hjust = 0.5, colour = 'white'),text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), legend.text = element_blank(), legend.title = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'),
        axis.ticks = element_line(color = "white"), axis.line = element_line(color = "white"))
#4 smote
pcasmote <- smotedata
pcasmote$OFFER_STATUS <- NULL
pca_smote_res <- prcomp(pcasmote, scale. = TRUE)
autoplot(pca_smote_res, data = smotedata, colour = 'OFFER_STATUS', size = 0.0000000000001) + 
  labs(title = 'SMOTE Data', x = '', y = '', color = 'Offer Status') + 
  scale_colour_manual(labels = c('Rejected', 'Accepted'), values=c("#FF3300","#0000FF")) + 
  guides(color = guide_legend(override.aes = list(size = 6))) + 
  theme(plot.title = element_text(size = 50, hjust = 0.5, colour = 'white'),text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), legend.text = element_text(size = 30), legend.title = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'black'), legend.key.size = unit(2, 'cm'),
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'),
        axis.ticks = element_line(color = "white"), axis.line = element_line(color = "white")) 

remove(pca_cut_res)
remove(pca_raw_res)
remove(pca_smote_res)
remove(pcacut)
remove(pcaraw)
remove(pcasmote)
remove(pca_over_res)
remove(pcaover)
remove
#-------------------------------------------------------------------------------

########################Training New Random Forests#############################

#-----------------------GRID SEARCH FOR IDEAL PARAMETERS------------------------
#We now conduct a grid search where we optimize for balanced accuracy in artificial test set

#1. raw model
hyper_grid_raw <- expand.grid(
  mtry       = seq(from = 4, to = 16, by = 1),
  sample_size = c(.632, .70, .80),
  balancedaccuracy = 0,
  ntrees = seq(300, 700, by = 10)
)
#build result df
result_raw <- test_master_artificial[,8,]
result_raw$actual <- result_raw$OFFER_STATUS
result_raw$OFFER_STATUS <- NULL
result_raw$actual <- as.numeric(result_raw$actual)
result_raw$actual <- result_raw$actual - 1
#start search
for(i in 1:1599) {
  print(i)
  #build model
  model_raw <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = training_master,
    num.trees       = hyper_grid_raw$ntrees[i],
    mtry            = hyper_grid_raw$mtry[i],
    min.node.size   = 1,
    sample.fraction = hyper_grid_raw$sample_size[i],
    seed            = 2022
  )
  #calculate balanced accuracy
  #generate predictions df:
  predictions_raw <- predict(model_raw, data = test_master_artificial)$predictions
  result_raw$predicted <- predictions_raw
  result_raw$predicted <- as.numeric(result_raw$predicted)
  result_raw$predicted <- result_raw$predicted - 1
  #calculate metrics
  accuracy_raw <- 0
  true_positive_raw <- 0
  false_positive_raw <- 0
  true_negative_raw <- 0
  false_negative_raw <- 0
  total_positives_actual_raw <- sum(result_raw$actual)
  total_negatives_actual_raw <- 4675 - total_positives_actual_raw
  total_positives_predicted_raw <- sum(result_raw$predicted)
  total_negatives_prediced_raw <- 4675 - total_positives_predicted_raw
  for (j in 1:4675) {
    if(result_raw[j, "actual"] == 1) {
      if(result_raw[j, "predicted"] == 1) {
        accuracy_raw <- accuracy_raw + 1
        true_positive_raw <- true_positive_raw + 1
      }
      else {
        false_negative_raw <- false_negative_raw + 1
      }
    }
    else {
      if(result_raw[j, "predicted"] == 0) {
        accuracy_raw <- accuracy_raw + 1
        true_negative_raw <- true_negative_raw + 1
      }
      else {
        false_positive_raw <- false_positive_raw + 1
      }
    }
  }
  #balanced accuracy
  ba <- ((true_positive_raw / (true_positive_raw + false_negative_raw)) + (true_negative_raw / (true_negative_raw + false_positive_raw))) / 2
  hyper_grid_raw$balancedaccuracy[i] <- ba
}


hyper_grid_raw %>% 
  dplyr::arrange(balancedaccuracy) %>%
  head(5)

remove(ba_raw)
remove(model_raw)
#TODOremove rest
#-------------------------------------
#2. cut model
hyper_grid_cut <- expand.grid(
  mtry       = seq(from = 4, to = 16, by = 1),
  sample_size = c(.632, .70, .80),
  balancedaccuracy = 0,
  ntrees = seq(300, 700, by = 10)
)
#build result df
result_cut <- test_master_artificial[,8,]
result_cut$actual <- result_cut$OFFER_STATUS
result_cut$OFFER_STATUS <- NULL
result_cut$actual <- as.numeric(result_cut$actual)
result_cut$actual <- result_cut$actual - 1
#start search
for(i in 1:1599) {
  print(i)
  #build model
  model_cut <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = cutdata,
    num.trees       = hyper_grid_cut$ntrees[i],
    mtry            = hyper_grid_cut$mtry[i],
    min.node.size   = 1,
    sample.fraction = hyper_grid_cut$sample_size[i],
    seed            = 2022
  )
  #calculate balanced accuracy
  #generate predictions df:
  predictions_cut <- predict(model_cut, data = test_master_artificial)$predictions
  result_cut$predicted <- predictions_cut
  result_cut$predicted <- as.numeric(result_cut$predicted)
  result_cut$predicted <- result_cut$predicted - 1
  #calculate metrics
  accuracy_cut <- 0
  true_positive_cut <- 0
  false_positive_cut <- 0
  true_negative_cut <- 0
  false_negative_cut <- 0
  total_positives_actual_cut <- sum(result_cut$actual)
  total_negatives_actual_cut <- 4675 - total_positives_actual_cut
  total_positives_predicted_cut <- sum(result_cut$predicted)
  total_negatives_prediced_cut <- 4675 - total_positives_predicted_cut
  for (j in 1:4675) {
    if(result_cut[j, "actual"] == 1) {
      if(result_cut[j, "predicted"] == 1) {
        accuracy_cut <- accuracy_cut + 1
        true_positive_cut <- true_positive_cut + 1
      }
      else {
        false_negative_cut <- false_negative_cut + 1
      }
    }
    else {
      if(result_cut[j, "predicted"] == 0) {
        accuracy_cut <- accuracy_cut + 1
        true_negative_cut <- true_negative_cut + 1
      }
      else {
        false_positive_cut <- false_positive_cut + 1
      }
    }
  }
  #balanced accuracy
  ba <- ((true_positive_cut / (true_positive_cut + false_negative_cut)) + (true_negative_cut / (true_negative_cut + false_positive_cut))) / 2
  hyper_grid_cut$balancedaccuracy[i] <- ba
}


hyper_grid_cut %>% 
  dplyr::arrange(balancedaccuracy) %>%
  tail(5)

remove(ba_cut)
remove(model_cut)
#TODOremove rest
#------------------------------------
#3 over model
hyper_grid_over <- expand.grid(
  mtry       = seq(from = 4, to = 16, by = 1),
  sample_size = c(.632, .70, .80),
  balancedaccuracy = 0,
  ntrees = seq(300, 700, by = 10)
)
#build result df
result_over <- test_master_artificial[,8,]
result_over$actual <- result_over$OFFER_STATUS
result_over$OFFER_STATUS <- NULL
result_over$actual <- as.numeric(result_over$actual)
result_over$actual <- result_over$actual - 1
#start search
for(i in 1:1599) {
  print(i)
  #build model
  model_over <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = overdata,
    num.trees       = hyper_grid_over$ntrees[i],
    mtry            = hyper_grid_over$mtry[i],
    min.node.size   = 1,
    sample.fraction = hyper_grid_over$sample_size[i],
    seed            = 2022
  )
  #calculate balanced accuracy
  #generate predictions df:
  predictions_over <- predict(model_over, data = test_master_artificial)$predictions
  result_over$predicted <- predictions_over
  result_over$predicted <- as.numeric(result_over$predicted)
  result_over$predicted <- result_over$predicted - 1
  #calculate metrics
  accuracy_over <- 0
  true_positive_over <- 0
  false_positive_over <- 0
  true_negative_over <- 0
  false_negative_over <- 0
  total_positives_actual_over <- sum(result_over$actual)
  total_negatives_actual_over <- 4675 - total_positives_actual_over
  total_positives_predicted_over <- sum(result_over$predicted)
  total_negatives_prediced_over <- 4675 - total_positives_predicted_over
  for (j in 1:4675) {
    if(result_over[j, "actual"] == 1) {
      if(result_over[j, "predicted"] == 1) {
        accuracy_over <- accuracy_over + 1
        true_positive_over <- true_positive_over + 1
      }
      else {
        false_negative_over <- false_negative_over + 1
      }
    }
    else {
      if(result_over[j, "predicted"] == 0) {
        accuracy_over <- accuracy_over + 1
        true_negative_over <- true_negative_over + 1
      }
      else {
        false_positive_over <- false_positive_over + 1
      }
    }
  }
  #balanced accuracy
  ba <- ((true_positive_over / (true_positive_over + false_negative_over)) + (true_negative_over / (true_negative_over + false_positive_over))) / 2
  hyper_grid_over$balancedaccuracy[i] <- ba
}


hyper_grid_over %>% 
  dplyr::arrange(balancedaccuracy) %>%
  head(5)

remove(ba_over)
remove(model_over)
#------------------------------------
#4. smote model
hyper_grid_smote <- expand.grid(
  mtry       = seq(from = 4, to = 16, by = 1),
  sample_size = c(.632, .70, .80),
  balancedaccuracy = 0,
  ntrees = seq(300, 700, by = 10)
)
#build result df
result_smote <- test_master_artificial[,8,]
result_smote$actual <- result_smote$OFFER_STATUS
result_smote$OFFER_STATUS <- NULL
result_smote$actual <- as.numeric(result_smote$actual)
result_smote$actual <- result_smote$actual - 1
#start search
for(i in 1:1599) {
  print(i)
  #build model
  model_smote <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = smotedata,
    num.trees       = hyper_grid_smote$ntrees[i],
    mtry            = hyper_grid_smote$mtry[i],
    min.node.size   = 1,
    sample.fraction = hyper_grid_smote$sample_size[i],
    seed            = 2022
  )
  #calculate balanced accuracy
  #generate predictions df:
  predictions_smote <- predict(model_smote, data = test_master_artificial)$predictions
  result_smote$predicted <- predictions_smote
  result_smote$predicted <- as.numeric(result_smote$predicted)
  result_smote$predicted <- result_smote$predicted - 1
  #calculate metrics
  accuracy_smote <- 0
  true_positive_smote <- 0
  false_positive_smote <- 0
  true_negative_smote <- 0
  false_negative_smote <- 0
  total_positives_actual_smote <- sum(result_smote$actual)
  total_negatives_actual_smote <- 4675 - total_positives_actual_smote
  total_positives_predicted_smote <- sum(result_smote$predicted)
  total_negatives_prediced_smote <- 4675 - total_positives_predicted_smote
  for (j in 1:4675) {
    if(result_smote[j, "actual"] == 1) {
      if(result_smote[j, "predicted"] == 1) {
        accuracy_smote <- accuracy_smote + 1
        true_positive_smote <- true_positive_smote + 1
      }
      else {
        false_negative_smote <- false_negative_smote + 1
      }
    }
    else {
      if(result_smote[j, "predicted"] == 0) {
        accuracy_smote <- accuracy_smote + 1
        true_negative_smote <- true_negative_smote + 1
      }
      else {
        false_positive_smote <- false_positive_smote + 1
      }
    }
  }
  #balanced accuracy
  ba <- ((true_positive_smote / (true_positive_smote + false_negative_smote)) + (true_negative_smote / (true_negative_smote + false_positive_smote))) / 2
  hyper_grid_smote$balancedaccuracy[i] <- ba
}


hyper_grid_smote %>% 
  dplyr::arrange(balancedaccuracy) %>%
  head(5)

remove(ba_smote)
remove(model_smote)
#TODOremove rest
#-------------------------------------------------------------------------------

#----------------Generating optimal model prediction scores---------------------
#1raw
#optimal parameters found by grid search for raw: mtry 15 sampe size 0.8 ntrees 600 with ba = 66.11
#generate model:
ranger_raw <- ranger(
  formula = OFFER_STATUS ~ .,
  data = training_master,
  num.trees = 620,
  mtry = 24,
  min.node.size = 1,
  sample.fraction = 0.8,
  importance = "impurity"
)
#generate predictions df:
predictions_raw <- predict(ranger_raw, data = test_master)$predictions
result_raw <- data.frame(test_master$TEST_SET_ID)
result_raw$predicted <- predictions_raw
remove(predictions_raw)
result_raw$actual <- solution$prediction
result_raw$actual <- as.numeric(result_raw$actual)
result_raw$predicted <- as.numeric(result_raw$predicted)
result_raw$predicted <- result_raw$predicted - 1
#calculate metrics
accuracy_raw <- 0
true_positive_raw <- 0
false_positive_raw <- 0
true_negative_raw <- 0
false_negative_raw <- 0
total_positives_actual_raw <- sum(result_raw$actual)
total_negatives_actual_raw <- 2576 - total_positives_actual_raw
total_positives_predicted_raw <- sum(result_raw$predicted)
total_negatives_prediced_raw <- 2576 - total_positives_predicted_raw
for (i in 1:2576) {
  if(result_raw[i, "actual"] == 1) {
    if(result_raw[i, "predicted"] == 1) {
      accuracy_raw <- accuracy_raw + 1
      true_positive_raw <- true_positive_raw + 1
    }
    else {
      false_negative_raw <- false_negative_raw + 1
    }
  }
  else {
    if(result_raw[i, "predicted"] == 0) {
      accuracy_raw <- accuracy_raw + 1
      true_negative_raw <- true_negative_raw + 1
    }
    else {
      false_positive_raw <- false_positive_raw + 1
    }
  }
}
accuracy_raw <- accuracy_raw / 2576
#recall = true positive rate = sensitivity -> how many of the total positives did we correctly predict
recall_raw <- true_positive_raw / total_positives_actual_raw
#precision <- how many of our positive predictions were actually positive?
precision_raw <- true_positive_raw / total_positives_predicted_raw
#specificity = true negative rate how many people out of total actual negatives also tested negative
specificity_raw <- true_negative_raw / total_negatives_actual_raw
#precision
precision_raw <- true_positive_raw / (true_positive_raw + true_negative_raw)
#balanced accuracy
ba_raw <- ((true_positive_raw / (true_positive_raw + false_negative_raw)) + (true_negative_raw / (true_negative_raw + false_positive_raw))) / 2
#-------------------------------------------------------------------------------
#2cut
#optimal parameters found by grid search for cut: mtry 10 sampe size 0.632 ntrees 470 with ba 74.71%
#generate model:
ranger_cut <- ranger(
  formula = OFFER_STATUS ~ .,
  data = cutdata,
  num.trees = 470,
  mtry = 10,
  min.node.size = 1,
  sample.fraction = 0.632,
  importance = "impurity"
)
#generate predictions df:
predictions_cut <- predict(ranger_cut, data = test_master)$predictions
result_cut <- data.frame(test_master$TEST_SET_ID)
result_cut$predicted <- predictions_cut
remove(predictions_cut)
result_cut$actual <- solution$prediction
result_cut$actual <- as.numeric(result_cut$actual)
result_cut$predicted <- as.numeric(result_cut$predicted)
result_cut$predicted <- result_cut$predicted - 1
#calculate metrics
accuracy_cut <- 0
true_positive_cut <- 0
false_positive_cut <- 0
true_negative_cut <- 0
false_negative_cut <- 0
total_positives_actual_cut <- sum(result_cut$actual)
total_negatives_actual_cut <- 2576 - total_positives_actual_cut
total_positives_predicted_cut <- sum(result_cut$predicted)
total_negatives_prediced_cut <- 2576 - total_positives_predicted_cut
for (i in 1:2576) {
  if(result_cut[i, "actual"] == 1) {
    if(result_cut[i, "predicted"] == 1) {
      accuracy_cut <- accuracy_cut + 1
      true_positive_cut <- true_positive_cut + 1
    }
    else {
      false_negative_cut <- false_negative_cut + 1
    }
  }
  else {
    if(result_cut[i, "predicted"] == 0) {
      accuracy_cut <- accuracy_cut + 1
      true_negative_cut <- true_negative_cut + 1
    }
    else {
      false_positive_cut <- false_positive_cut + 1
    }
  }
}
accuracy_cut <- accuracy_cut / 2576
#recall = true positive rate = sensitivity -> how many of the total positives did we correctly predict
recall_cut <- true_positive_cut / total_positives_actual_cut
#precision <- how many of our positive predictions were actually positive?
precision_cut <- true_positive_cut / total_positives_predicted_cut
#specificity = true negative rate how many people out of total actual negatives also tested negative
specificity_cut <- true_negative_cut / total_negatives_actual_cut
#precision
precision_cut <- true_positive_cut / (true_positive_cut + true_negative_cut)
#balanced accuracy
ba_cut <- ((true_positive_cut / (true_positive_cut + false_negative_cut)) + (true_negative_cut / (true_negative_cut + false_positive_cut))) / 2
#-------------------------------------------------------------------------------
#3 over model
#optimal parameters found by grid search for over: mtry 8 sampe size 0.7 ntrees 670 with ba 73.93%
#generate model:
ranger_over <- ranger(
  formula = OFFER_STATUS ~ .,
  data = overdata,
  num.trees = 670,
  mtry = 8,
  min.node.size = 1,
  sample.fraction = 0.7,
  importance = "impurity"
)
#generate predictions df:
predictions_over <- predict(ranger_over, data = test_master)$predictions
result_over <- data.frame(test_master$TEST_SET_ID)
result_over$predicted <- predictions_over
remove(predictions_over)
result_over$actual <- solution$prediction
result_over$actual <- as.numeric(result_over$actual)
result_over$predicted <- as.numeric(result_over$predicted)
result_over$predicted <- result_over$predicted - 1
#calculate metrics
accuracy_over <- 0
true_positive_over <- 0
false_positive_over <- 0
true_negative_over <- 0
false_negative_over <- 0
total_positives_actual_over <- sum(result_over$actual)
total_negatives_actual_over <- 2576 - total_positives_actual_over
total_positives_predicted_over <- sum(result_over$predicted)
total_negatives_prediced_over <- 2576 - total_positives_predicted_over
for (i in 1:2576) {
  if(result_over[i, "actual"] == 1) {
    if(result_over[i, "predicted"] == 1) {
      accuracy_over <- accuracy_over + 1
      true_positive_over <- true_positive_over + 1
    }
    else {
      false_negative_over <- false_negative_over + 1
    }
  }
  else {
    if(result_over[i, "predicted"] == 0) {
      accuracy_over <- accuracy_over + 1
      true_negative_over <- true_negative_over + 1
    }
    else {
      false_positive_over <- false_positive_over + 1
    }
  }
}
accuracy_over <- accuracy_over / 2576
#recall = true positive rate = sensitivity -> how many of the total positives did we correctly predict
recall_over <- true_positive_over / total_positives_actual_over
#precision <- how many of our positive predictions were actually positive?
precision_over <- true_positive_over / total_positives_predicted_over
#specificity = true negative rate how many people out of total actual negatives also tested negative
specificity_over <- true_negative_over / total_negatives_actual_over
#precision
precision_over <- true_positive_over / (true_positive_over + true_negative_over)
#balanced accuracy
ba_over <- ((true_positive_over / (true_positive_over + false_negative_over)) + (true_negative_over / (true_negative_over + false_positive_over))) / 2
#-------------------------------------------------------------------------------
#4. smote
#optimal parameters found by grid search for smote: mtry 5 sampe size 0.7 ntrees 390 with ba 73.44%
#generate model:
ranger_smote <- ranger(
  formula = OFFER_STATUS ~ .,
  data = smotedata,
  num.trees = 390,
  mtry = 5,
  min.node.size = 1,
  sample.fraction = 0.7,
  importance = "impurity"
)
#generate predictions df:
predictions_smote <- predict(ranger_smote, data = test_master)$predictions
result_smote <- data.frame(test_master$TEST_SET_ID)
result_smote$predicted <- predictions_smote
remove(predictions_smote)
result_smote$actual <- solution$prediction
result_smote$actual <- as.numeric(result_smote$actual)
result_smote$predicted <- as.numeric(result_smote$predicted)
result_smote$predicted <- result_smote$predicted - 1
#calculate metrics
accuracy_smote <- 0
true_positive_smote <- 0
false_positive_smote <- 0
true_negative_smote <- 0
false_negative_smote <- 0
total_positives_actual_smote <- sum(result_smote$actual)
total_negatives_actual_smote <- 2576 - total_positives_actual_smote
total_positives_predicted_smote <- sum(result_smote$predicted)
total_negatives_prediced_smote <- 2576 - total_positives_predicted_smote
for (i in 1:2576) {
  if(result_smote[i, "actual"] == 1) {
    if(result_smote[i, "predicted"] == 1) {
      accuracy_smote <- accuracy_smote + 1
      true_positive_smote <- true_positive_smote + 1
    }
    else {
      false_negative_smote <- false_negative_smote + 1
    }
  }
  else {
    if(result_smote[i, "predicted"] == 0) {
      accuracy_smote <- accuracy_smote + 1
      true_negative_smote <- true_negative_smote + 1
    }
    else {
      false_positive_smote <- false_positive_smote + 1
    }
  }
}
accuracy_smote <- accuracy_smote / 2576
#recall = true positive rate = sensitivity -> how many of the total positives did we correctly predict
recall_smote <- true_positive_smote / total_positives_actual_smote
#precision <- how many of our positive predictions were actually positive?
precision_smote <- true_positive_smote / total_positives_predicted_smote
#specificity = true negative rate how many people out of total actual negatives also tested negative
specificity_smote <- true_negative_smote / total_negatives_actual_smote
#precision
precision_smote <- true_positive_smote / (true_positive_smote + true_negative_smote)
#balanced accuracy
ba_smote <- ((true_positive_smote / (true_positive_smote + false_negative_smote)) + (true_negative_smote / (true_negative_smote + false_positive_smote))) / 2
#-------------------------------------------------------------------------------

#-------------------------draw relevant plots-----------------------------------
#1 bar chart with overall accuracy and balanced accuracy
Model <- c("Original", "Oversampled", "SMOTE", "Undersampled", "Original", "Oversampled", "SMOTE", "Undersampled", "Original", "Oversampled", "SMOTE", "Undersampled")
Scores <- c( specificity_raw, specificity_over, specificity_smote, specificity_cut, recall_raw, recall_over, recall_smote, recall_cut, ba_raw, ba_over, ba_smote, ba_cut)
type <- c('Specificity', 'Specificity', 'Specificity', 'Specificity', 'Sensitivity', 'Sensitivity', 'Sensitivity', 'Sensitivity', 'Balanced accuracy', 'Balanced accuracy', 'Balanced accuracy', 'Balanced accuracy')
barplot_df <- data.frame(Model, Scores, type)
barplot_df$type <- factor(barplot_df$type, levels = c('Specificity', 'Sensitivity', 'Balanced accuracy'))
remove(Model, Scores, type)
ggplot(data = barplot_df, aes(x = Model, y = Scores, fill = type)) + 
  geom_bar(position = "dodge", stat = 'identity') + 
  geom_hline(yintercept = 0.65, color = "white") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c('#66FF33', '#FF3300','#0000FF') ) +
  theme(plot.title = element_blank(), text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white', size = 20), legend.text = element_text(size = 20), legend.title = element_blank(),
        panel.background = element_rect(colour = 'black', fill = 'black'), axis.title = element_blank(), legend.spacing.x = unit(1.0, 'cm'),
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'),
        axis.ticks = element_line(color = "white"), axis.line = element_line(color = "white"), legend.position = "top") 

#2 draw most important variables diagram for undersampling model
ranger_cut$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(5) %>%
  ggplot(aes(reorder(names, x), x)) +
  labs(x = '', y = '', color = '') +
  geom_col(color = "white", fill = c("#0000FF", "#0000FF", "white", "white", "#0000FF"), width = 0.6 )  +
  coord_flip() +
  ggtitle("Variable Importance \n (Top 5 by Gini Impurity)") + 
  scale_x_discrete(labels=c("Margin", "Offer Price", "Service Cost", "Discount", "Offer Creation Duration")) +
  theme(plot.title = element_text(color = "white", size = 30), text = element_text(color = "white"), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white', size = 20), axis.text.x = element_blank(), 
        axis.text.y = element_text(face = c("bold", "plain", "plain", "bold", "bold")),axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'), panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  


#draw histogram of prediction error
OOB_pred_err_raw_ntrees <- vector(mode = "numeric", length = 100)
for(i in 1:100) {
  optimal_ranger_raw <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = training_master, 
    num.trees       = 320,
    mtry            = 18,
    min.node.size   = 1,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  OOB_pred_err_raw_ntrees[i] <- optimal_ranger_raw$prediction.error
}
qplot(OOB_pred_err_raw_ntrees, geom = "histogram") +
  geom_bar(stat = "bin", color = "black", fill = "#0000FF") + 
  labs(x = 'Prediction Error', y = 'Count', color = '') +
  theme(text = element_text(color = "white"), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'), 
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#draw prederror ntree diagram
OOB_pred_err_raw_ntrees <- vector(mode = "numeric", length = 600)
for(i in 1:600) {
  optimal_ranger_raw_numtrees <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = training_master, 
    num.trees       = i,
    mtry            = 18,
    min.node.size   = 1,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  print(i)
  OOB_pred_err_raw_ntrees[i] <- optimal_ranger_raw$prediction.error
}
df_raw <- data.frame(trees_raw = 1:600, errors_raw = OOB_pred_err_raw_ntrees)
ggplot(data = df_raw, aes(x = trees_raw, y = errors_raw, group = 1)) +
  geom_line(color = "#FF3300") +
  theme(text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'),                                                                                                                                           
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'))


#----------------------------------------
#2cut
#optimal parameters found by grid search for cut: mtry 22 sampe size 0.8 ntrees 560

#draw histogram of prediction error
OOB_pred_err_cut_ntrees <- vector(mode = "numeric", length = 100)
for(i in 1:100) {
  optimal_ranger_cut <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = cutdata, 
    num.trees       = 560,
    mtry            = 22,
    min.node.size   = 1,
    sample.fraction = .7,
    importance      = 'impurity'
  )
  OOB_pred_err_cut_ntrees[i] <- optimal_ranger_cut$prediction.error
}
qplot(OOB_pred_err_cut_ntrees, geom = "histogram") +
  geom_bar(stat = "bin", color = "black", fill = "#0000FF") + 
  labs(x = 'Prediction Error', y = 'Count', color = '') +
  theme(text = element_text(color = "white"), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'), 
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#draw most important variables diagram
optimal_ranger_cut$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  labs(x = '', y = '', color = '') +
  geom_col(color = "black", fill = "#0000FF") +
  coord_flip() +
  theme(text = element_text(color = "white"), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'), panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("")

#draw prederror ntree diagram
OOB_pred_err_cut_ntrees <- vector(mode = "numeric", length = 600)
for(i in 1:600) {
  optimal_ranger_cut_numtrees <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = cutdata, 
    num.trees       = i,
    mtry            = 22,
    min.node.size   = 1,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  print(i)
  OOB_pred_err_cut_ntrees[i] <- optimal_ranger_cut$prediction.error
}
df_cut <- data.frame(trees_cut = 1:600, errors_cut = OOB_pred_err_cut_ntrees)
ggplot(data = df_cut, aes(x = trees_cut, y = errors_cut, group = 1)) +
  geom_line(color = "#FF3300") +
  theme(text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'),                                                                                                                                           
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'))
#----------------------------------------
#3.smote
#optimal parameters found by grid search for smote: mtry 20 sampe size 0.8 ntrees 580
#draw histogram of prediction error
OOB_pred_err_smote_ntrees <- vector(mode = "numeric", length = 100)
for(i in 1:100) {
  optimal_ranger_smote <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = smotedata, 
    num.trees       = 580,
    mtry            = 20,
    min.node.size   = 1,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  OOB_pred_err_smote_ntrees[i] <- optimal_ranger_smote$prediction.error
}
qplot(OOB_pred_err_smote_ntrees, geom = "histogram") +
  geom_bar(stat = "bin", color = "black", fill = "#0000FF") + 
  labs(x = 'Prediction Error', y = 'Count', color = '') +
  theme(text = element_text(color = "white"), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'), 
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#draw most important variables diagram
optimal_ranger_smote$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  labs(x = '', y = '', color = '') +
  geom_col(color = "black", fill = "#0000FF") +
  coord_flip() +
  theme(text = element_text(color = "white"), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'), panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("")

#draw prederror ntree diagram
OOB_pred_err_smote_ntrees <- vector(mode = "numeric", length = 600)
for(i in 1:600) {
  optimal_ranger_smote_numtrees <- ranger(
    formula         = OFFER_STATUS ~ ., 
    data            = smotedata, 
    num.trees       = i,
    mtry            = 20,
    min.node.size   = 1,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  print(i)
  OOB_pred_err_smote_ntrees[i] <- optimal_ranger_smote$prediction.error
}
df_smote <- data.frame(trees_smote = 1:600, errors_smote = OOB_pred_err_smote_ntrees)
ggplot(data = df_smote, aes(x = trees_smote, y = errors_smote, group = 1)) +
  geom_line(color = "#FF3300") +
  theme(text = element_text(colour = 'white'), plot.background = element_rect(colour = 'black', fill = 'black'), 
        axis.text = element_text(colour = 'white'), axis.line = element_line(color = 'white'),
        axis.ticks = element_line(color = 'white'),                                                                                                                                           
        panel.background = element_rect(colour = 'black', fill = 'black'), 
        panel.grid.major = element_blank(), legend.key = element_rect(colour = 'black', fill = 'black'),
        panel.grid.minor = element_blank(), legend.background = element_rect(colour = 'black', fill = 'black'))

#----------------------------------------
#-------------------------------------------------------------------------------

#----------------Generate Predictions for actual test set-----------------------
