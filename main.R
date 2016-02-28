#################################################################################################################
#  US CENSUS DATA 1994
#################################################################################################################


# Objective : construction of statistical model to predict a level of people saving, that takes two
# values <= 50000  and  > 50000 . So we construct a classifcation model (binary variable)


#################################################################################################################
#librarys
#install.packages("discretization")
library(Hmisc); library(pastecs); library(psych); library(caret);
library(ggplot2)
library(maps)
library(lsr); library(cramer)
library(caret)
library(discretization); 
library(pROC)
#################################################################################################################


#1 Data import and transformation

USCensusData = read.csv("us_census_full/census_income_learn.csv", header = F)

# import of names of variables
headers = read.table("us_census_full/headers.csv", sep = ",", header = T)
headers$colNames = as.vector(apply(headers, 1, function (x) sub("^.*?\\((.*)\\)[^)]*$", "\\1", x)))
colnames(USCensusData) = c(headers$colNames[1:24], "instance_weight", headers$colNames[25:36], "veteran admin", "veterans benefits",
                        "weeks worked in year", "year", "y"  )
colnames(USCensusData) = gsub(" ", "_", colnames(USCensusData))
colnames(USCensusData) = gsub("[[:punct:]]", "_", colnames(USCensusData))
          

#since we have:
#| The instance weight indicates the number of people in the population
#| that each record represents due to stratified sampling.
#| To do real analysis and derive conclusions, this field must be used.
#| This attribute should *not* be used in the classifiers, so it is
#| set to "ignore" in this file.

#I remove colomn 25
colnames(USCensusData)
USCensusData = USCensusData[, -25]
colnames(USCensusData)

#rearangement of variables by type 
temp  = USCensusData[c(1, 6, 17, 18, 19, 30,39)]
temp2 = USCensusData[, !colnames(USCensusData) %in% colnames(temp)]
temp2[temp2 == " ?"] = NA
temp2 = as.data.frame(apply(temp2, 2, as.factor))
USCensusData = cbind.data.frame(temp, temp2);
USCensusData = USCensusData[, c(41, 1:40)]

# clean some temperory data
rm(temp, temp2, headers)

#################################################################################################################

# Descriptive univariate and multivariate analysis 

USCensusData$y = as.factor(USCensusData$y); is.character(USCensusData$y)
y = (ifelse(USCensusData$y == " - 50000.", 0, 1))

#check
sum(y)/nrow(USCensusData)
dim(USCensusData); names(USCensusData) ; rm(y)



describe(USCensusData[,2:7]);
summary(USCensusData);

#we observe that for numeric variables we have the max = 99999, it's suspicious
#we can think that this values are NA, i verify that how a target distributed accroding to this extreme values
#we see that the indivuduals with high "capital_gains" and "dividends_from_stocks" have a high probability to 
#to have high savings, so this extremes values correspond to some normalisation or "ceil". 

check  = USCensusData[USCensusData$capital_gains == 99999,]; summary(check); rm(check)
check  = USCensusData[USCensusData$dividends_from_stocks == 99999,]; summary(check); rm(check)


#we observe that varibles about migrations are the same frequances
summary(USCensusData);
table(USCensusData$migration_code_move_within_reg, USCensusData$migration_code_change_in_msa)
table(USCensusData$migration_prev_res_in_sunbelt, USCensusData$migration_code_change_in_msa)
factor_class_plot(var = "migration_prev_res_in_sunbelt")
factor_class_plot(var ="migration_code_change_in_msa")
factor_class_plot(var ="migration_code_move_within_reg")

cramersV(USCensusData$y, USCensusData[, "migration_prev_res_in_sunbelt"])
cramersV(USCensusData$y, USCensusData[, "migration_code_move_within_reg"])

#if we use a cramer to evaluate a dependance of target and this migration variables 
#it seems that migration variables do not explain target. 
#And more than that we have a more then 50% of NA values for "migration" variables, imputation is not possible, because we can
# break a data structure. Probably we could take 2 or 3 clases: nonmovers, movers and NA. For the moment 
#i will proceed. 


#I dicided to develop some tools, that will allow me to see things clearly and quikcly
# note: "multiplot" function is not mine, this function is from "Cookbook for R". 
# To not overload the main script i put this functions in separate fail.  
#___________________________________________________________________________________________________________________________

# "numeric_class_plot" gives a distribution plots and summary statistics by class of target


numeric_class_plot("age")
# we observe that it's not possible to be under 16 and have > 50000 savings, so i diceded 
# to make a first rule: all persons under 16  have  <= 50000 savings. In reality this assumption hold true
# children are not capable to make this kind of savings
numeric_class_plot("age" , 16)
#we observe that old and young people make less savings than midle age (35-60).


USCensusData= USCensusData[USCensusData$age>=16,] # we almost remove 25% of our data set

colnames(USCensusData)
numeric_class_plot("wage_per_hour")
numeric_class_plot("wage_per_hour", 0)
#we observe that distribution for target > 50000 is bimodal, but anyway this variable can be important 
#for future analysis


numeric_class_plot("capital_gains")
numeric_class_plot("capital_gains", 0)
numeric_class_plot("capital_gains", 0, 6000)
numeric_class_plot("capital_gains", 6000,  10000)
#we observe that we do not have a clear signal for this variable. Probably we have do discretization of this variable
#get for exemple  <2000 ,  4000:6000 etc... and other levels make "not sure" we could use a "discretisation" package in R 


colnames(USCensusData)
numeric_class_plot("num_persons_worked_for_employer")
#we observe that if 0 => more chances to be under 50000 savings, more than 6 more chances to be > 50000
#in between we "do not know" : i dicede to regroup 0 , 6 and others and make them factors 
USCensusData$num_persons_worked_for_employer = ifelse(USCensusData$num_persons_worked_for_employer ==0 , "0workers",
                     ifelse(USCensusData$num_persons_worked_for_employer ==6, "6workers", "someWorkers") )
USCensusData$num_persons_worked_for_employer = as.factor(USCensusData$num_persons_worked_for_employer )

factor_class_plot("num_persons_worked_for_employer") ; cramersV(USCensusData$y, USCensusData[, "num_persons_worked_for_employer"])
#relatively high cramer, so we create an important variable


colnames(USCensusData)
numeric_class_plot("weeks_worked_in_year")
#i used a packa discretize to have some indications about cuts
cutPoints(USCensusData$weeks_worked_in_year, USCensusData$y)
USCensusData$weeks_worked_in_year = ifelse(USCensusData$weeks_worked_in_year < 26 , "week25",
       ifelse(USCensusData$weeks_worked_in_year > 25  & 
                USCensusData$weeks_worked_in_year < 46, "weeks2545", "weeks50") )
USCensusData$weeks_worked_in_year = as.factor(USCensusData$weeks_worked_in_year )

factor_class_plot("weeks_worked_in_year"); cramersV(USCensusData$y, USCensusData[, "weeks_worked_in_year"])
#relatively high cramer, so we create an important variable


###################################################
# for factor variables i made "factor_class_plot", which i've used already
# This work is the most time expensive. We have some time limits so, i decided to 
# not go in furter details

factor_class_plot(var = "weeks_worked_in_year") ; cramersV(USCensusData$y, USCensusData[, "education"])


factor_class_plot(var = "education") ; cramersV(USCensusData$y, USCensusData[, "education"])
factor_class_plot(var = "sex") ; cramersV(USCensusData$y, USCensusData[, "sex"])
factor_class_plot(var = "race") ; cramersV(USCensusData$y, USCensusData[, "race"])
factor_class_plot(var = "marital_stat") ; cramersV(USCensusData$y, USCensusData[, "marital_stat"])
factor_class_plot(var = "own_business_or_self_employed") ; cramersV(USCensusData$y, USCensusData[, "own_business_or_self_employed"])
factor_class_plot(var = "citizenship") ; cramersV(USCensusData$y, USCensusData[, "citizenship"])


# This work is the most time expensive. We have some time limits so, i decided to 
# not go in furters details. More than that we can come and back to parametrize our inputs
# in the momemnt of model construction 

colnames(USCensusData)

#Finaly i decided to make a map. To see how the saving distributed in US. I took a variable "state_of_previous_residence"
#___________________________________________________________________________________________________________________________
# tx is a number of > 50000 saving divided by total number. 
#We see that Lousiana, Nebraska, Tennessee number of people with > 50000 is low 
# on the other hand Columbia and New Jersey have more people with high savings

USCensusData$n = (ifelse(USCensusData$y == " - 50000.", 0, 1))
data = data.frame(region=as.character(tolower((USCensusData$state_of_previous_residence))), 
                  income=USCensusData$n, 1, 
                  stringsAsFactors=F)

data = data[data$region != " not in universe" & data$region != " ?", ]
data$region = substring(data$region ,2, 1000)

aggregate = cbind.data.frame(aggregate(income ~ region, data = data, sum ), aggregate(X1 ~ region, data = data, sum )[,2])
aggregate$rate_of_50000_Savings = aggregate[,2]/aggregate[,3]
states_map <- map_data("state")
ggplot(aggregate, aes(map_id = region)) + 
  geom_map(aes(fill = rate_of_50000_Savings), map = states_map) +
  scale_fill_gradientn(colours=c("blue","green")) + 
  expand_limits(x = states_map$long, y = states_map$lat)+
  ggtitle("Rate of americans with savings > 50000")+
  theme(plot.title=element_text(family="Times", face="bold", size=20))
  
#___________________________________________________________________________________________________________________________
maps = function(var){
  
  data = data.frame(region=as.character(tolower((USCensusData$state_of_previous_residence))), 
                 variable=(USCensusData[, var]), 
                 stringsAsFactors=F)
  data = data[data$region != " not in universe" & data$region != " ?", ]
  data$region = substring(data$region ,2, 1000)
  
  states_map <- map_data("state")
  print(ggplot(data, aes(map_id = region)) + 
    geom_map(aes(fill = variable), map = states_map) +
    scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
    expand_limits(x = states_map$long, y = states_map$lat))
  
}
colnames(USCensusData)
maps("age")
#___________________________________________________________________________________________________________________________


# Classification models 

#Logistic regresion

train_log = USCensusData
train_log$y1 = ifelse(train_log$y == " - 50000.", 0, 1)
train_log$y1  = as.factor(train_log$y1 )
logit = glm(y1 ~age +
              wage_per_hour+
              capital_gains+
              capital_losses+
              dividends_from_stocks+
              num_persons_worked_for_employer+
              weeks_worked_in_year+
              class_of_worker+
              education+
              marital_stat+
              sex+
              full_or_part_time_employment_stat+
              tax_filer_stat+
              detailed_household_summary_in_household , data = train_log, family = binomial(link = "logit"))

#backwards = step(logit); backwards
summary(logit)
test_log = testSet
test_log$y1 = ifelse(test_log$y == " - 50000.", 0, 1)
test_log$y1 = as.factor(test_log$y1)

test_log = test_log[test_log$age>15,]

fitted.results <- predict(logit, test_log, type = "response")
fitted.results <- ifelse(fitted.results >= 0.5,1,0)
table(fitted.results, test_log$y1)
summary(as.factor(test_log$y1))

fitted.results <- predict(logit, test_log, type = "response")
fitted.results  = cbind.data.frame(1-fitted.results, fitted.results)
colnames(fitted.results) = c("negative", "positive")

ROC.log <- roc(predictor = fitted.results$negative,
             response = test_log$y1,
             levels = rev(levels(test_log$y1)))
plot(ROC.log)
#######################################################""

USCensusData$n = ifelse(USCensusData$y == " - 50000.", "negative", "positive")
testSet$n = ifelse(testSet$y == " - 50000.", "negative", "positive")
testSet$n = as.factor(testSet$n)


testSet$num_persons_worked_for_employer = ifelse(testSet$num_persons_worked_for_employer ==0 , "0workers",
                                                 ifelse(testSet$num_persons_worked_for_employer ==6, "6workers", "someWorkers") )
testSet$num_persons_worked_for_employer = as.factor(testSet$num_persons_worked_for_employer )

testSet$weeks_worked_in_year = ifelse(testSet$weeks_worked_in_year < 26 , "week25",
                                      ifelse(testSet$weeks_worked_in_year > 25  & 
                                               testSet$weeks_worked_in_year < 46, "weeks2545", "weeks50") )
testSet$weeks_worked_in_year = as.factor(testSet$weeks_worked_in_year )


##################################################################################"
#C5.0 boosted trees

grid <- expand.grid(.model = "tree",
                   .trials = c(1:100),
                   .winnow = FALSE)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           classProbs = T,
                           summaryFunction = twoClassSummary)
c5 <- train(n ~age +
                  wage_per_hour+
                  capital_gains+
                  capital_losses+
                  dividends_from_stocks+
                  num_persons_worked_for_employer+
                  weeks_worked_in_year+
                  class_of_worker+
                  education+
                  marital_stat+
                  major_occupation_code+
                  sex+
                  full_or_part_time_employment_stat+
                  tax_filer_stat+
                  detailed_household_summary_in_household,
                data = USCensusData,
                method = "C5.0",
                metric = "ROC",
                tuneGrid = grid,
                trControl = fitControl)

plot(c5)



testSet_without_children = testSet[testSet$age>15,]

c5proba <- predict(c5, testSet, type = "prob")
ROC.C5 <- roc(predictor = c5proba$positive,
                               response = testSet$n,
                               levels = rev(levels(testSet$n)))

c5proba <- predict(c5, testSet_without_children, type = "prob")
ROC.C5_without_children <- roc(predictor = c5proba$positive,
              response = testSet_without_children$n,
              levels = rev(levels(testSet_without_children$n)))

plot(ROC.C5, col = "darkred")
plot(ROC.C5_without_children, add = T, col = "darkred", lwd = 2)
plot(ROC.log, add = T)

varimpC5 = varImp(c5, scale = F)
plot(varimpC5, 30)

##################################################################################"
# GBM

fitControl <- trainControl(method = "cv",
                          number = 3,
                          classProbs = T,
                          summaryFunction = twoClassSummary)

gbmGrid <-  expand.grid(interaction.depth = c(13, 11),
                        n.trees = c(1000, 1100),
                        shrinkage = c(0.1),
                        n.minobsinnode = 100)


gbmGrid <-  expand.grid(interaction.depth = c(5, 11),
                        n.trees = 1000,
                        shrinkage = c(0.1, 0.3),
                        n.minobsinnode = 100)

colnames(train)
ggbmFit5 <- train(n ~age +
                    wage_per_hour+
                    capital_gains+
                    capital_losses+
                    dividends_from_stocks+
                    num_persons_worked_for_employer+
                    weeks_worked_in_year+
                    class_of_worker+
                    education+
                    marital_stat+
                    major_occupation_code+
                    sex+
                    full_or_part_time_employment_stat+
                    tax_filer_stat+
                    detailed_household_summary_in_household,
                        data = USCensusData,
                        method = "gbm",
                        trControl = fitControl,
                        tuneGrid = gbmGrid,
                        verbose = TRUE)
gbmFit5
plot(gbmFit5)





confusionMatrix(predict(gbmFit5, testSet), testSet$n)

final = cbind.data.frame(testSet, rpartProbs)
final$decision = ifelse(final$positive>=0.10, "positive", "negative")


table(final$decision, final$n)
final$decision = ifelse(final$age<16,  "negative", final$decision )
table(final$decision, final$n)



summary(final$n)

rpartROC<-roc(testSet$n, rpartProbs[,"YES"], levels=rev(testSet$n))
plot(rpartROC,type="S",print.thres=0.5)


confusionMatrix(predict(gbmFit5, testSet), testSet$n)

rpartProbs<-predict(gbmFit5, testSet,type="prob")

testtt = testSet
testtt = testtt[testtt$age > 15, ]
dbm <- predict(gbmFit5, testtt, type = "prob")
dbm <- roc(predictor = dbm$positive,
             response = testtt$n,
             levels = rev(levels(testtt$n)))

rpartProbs<-predict(gbmFit5, testSet,type="prob")


auc(c5ROC)

c5ROC
plot(c5ROC, add = T,  col = "black")
plot(dbm, add = T,  col = "green")


c = varImp(gbmFit6, scale = F)
plot(c)

