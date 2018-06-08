# This script 
# 1) runs gradient boosted regression trees (GBRT) using XGBOOST. 
# 2) contains some base code to run GBRT with the GBM functionality
# 3) produces exploration plots of environmental timeseries
# 4) contains in-depth description and sources for XGBOOST

# NOTE: it mostly relies on functions_for_mfe_xgboost.R. if you want to
# run the GBM then it will use brt.functions.R


#METHODOLOGY
# Script describes steps of classification, and poisson using TB as an example
# with other areas the pre-defined functions have the same content so you can just skip to that part. 
#start right away with the mrfe function and then inspect the model 

# code to do boosted regression trees on the data produced in Join_Env_Vars_with_FIM.R
trace(utils:::unpackPkgZip, edit=TRUE)
# Set Working Directory ####
#must change working directory for data when working on personal vs work computer
rm(list=ls())

# Set Location
IS_HOME = FALSE

if (IS_HOME == TRUE) {
  #personal_comp = "~/Desktop/PhD project/Projects/Seatrout/FWRI SCRATCH FOLDER/Elizabeth Herdter/SAS data sets/FIMData/NEWNov7"
  #phys_dat = "~/Desktop/PhD project/Projects/Seatrout/Data/Raw Survey Data/Seatrout FIM Data"
  #enviro_data = "~/Desktop/PhD project/Projects/Seatrout/Data/EnvironmentalData"
  data =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes/Seatrout_ENV_Chapter2"
  source_location= "/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2"
  out =   "~/Desktop/PhD project/Projects/Seatrout/Data/Exported R Dataframes"
  setwd(data)
  source("~/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2/brt.functions.R")
} else {
  #work_comp = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
  #phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
  #enviro_data = "U:/PhD_projectfiles/Raw_Data/Environmental_Data"
  data =   "U:/PhD_projectfiles/Exported_R_Datafiles/Seatrout_ENV_Chapter2"
  out =   "U:/PhD_projectfiles/Exported_R_Datafiles"
  source_location = "U:/R_For_School/Seatrout_ENV_Chapter2"
  source("U:/R_For_School/Seatrout_ENV_Chapter2/brt.functions.R")
  source("U:/R_For_School/Seatrout_ENV_Chapter2/functions_for_mfe_xgboost.R")
  setwd(data)
}
#trace(utils:::unpackPkgZip, edit=TRUE)
#install.packages('xgboost')
library(gbm)
library(tidyverse)
library(xgboost)
library(caret)
#library(dismo)
#install.packages('caret', dependencies = TRUE)

# number -34
# 
# salinity  (FIM-at moment) - 43
# temperature (FIM - at moment) - 54
# river_flow (mean monthly flow rate) - 70
# Nit_val (month) - 71
# Phos_val (month) - 72
# Sal_val  (month) - 73
# WatTemp_val  (month) - 74
# Z_val    (month) -NO
# Z_anom   (month)
# MaxT_val  (month) -NO
# MaxT_anom  (month)
# MinT_val   (month) -NO
# MinT_anom   (month)
# MeanMonthlyRF  (month)
# ext_coef
# spring_dis
# summer_dis
# winter_dis
# prev_autumn_dis
# spring_Z_val  - NO
# spring_Z_anom
# spring_MaxT_val	-NO
# spring_MaxT_anom	
# spring_MinT_val	-NO
# spring_MinT_anom	
# summer_Z_val	-NO
# summer_Z_anom	
# summer_MaxT_val	-NO
# summer_MaxT_anom	
# summer_MinT_val	-NO
# summer_MinT_anom	
# winter_Z_val	-NO
# winter_Z_anom	
# winter_MaxT_val	-NO
# winter_MaxT_anom	
# winter_MinT_val	-NO
# winter_MinT_anom	
# prev_autumn_Z_val	-NO
# prev_autumn_Z_anom	
# prev_autumn_MaxT_val	-NO
# prev_autumn_MaxT_anom	
# prev_autumn_MinT_val	-NO
# prev_autumn_MinT_anom	
# spring_RF	
# summer_RF	
# winter_RF	
# prev_autumn_RF	
# spring_dis_ALL	
# summer_dis_ALL	
# winter_dis_ALL	
# prev_autumn_dis_ALL	
# first_spawn_salinity	
# first_spawn_waterT	
# second_spawn_salinity	
# second_spawn_waterT	
# third_spawn_salinity	
# third_spawn_waterT
# atspawn_nitro
# avg_last2_nitro
# avg_last3_nitro - 129


# TB ####
TB4 <- filter_TB(4)
TB6 <- filter_TB (6)
TB9 <- filter_TB(9)

# explore possible outliers
# hist(TB$Nit_val); plot(Nit_val~year, data=TB)
# 
# hist(TB$atspawn_nitro); plot(atspawn_nitro~year, data=TB)
# 
# hist(TB$avg_last2_nitro); plot(avg_last2_nitro~year, data=TB)
# 
# hist(TB$avg_last3_nitro) ;plot(avg_last3_nitro~year, data=TB)
# 
# hist(TB$riv_flow); plot(riv_flow ~ year, data=TB)
# 
# hist(TB$spring_dis) ; plot(spring_dis ~ year, data=TB)
# 
# hist(TB$summer_dis) ; plot(summer_dis ~ year, data=TB)
# 
# hist(TB$winter_dis); plot(winter_dis ~ year, data=TB)
# 
# hist(TB$prev_autumn_dis); plot(prev_autumn_dis ~ year, data=TB)
# 
# hist(TB$TotalMonthlyRF); plot(TotalMonthlyRF~year, data=TB)
# 
# hist(TB$spring_RF); plot(spring_RF~year, data=TB)
# 
# hist(TB$summer_RF); plot(summer_RF~year, data=TB)
# 
# hist(TB$winter_RF); plot(winter_RF~year, data=TB)
# 
# hist(TB$prev_autumn_RF); plot(prev_autumn_RF~year, data=TB)
# 
# hist(TB$WatTemp_val); hist(TB$atspawn_waterT); hist(TB$Sal_val)
# 
# hist(TB$number)
# max(TB$number)
# 
# plot(number ~year, data=TB)
# Plot predictor and response data ####
#PREDICTORS
# use gather to get it into format expected for dplyr summarize 
#salinity, temperature, Nit-val, MeanMonthlyRF, river_flow
selec_enviro <- TB4 %>% dplyr::select(year, month, salinity, temperature, Nit_val, Clor_val, TotalMonthlyRF, riv_flow) %>% 
  transmute(year=year, month=month, sal_scal = scale(salinity), temp_scal = scale(temperature), N_scal = scale(Nit_val), Clor_scal=scale(Clor_val), RF_scal=scale(TotalMonthlyRF), river_scal = scale(riv_flow)) %>% 
  gather(sal_scal, temp_scal, N_scal, Clor_scal, RF_scal, river_scal, key="variable", value="observed")

sum_env <- selec_enviro %>% group_by(variable, year, month) %>% summarise(N=sum(!is.na(observed)), mean=mean(observed, na.rm=TRUE), sd=sd(observed, na.rm=TRUE), se = sd/sqrt(N))

ggplot(sum_env, aes(year, mean)) + geom_line()+geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  facet_grid(month~variable)+
  scale_y_continuous(breaks=seq(0,120,20), labels=seq(0,120,20))+
  scale_x_continuous(breaks=seq(1988, 2014,4), labels=seq(1988, 2014,4))+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  xlab("Year")+
  ylab("Zero-centered mean Value")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 									
        panel.background=element_rect(fill='white', colour='black'),
        axis.title.y = element_text(colour="black", size=10), # changing font of y axis title
        axis.title.x = element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black", size=10, angle=45), #changing  colour and font of x axis text
        axis.text.y=element_text(colour="black", size=10),
        strip.text.y = element_text(size=10))  #changing colour and font of y axis

#Z_anomaly, maxT, minT
cd_ENV <- TB4 %>% dplyr::select("year", "month", "Z_anom", "MaxT_anom", "MinT_anom") %>%
  gather(Z_anom, MaxT_anom, MinT_anom, key="variable", value="observed")

cd_sum <- cd_ENV %>% group_by(variable, year, month) %>% summarise(N=sum(!is.na(observed)), mean=mean(observed, na.rm=TRUE), sd=sd(observed, na.rm=TRUE), se = sd/sqrt(N))

ggplot(cd_sum, aes(year, mean)) + geom_line()+geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  facet_grid(month~variable)+
  #scale_y_continuous(breaks=seq(0,120,20), labels=seq(0,120,20))+
  scale_x_continuous(breaks=seq(1988, 2014,4), labels=seq(1988, 2014,4))+
  #geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  xlab("Year")+
  ylab("Zero-centered mean Value")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 									
        panel.background=element_rect(fill='white', colour='black'),
        axis.title.y = element_text(colour="black", size=10), # changing font of y axis title
        axis.title.x = element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black", size=10, angle=45), #changing  colour and font of x axis text
        axis.text.y=element_text(colour="black", size=10),
        strip.text.y = element_text(size=10))  #changing colour and font of y axis


#response
num <- TB4 %>% dplyr::select("year", "month", "number") %>% group_by(year, month) %>% summarise(N=length(number), sum=sum(number), mean=mean(number), num_per_haul = sum/N, sd= sd(number, na.rm=TRUE), se=sd/sqrt(N))

ggplot(num, aes(year, num_per_haul)) + geom_line()+
  geom_ribbon(aes(ymin=num_per_haul-se, ymax=num_per_haul+se), alpha=0.1) + facet_grid(month~.)


# ....######
# XGBOOST WITH TB as Example ####
# ......#####
#  CLASSIFICATION####
# only 0's & 1's ####
#..........######
#https://www.kaggle.com/rtatman/machine-learning-with-xgboost-in-r

#Process and create train and test ####
TB_class <- TB
TB_class$number[TB_class$number >=1] <- 1
table(TB_class$number)

#try to remove month =5
#TB_class <- TB_class[TB_class$month>5,]

#remove variables that only depend on the animal having been present
TB_class <- TB_class %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

#table the number of NAs by variable to see if I should keep them in
Num_NA<-sapply(TB_class,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(TB_class),Count=Num_NA)

#make different datasets based on haul timing and to minimize the number of NAs present
#early spring/ < month=6
#late spring/early summer >=6
#late summer >=9


#shuffle
set.seed(12345) #set the seed so that it always samples the same
TB_clsslct <- TB_class[sample(nrow(TB_class),0.86*(nrow(TB_class))),] #dplyr::selected classification

#split dataset into testing and training subset

#training data - this statement is necessary for trian() below
df_train <- TB_clsslct %>% mutate(number = factor(number, labels=c("Absent", "Present")))


label_name="number"
train_features <- TB_clsslct %>% dplyr::select(-c(number))
train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
train_label <- TB_clsslct$number

#testing data 
TB_test <- anti_join(TB_class, TB_clsslct)
test_data <- TB_test %>% dplyr::select(-c(number))
test_data <- as.matrix(mutate_all(test_data, function(x) as.numeric(as.character(x))))
test_label <- TB_test %>% dplyr::select(number)

# put our testing & training data into two seperates matrix objects
train_features <- as.matrix(train_features)
train_label <- as.matrix(train_label)

test_data <- as.matrix(test_data)
test_label <- as.matrix(test_label)

#convert cleaned dataframe to a dmatrix - xgboost naturally accepts sparse features format, feathres that are not presented in the sparse feature matrix are treated as missing

xgbMatrixTrain <- xgb.DMatrix(data = train_features, label = train_label, missing = "NA")
xgbMatrixTest <- xgb.DMatrix(data = test_data, label = test_label, missing = "NA")

#fit the model with the arbitrary parameters using either xgboost or xgb.train
#Dont necessarily have to do this. Better to first determine nrounds with xgb.cv and then hyperparameter tuning and THEN train the model
#from example on web: how to tune
# xgb_1 = xgboost(data = as.matrix(df_train %>%
#                                    dplyr::select(-SeriousDlqin2yrs)),
#                 label = df_train$SeriousDlqin2yrs, params = xgb_params_1,
#                 nrounds = 100,                                                 # max number of trees to build
#                 verbose = TRUE,                                         
#                 print.every.n = 1,
#                 early.stop.round = 10                                          # stop if no improvement within 10 trees
# )

#Build basic xgboost model ####
#Basic xgboost model- will be used in parameter tuning step
#xgboost parameters starting values 

eta = 0.001

xgb_params <- list(colsample_bytree = 0.7 , #similar to max features of GBM, denotes the fraction of columns to be randomly sampled for each tree, typical is 0.5 to 1 
                   subsample= 0.7  , #same as the subsample of gbm, like bag fraction of GBM, denotes the fraction of observations to be randomly sampled for ecah tree, lower values make the algorithm more convservative and prevents overfitting 
                   booster= "gbtree",  #whether to do trees or regression
                   max_depth=5, #max depth of a tree, should be tuned using CV, typical values 3-10
                   eta = eta, #analogous to learning rate in gbm
                   #eval_metric = "auc", # default according to the objective, default is rmse for regression and error for classification
                   objective = "binary:logistic") # default, defines the loss function to be minimized)

#determine best number of trees to use in the tuning steps
negative_cases <- sum(train_label == FALSE)
positive_cases <- sum(train_label == TRUE)


xgb_cv <- xgb.cv(xgb_params, data=train_features, label=train_label, #xgbMatrixTrain,
                 early_stopping_rounds = 20, #monitors the performece of the model and stops the procedure once the performance has not improved after a fixed number of training iterations
                 nfold = 10, #k-fold CV,  
                 nrounds=2000,
                 scale_pos_weight = negative_cases/positive_cases) # number of trees

#where is minimum rmse 
min <- which.min(xgb_cv$evaluation_log[ ,test_error_mean])
max <- which.max(xgb_cv$evaluation_log[ ,test_error_mean])
#says 1 but chosing 3 because thats when train error is lowest too

#same thing but different way
ntree <- xgb_cv$best_ntreelimit

#plot
xgb_cv$evaluation_log %>% dplyr::select(1,2,4) %>% 
  gather(type,value, -iter) %>% 
  ggplot(aes(iter,value)) + geom_line(aes(colour = type))

#minimum iterations for training and test rmse mean will happen at different places but thats because the xgb_cv is doing k fold so inherently there is a training set that is then tested on the test set. the results from the test set say whats best



#Hyperparameter tuning ####
#search using train() in caret package
#use caret package to do hyperparameter grid search

#General method for tuning
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#1. Chose a relative high learning rate (0.05 to 0.3). Determine the optimum number of trees for this learning rate/
#2. Tune tree-specific parameters (maxdepth, min child weight, gamma, subsample, colsample by tree) for decided learning rate and number of trees
#3. Tune regularization parameters which can help reduce model complexity and enhance performance
#4. Lower the learning rate and decide optimal parameters

# Other really good resources:Top one gives general layout, bottom one has set up exactly what it needs to be
#https://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
#https://github.com/topepo/caret/issues/507
#https://topepo.github.io/caret/subsampling-for-class-imbalances.html

#1. Tune max depth and min child weight ####
#if nrounds (trees) is acceptible then use here to tune tree using nrounds determined above
#set up the cross validated hyper parameter search. These are all the values I want to search over. 
# eta = c(0.01,0.05,0.1),max_depth = c(2,4,6,8,10,14), min_child_weight =c(6,8,10,12), gamma=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), colsample_bytree =c(0.4, 0.6, 0.7,0.8,1.0),subsample =c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0))

cv.ctrl <- trainControl(method = "cv", number = 10, 
                        summaryFunction = twoClassSummary, #for classification
                        allowParallel=T, classProbs=T, verboseIter=T, sampling = "up")
# two class summary and classProbs are for classification problems 



xgb.grid <- expand.grid(nrounds=ntree,
                        eta = c(eta),
                        max_depth = c(6,8,10,12,14),
                        gamma = c(0.001),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(6,8,10,12),
                        subsample = c(0.8))

xgb_tune1 <- train(x=as.matrix(df_train %>% dplyr::select(-number)), 
                 y=as.factor(df_train$number), #this set up is different for regression
                 method = 'xgbTree',
                 objective = 'binary:logistic',
                 metric = "error",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 scale_pos_weight = negative_cases/positive_cases)

ggplot(xgb_tune1$results, aes(x = as.factor(max_depth), y = min_child_weight, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

max_depth <- xgb_tune1$results[xgb_tune1$results$ROC == max(xgb_tune1$results$ROC),]$max_depth
min_child_weight <- xgb_tune1$results[xgb_tune1$results$ROC == max(xgb_tune1$results$ROC),]$min_child_weight

#2. Tune gamma ####
#min child weight =6 nand max depth =8 is best option
# retune with those

xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(0.001, 0.1, 0.2, 0.3, 0.4),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.8 ))

xgb_tune2 <-train(x=as.matrix(df_train %>% dplyr::select(-number)), 
                  y=as.factor(df_train$number), #this set up is different for regression
                  method = 'xgbTree',
                  objective = 'binary:logistic',
                  metric = "error",
                  trControl=cv.ctrl,
                  tuneGrid=xgb.grid,
                  scale_pos_weight = negative_cases/positive_cases)


ggplot(xgb_tune2$results, aes(x = as.factor(gamma), y = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

gamma<- xgb_tune2$results[xgb_tune2$results$ROC == max(xgb_tune2$results$ROC),]$gamma


#3. Tune subsample and colsample by tree ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(gamma),
                        colsample_bytree = c(0.6,0.7, 0.8, 0.9),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.6,0.7, 0.8, 0.9))

xgb_tune3 <-train(x=as.matrix(df_train %>% dplyr::select(-number)), 
                  y=as.factor(df_train$number), #this set up is different for regression
                  method = 'xgbTree',
                  objective = 'binary:logistic',
                  metric = "error",
                  trControl=cv.ctrl,
                  tuneGrid=xgb.grid,
                  scale_pos_weight = negative_cases/positive_cases)

ggplot(xgb_tune3$results, aes(x = as.factor(colsample_bytree), y = subsample, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

colsample_bytree<- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$colsample_bytree
subsample <- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$subsample

#best colsample by tree =0.6, subsample =0.7 (0.6 or 0.9, also)

#4. Train model with found parameters ####
#train the model using these best parameters found above

model_tuned <- xgboost(data = xgbMatrixTrain, # the data           
                       max.depth = max_depth, # the maximum depth of each decision tree
                       nround = ntree, # max number of boosting iterations
                       objective = "binary:logistic",
                       eta=eta,
                       gamma=gamma,
                       colsample_bytree = colsample_bytree,
                       subsample= subsample) # the objective function 


#View feature importance/influence from the learnt model
importance_matrix <- xgb.importance(names(df_train %>%  dplyr::select(-number)), model=model_tuned)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
#xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()


# Predict ####
pred <- predict(model_tuned, xgbMatrixTest)

#evaluate model performance
#from caret package

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_label)
print(paste("test-error=", err))

modelLookup("xgbTree")


# Dimensionality reduction with PCA ####
#https://shiring.github.io/machine_learning/2016/12/02/flu_outcome_ML_2_post

# for each fold:
#   split data
# conduct PCA on the 90% used for training
# pick the number of components
# fit linear regression
# predict the 10% held out
# end:
# from: https://stats.stackexchange.com/questions/46216/pca-and-k-fold-cross-validation-in-caret-package-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa


xgb_pca <-train(x=as.matrix(df_train %>% dplyr::select(-number)), 
                y=as.factor(df_train$number), #this set up is different for regression
                method = 'xgbTree',
                preProcess='pca',
                objective = 'binary:logistic',
                metric = "error",
                trControl=cv.ctrl,
                tuneGrid=xgb.grid,
                scale_pos_weight = negative_cases/positive_cases)

confusionMatrix(predict(xgb_pca, df_train %>% dplyr::select(-number)), df_train$number)


# .....######
# COUNT:POISSAN ####
# 0's & all present #### 
### .....######

#Process and create train and test ####
TB_rgrs <- TB

#TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
#must remove these if I include 0s in the data set because these three variables are dependent on 
# there having been at least 1 observation
TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))

table(TB_rgrs$number)

#table the number of NAs by variable to see if I should keep them in
Num_NA<-sapply(TB_rgrs,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(TB_rgrs),Count=Num_NA)

#shuffle
set.seed(12345) #set the seed so that it always samples the same
TB_rgrs <- TB_rgrs[sample(nrow(TB_rgrs)),]

#split dataset into testing and training subset
numberOfTrainingSamples <- round(nrow(TB_rgrs) * .7)
TB_rgrsslct<- TB_rgrs[1:numberOfTrainingSamples,]

#TB_rgrsslct <- TB_rgrs[sample(nrow(TB_rgrs),0.80*(nrow(TB_rgrs))),]

#training data - this statement is necessary for trian() below
df_train <- TB_rgrsslct 

label_name="number"
train_features <- TB_rgrsslct %>% dplyr::select(-c(number))
train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
train_label <- TB_rgrsslct$number

#testing data 
TB_test <- TB_rgrs[-(1:numberOfTrainingSamples),]
test_data <- TB_test %>% dplyr::select(-c(number))
test_data <- as.matrix(mutate_all(test_data, function(x) as.numeric(as.character(x))))
test_label <- TB_test %>% dplyr::select(number)

# put our testing & training data into two seperates matrix objects
train_features <- as.matrix(train_features)
train_label <- as.matrix(train_label)

test_data <- as.matrix(test_data)
test_label <- as.matrix(test_label)

#convert cleaned dataframe to a dmatrix - xgboost naturally accepts sparse features format, feathres that are not presented in the sparse feature matrix are treated as missing

xgbMatrixTrain <- xgb.DMatrix(data = train_features, label = train_label, missing = "NA")
xgbMatrixTest <- xgb.DMatrix(data = test_data, label = test_label, missing = "NA")

#fit the model with the arbitrary parameters using either xgboost or xgb.train
#Dont necessarily have to do this. Better to first determine nrounds with xgb.cv and then hyperparameter tuning and THEN train the model
#from example on web: how to tune
# xgb_1 = xgboost(data = as.matrix(df_train %>%
#                                    dplyr::select(-SeriousDlqin2yrs)),
#                 label = df_train$SeriousDlqin2yrs, params = xgb_params_1,
#                 nrounds = 100,                                                 # max number of trees to build
#                 verbose = TRUE,                                         
#                 print.every.n = 1,
#                 early.stop.round = 10                                          # stop if no improvement within 10 trees
# )



#Build basic xgboost model ####
#Basic xgboost model- will be used in parameter tuning step
#xgboost parameters starting values 
eta=0.01
xgb_params <- list(colsample_bytree = 0.7 , #similar to max features of GBM, denotes the fraction of columns to be randomly sampled for each tree, typical is 0.5 to 1 
                   subsample= 0.7  , #same as the subsample of gbm, like bag fraction of GBM, denotes the fraction of observations to be randomly sampled for ecah tree, lower values make the algorithm more convservative and prevents overfitting 
                   booster= "gbtree",  #whether to do trees or regression
                   max_depth=5, #max depth of a tree, should be tuned using CV, typical values 3-10
                   eta = eta, #analogous to learning rate in gbm
                   eval_metric = "poisson-nloglik", # default according to the objective, default is rmse for regression and error for classification
                   objective = "count:poisson") # default, defines the loss function to be minimized)


#determine best number of trees to use in the tuning steps                   
xgb_cv <- xgb.cv(xgb_params, xgbMatrixTrain,
                 early_stopping_rounds = 10, #monitors the performece of the model and stops the procedure once the performance has not improved after a fixed number of training iterations
                 nfold = 10, #k-fold CV,  
                 nrounds=2000) # number of trees

#where is minimum rmse 
minpoisson_nloglik_mean <- which.min(xgb_cv$evaluation_log[ ,test_poisson_nloglik_mean])
maxpoisson_nloglik_mean <- which.max(xgb_cv$evaluation_log[ ,test_poisson_nloglik_mean])

#same thing but different way
ntree <- xgb_cv$best_ntreelimit

#plot
xgb_cv$evaluation_log %>% dplyr::select(1,2,4) %>% 
  gather(type,value, -iter) %>% 
  ggplot(aes(iter,value)) + geom_line(aes(colour = type))

#minimum iterations for training and test rmse mean will happen at different places but thats because the xgb_cv is doing k fold so inherently there is a training set that is then tested on the test set. the results from the test set say whats best

#Hyperparameter tuning ####
#search using train() in caret package
#use caret package to do hyperparameter grid search

#General method for tuning
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#1. Chose a relative high learning rate (0.05 to 0.3). Determine the optimum number of trees for this learning rate/
#2. Tune tree-specific parameters (maxdepth, min child weight, gamma, subsample, colsample by tree) for decided learning rate and number of trees
#3. Tune regularization parameters which can help reduce model complexity and enhance performance
#4. Lower the learning rate and decide optimal parameters

# Other really good resources:Top one gives general layout, bottom one has set up exactly what it needs to be
#https://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
#https://github.com/topepo/caret/issues/507


#1. Tune max depth and min child weight ####
#if nrounds (trees) is acceptible then use here to tune tree using nrounds determined above
#set up the cross validated hyper parameter search. These are all the values I want to search over. 
# eta = c(0.01,0.05,0.1),max_depth = c(2,4,6,8,10,14), min_child_weight =c(6,8,10,12), gamma=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), colsample_bytree =c(0.4, 0.6, 0.7,0.8,1.0),subsample =c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0))


#pack the training control parameters
cv.ctrl <- trainControl(method = "cv", number = 10, 
                        #summaryFunction = defaultSummary, #for regression (this does mean squared error and R-squared)
                        allowParallel=T, verboseIter=T)

xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(2,4,6,8,10,12,14),
                        gamma = c(0.001),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(6,8,10,12),
                        subsample = c(0.8 ))


#train the model for each parameter combination in the grid, using CV to evaluate 
label_var = "number"

xgb_tune_cp1 <-train(x=xgbMatrixTrain,
                 #y= train_label, #if you use this format it thinks you are doing classification because you specify label, idkw
                 y=TB_rgrsslct %>% pull(label_var),
                 method = 'xgbTree',
                 #objective = "gamma-deviance",
                 objective = 'count:poisson',
                 eval_metric = 'poisson-nloglik',
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid)

# scatter plot of the rmse against max_depth and eta - from example 
ggplot(xgb_tune_cp1$results, aes(x = as.factor(min_child_weight), y = max_depth, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

max_depth <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$max_depth
min_child_weight <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$min_child_weight


#2. Tune gamma ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(0.001, 0.1, 0.2, 0.3, 0.4),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.8 ))

xgb_tune_cp2 <-train(x=xgbMatrixTrain, 
                 y=TB_rgrsslct %>% pull(label_var),
                 method = 'xgbTree',
                 objective = 'count:poisson',
                 eval_metric = "poisson-nloglik",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid)

ggplot(xgb_tune_cp2$results, aes(x = as.factor(gamma), y = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

gamma<- xgb_tune_cp2$results[xgb_tune_cp2$results$RMSE == max(xgb_tune_cp2$results$RMSE),]$gamma

#3. Tune subsample and colsample by tree ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(gamma),
                        colsample_bytree = c(0.6,0.7, 0.8, 0.9),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.6,0.7, 0.8, 0.9))

xgb_tune_cp3 <-train(x=xgbMatrixTrain, 
                 y=TB_rgrsslct %>% pull(label_var),
                 method = 'xgbTree',
                 objective = 'count:poisson',
                 eval_metric = "poisson-nloglik",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid)

ggplot(xgb_tune_cp3$results, aes(x = as.factor(colsample_bytree), y = subsample, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

colsample_bytree<- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$colsample_bytree
subsample <- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$subsample


#this will determine best parameters...then use those parameters with prediction

#4. Train model with found parameters ####
#train the model using these best parameters found above

model_tuned_cp <- xgboost(data = xgbMatrixTrain, # the data           
                       max.depth = max_depth, # the maximum depth of each decision tree
                       min_child_weight =min_child_weight,
                       nround = ntree, # max number of boosting iterations
                       objective = "count:poisson",
                       eval_metric = "poisson-nloglik",
                       eta=eta,
                       #gamma=0.01,
                       #colsample_bytree = 0.6,
                       #subsample = 0.6)
                       gamma=gamma,
                       colsample_bytree =colsample_bytree,
                       subsample=subsample) # the objective function 


#View feature importance/influence from the learnt model
importance_matrix_cp <- xgb.importance(names(df_train %>%  dplyr::select(-number)), model=model_tuned_cp)
print(importance_matrix_cp)
print(importance_matrix_cp$Feature)
xgb.plot.importance(importance_matrix = importance_matrix_cp)
#xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()

# Predict ####
pred_cp <- predict(model_tuned_cp, xgbMatrixTest)

#evaluate model performance
#from caret package
RMSE(pred_cp, test_label) #from caret package
R2(pred_cp, test_label)
sum(-log(dpois(test_label, lambda=pred_cp)))


#deviance explained

#calculate null deviance of Poisson distribution for the intercept model
#https://stats.stackexchange.com/questions/140044/null-deviance-in-glm-r
new_label <- test_label +0.001
lf=sum(new_label * log(new_label) - new_label-log(factorial(new_label)))
lnull = sum(new_label * log(mean(new_label)) - mean(new_label) - log(factorial(new_label)))
null_dev = 2*(lf-lnull)

#null deviance for NO intercept
lnull = sum(-1 - log(factorial(new_label))) 
lf = sum(new_label * log(new_label) - new_label - log(factorial(new_label))) 
null_dev = 2*(lf-lnull)

#calculate residual deviance
library(dismo)
#important that calc.mean=FALSE if not it will give you mean residual deviance which isnt what standard glms give you. 
res_dev = calc.deviance(as.matrix(new_label), as.matrix(pred_cp), family="poisson", calc.mean=FALSE)

#deviance explained
dev_exp <- (null_dev - res_dev) / null_dev

# .... ####
# POISSON ####
# NO 0's ####
# ....####
TB_rgrs <- TB

TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
#must remove these if I include 0s in the data set because these three variables are dependent on 
# there having been at least 1 observation
#TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro ))
table(TB_rgrs$number)

#table the number of NAs by variable to see if I should keep them in
Num_NA<-sapply(TB_rgrs,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(TB_rgrs),Count=Num_NA)

#shuffle
set.seed(12345) #set the seed so that it always samples the same
TB_rgrs <- TB_rgrs[sample(nrow(TB_rgrs)),]

#split dataset into testing and training subset
numberOfTrainingSamples <- round(nrow(TB_rgrs) * .7)
TB_rgrsslct<- TB_rgrs[1:numberOfTrainingSamples,]

#TB_rgrsslct <- TB_rgrs[sample(nrow(TB_rgrs),0.80*(nrow(TB_rgrs))),]

#training data - this statement is necessary for trian() below
df_train <- TB_rgrsslct 

label_name="number"
train_features <- TB_rgrsslct %>% dplyr::select(-c(number))
train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
train_label <- TB_rgrsslct$number

#testing data 
TB_test <- TB_rgrs[-(1:numberOfTrainingSamples),]
test_data <- TB_test %>% dplyr::select(-c(number))
test_data <- as.matrix(mutate_all(test_data, function(x) as.numeric(as.character(x))))
test_label <- TB_test %>% dplyr::select(number)

# put our testing & training data into two seperates matrix objects
train_features <- as.matrix(train_features)
train_label <- as.matrix(train_label)

test_data <- as.matrix(test_data)
test_label <- as.matrix(test_label)

#convert cleaned dataframe to a dmatrix - xgboost naturally accepts sparse features format, feathres that are not presented in the sparse feature matrix are treated as missing

xgbMatrixTrain <- xgb.DMatrix(data = train_features, label = train_label, missing = "NA")
xgbMatrixTest <- xgb.DMatrix(data = test_data, label = test_label, missing = "NA")

#fit the model with the arbitrary parameters using either xgboost or xgb.train
#Dont necessarily have to do this. Better to first determine nrounds with xgb.cv and then hyperparameter tuning and THEN train the model
#from example on web: how to tune
# xgb_1 = xgboost(data = as.matrix(df_train %>%
#                                    dplyr::select(-SeriousDlqin2yrs)),
#                 label = df_train$SeriousDlqin2yrs, params = xgb_params_1,
#                 nrounds = 100,                                                 # max number of trees to build
#                 verbose = TRUE,                                         
#                 print.every.n = 1,
#                 early.stop.round = 10                                          # stop if no improvement within 10 trees
# )



#Build basic xgboost model ####
#Basic xgboost model- will be used in parameter tuning step
#xgboost parameters starting values 
eta=0.01
xgb_params <- list(colsample_bytree = 0.7 , #similar to max features of GBM, denotes the fraction of columns to be randomly sampled for each tree, typical is 0.5 to 1 
                   subsample= 0.7  , #same as the subsample of gbm, like bag fraction of GBM, denotes the fraction of observations to be randomly sampled for ecah tree, lower values make the algorithm more convservative and prevents overfitting 
                   booster= "gbtree",  #whether to do trees or regression
                   max_depth=5, #max depth of a tree, should be tuned using CV, typical values 3-10
                   eta = eta, #analogous to learning rate in gbm
                   eval_metric = "poisson-nloglik", # default according to the objective, default is rmse for regression and error for classification
                   objective = "count:poisson") # default, defines the loss function to be minimized)


#determine best number of trees to use in the tuning steps                   
xgb_cv <- xgb.cv(xgb_params, xgbMatrixTrain,
                 early_stopping_rounds = 10, #monitors the performece of the model and stops the procedure once the performance has not improved after a fixed number of training iterations
                 nfold = 10, #k-fold CV,  
                 nrounds=2000) # number of trees

#where is minimum rmse 
minpoisson_nloglik_mean <- which.min(xgb_cv$evaluation_log[ ,test_poisson_nloglik_mean])
maxpoisson_nloglik_mean <- which.max(xgb_cv$evaluation_log[ ,test_poisson_nloglik_mean])

#same thing but different way
ntree <- xgb_cv$best_ntreelimit

#plot
xgb_cv$evaluation_log %>% dplyr::select(1,2,4) %>% 
  gather(type,value, -iter) %>% 
  ggplot(aes(iter,value)) + geom_line(aes(colour = type))

#minimum iterations for training and test rmse mean will happen at different places but thats because the xgb_cv is doing k fold so inherently there is a training set that is then tested on the test set. the results from the test set say whats best

#Hyperparameter tuning ####
#search using train() in caret package
#use caret package to do hyperparameter grid search

#General method for tuning
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#1. Chose a relative high learning rate (0.05 to 0.3). Determine the optimum number of trees for this learning rate/
#2. Tune tree-specific parameters (maxdepth, min child weight, gamma, subsample, colsample by tree) for decided learning rate and number of trees
#3. Tune regularization parameters which can help reduce model complexity and enhance performance
#4. Lower the learning rate and decide optimal parameters

# Other really good resources:Top one gives general layout, bottom one has set up exactly what it needs to be
#https://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
#https://github.com/topepo/caret/issues/507


#1. Tune max depth and min child weight ####
#if nrounds (trees) is acceptible then use here to tune tree using nrounds determined above
#set up the cross validated hyper parameter search. These are all the values I want to search over. 
# eta = c(0.01,0.05,0.1),max_depth = c(2,4,6,8,10,14), min_child_weight =c(6,8,10,12), gamma=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), colsample_bytree =c(0.4, 0.6, 0.7,0.8,1.0),subsample =c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0))


#pack the training control parameters
cv.ctrl <- trainControl(method = "cv", number = 10, 
                        #summaryFunction = defaultSummary, #for regression (this does mean squared error and R-squared)
                        allowParallel=T, verboseIter=T)

xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(2,4,6,8,10,12,14),
                        gamma = c(0.001),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(6,8,10,12),
                        subsample = c(0.8 ))


#train the model for each parameter combination in the grid, using CV to evaluate 
label_var = "number"

xgb_tune_cp1 <-train(x=xgbMatrixTrain,
                     #y= train_label, #if you use this format it thinks you are doing classification because you specify label, idkw
                     y=TB_rgrsslct %>% pull(label_var),
                     method = 'xgbTree',
                     #objective = "gamma-deviance",
                     objective = 'count:poisson',
                     eval_metric = 'poisson-nloglik',
                     trControl=cv.ctrl,
                     tuneGrid=xgb.grid)

# scatter plot of the rmse against max_depth and eta - from example 
ggplot(xgb_tune_cp1$results, aes(x = as.factor(min_child_weight), y = max_depth, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

max_depth <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$max_depth
min_child_weight <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$min_child_weight


#2. Tune gamma ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(0.001, 0.1, 0.2, 0.3, 0.4),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.8 ))

xgb_tune_cp2 <-train(x=xgbMatrixTrain, 
                     y=TB_rgrsslct %>% pull(label_var),
                     method = 'xgbTree',
                     objective = 'count:poisson',
                     eval_metric = "poisson-nloglik",
                     trControl=cv.ctrl,
                     tuneGrid=xgb.grid)

ggplot(xgb_tune_cp2$results, aes(x = as.factor(gamma), y = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

gamma<- xgb_tune_cp2$results[xgb_tune_cp2$results$RMSE == max(xgb_tune_cp2$results$RMSE),]$gamma


#RMSE is lowest when gamma = 0.1 

#3. Tune subsample and colsample by tree ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(gamma),
                        colsample_bytree = c(0.6,0.7, 0.8, 0.9),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.6,0.7, 0.8, 0.9))

xgb_tune_cp3 <-train(x=xgbMatrixTrain, 
                     y=TB_rgrsslct %>% pull(label_var),
                     method = 'xgbTree',
                     objective = 'count:poisson',
                     eval_metric = "poisson-nloglik",
                     trControl=cv.ctrl,
                     tuneGrid=xgb.grid)

ggplot(xgb_tune_cp3$results, aes(x = as.factor(colsample_bytree), y = subsample, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

colsample_bytree<- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$colsample_bytree
subsample <- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$subsample


#this will determine best parameters...then use those parameters with prediction

#4. Train model with found parameters ####
#train the model using these best parameters found above

model_tuned_cp <- xgboost(data = xgbMatrixTrain, # the data           
                          max.depth = max_depth, # the maximum depth of each decision tree
                          min_child_weight =min_child_weight,
                          nround = ntree, # max number of boosting iterations
                          objective = "count:poisson",
                          eval_metric = "poisson-nloglik",
                          eta=eta,
                          gamma=gamma,
                          colsample_bytree =colsample_bytree,
                          subsample=subsample) # the objective function 


#View feature importance/influence from the learnt model
importance_matrix_cp <- xgb.importance(names(df_train %>%  dplyr::select(-number)), model=model_tuned_cp)
print(importance_matrix_cp)
print(importance_matrix_cp$Feature)
xgb.plot.importance(importance_matrix = importance_matrix_cp)
#xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()


# Predict ####
pred_cp <- predict(model_tuned_cp, xgbMatrixTest)

#deviance explained
#calculate null deviance of Poisson distribution for the intercept model
#https://stats.stackexchange.com/questions/140044/null-deviance-in-glm-r
new_label <- test_label +0.001
lf=sum(new_label * log(new_label) - new_label-log(factorial(new_label)))
lnull = sum(new_label * log(mean(new_label)) - mean(new_label) - log(factorial(new_label)))
null_dev = 2*(lf-lnull)

#null deviance for NO intercept
lnull = sum(-1 - log(factorial(new_label))) 
lf = sum(new_label * log(new_label) - new_label - log(factorial(new_label))) 
null_dev = 2*(lf-lnull)

#calculate residual deviance
#important that calc.mean=FALSE if not it will give you mean residual deviance which isnt what standard glms give you. 
res_dev = calc.deviance(as.matrix(new_label), as.matrix(pred_cp), family="poisson", calc.mean=FALSE)


#evaluate model performance
#from caret package
RMSE(pred_cp, test_label) #from caret package
R2(pred_cp, test_label)
sum(-log(dpois(test_label, lambda=pred_cp)))


#deviance explained

#calculate null deviance of Poisson distribution for the intercept model
#https://stats.stackexchange.com/questions/140044/null-deviance-in-glm-r
test_label <- test_label +0.001

lf=sum(test_label * log(test_label) - test_label-log(factorial(test_label)))
lnull = sum(test_label * log(mean(test_label)) - mean(test_label) - log(factorial(test_label)))

#null deviance
null_dev = 2*(lf-lnull)

#calculate residual deviance
#library(dismo)
#important that calc.mean=FALSE if not it will give you mean residual deviance which isnt what standard glms give you. 
res_dev = calc.deviance(as.matrix(test_label), as.matrix(pred_cp), family="poisson", calc.mean=FALSE)

#deviance explained
dev_exp <- (null_dev - res_dev) / null_dev

#...####
# RUN ENTIRE ROUTINE MANUAL RECURSIVE FEATURE ELIMINATION ####
# ... ####


#poisson 
entire_routine_poisson(TB4,"TB4", dopospos=FALSE, complete.cases=FALSE)
#entire_routine_poisson(TB6,"TB6", dopospos=FALSE, complete.cases=FALSE)
#entire_routine_poisson(TB9,"TB9", dopospos=FALSE, complete.cases=FALSE)

entire_routine_poisson(TB4,"TB4", dopospos=TRUE, complete.cases=FALSE)
#entire_routine_poisson(TB6,"TB6", dopospos=TRUE, complete.cases=FALSE)
#entire_routine_poisson(TB9,"TB9", dopospos=TRUE, complete.cases=FALSE)

entire_routine_class(TB4, "TB4")
#entire_routine_class(TB6, "TB6")
#entire_routine_class(TB9, "TB9")

#produce data set
AP4 <- filter_AP(4)
AP6 <- filter_AP(6)
AP9 <- filter_AP(9)

CK4 <- filter_CK(4) %>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
CK6 <- filter_CK(6) %>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
CK9 <- filter_CK(9) %>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)

CH4 <- filter_CH(4)
CH6 <- filter_CH(6)
CH9 <- filter_CH(9)

IR4 <- filter_IR(4)
IR6 <- filter_IR(6)
IR9 <- filter_IR(9)

JX4 <- filter_JX(4)%>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
JX6 <- filter_JX(6)%>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
JX9 <- filter_JX(9)%>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)

#poisson
entire_routine_poisson(AP4,"AP4", dopospos=FALSE, complete.cases=FALSE)
entire_routine_poisson(CK4,"CK4", dopospos=FALSE, complete.cases=FALSE)
entire_routine_poisson(CH4,"CH4", dopospos=FALSE, complete.cases=FALSE)
entire_routine_poisson(IR4,"IR4", dopospos=FALSE, complete.cases=FALSE)
entire_routine_poisson(JX4,"JX4", dopospos=FALSE, complete.cases=FALSE)

#positive poisson
#entire_routine_poisson(AP4,"AP4", dopospos=TRUE, complete.cases=FALSE)
#entire_routine_poisson(CK4,"CK4", dopospos=TRUE, complete.cases=FALSE)
#entire_routine_poisson(CH4,"CH4", dopospos=TRUE, complete.cases=FALSE)
#entire_routine_poisson(IR4,"IR4", dopospos=TRUE, complete.cases=FALSE)
#entire_routine_poisson(JX4,"JX4", dopospos=TRUE, complete.cases=FALSE)


#binomial
entire_routine_class(AP4, "AP4")
#entire_routine_class(AP6, "AP6")
#entire_routine_class(AP9, "AP9")




#.....#########################
# COMPLETE CASES XGBOOST ####
#.....#########################

#POISSON
TB4 <- filter_TB(4)
TB4sht<- TB4 %>% dplyr::select(-aten_ceof)

AP4 <- filter_AP(4)
AP4sht<- AP4 %>% dplyr::select(-c(ext_ceof, Nit_val))

CK4 <- filter_CK(4) %>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
CK4sht<- CK4 %>% dplyr::select(-c(ext_ceof, winter_dis_ALL, prev_autumn_dis_ALL))

CH4 <- filter_CH(4)
CH4sht<- CH4 %>% dplyr::select(-c(ext_ceof, Nit_val))


JX4 <- filter_JX(4)%>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)
JX4sht<- JX4 %>% dplyr::select(-ext_ceof)


IR4 <- filter_IR(4)
IR4sht<- IR4 %>% dplyr::select(-c(Nit_val, ext_ceof, atspawn_nitro, avg_last2_nitro, avg_last3_nitro))
#not enough of the last variables for either pospos or poisson (either with Complete cases)
IR4sht <- IR4sht %>% mutate(atspawn_nitro = NA, avg_last2_nitro=NA, avg_last3_nitro=NA, atspawn_waterT=NA, atspawn_salinity=NA)

Num_NA<-sapply(IR4sht,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(IR4sht),Count=Num_NA)


#complete cases positive poisson
entire_routine_poisson(TB4sht, "TB4sht", dopospos=TRUE, complete.cases=TRUE, is.ckjxir=FALSE)
entire_routine_poisson(AP4sht, "AP4sht", dopospos=TRUE, complete.cases=TRUE, is.ckjxir=FALSE)

#test <- manualrfe_poisson(CK4sht, dopospos=TRUE, complete.cases=TRUE, is.ckjxir=TRUE)
#entire_routine_poisson(CK4sht, "CK4sht",  dopospos=TRUE, complete.cases=TRUE, is.ckjxir=TRUE)

entire_routine_poisson(CH4sht, "CH4sht",  dopospos=TRUE, complete.cases=TRUE, is.ckjxir=FALSE)
entire_routine_poisson(JX4sht, "JX4sht", dopospos=TRUE, complete.cases=TRUE, is.ckjxir=TRUE)
entire_routine_poisson(IR4sht, "IR4sht", dopospos=TRUE, complete.cases=TRUE, is.ckjxir=TRUE)

# HERE complete cases poisson
entire_routine_poisson(AP4sht, "AP4sht", dopospos=FALSE, complete.cases=TRUE, is.ckjxir=FALSE)
entire_routine_poisson(TB4sht, "TB4sht", dopospos=FALSE, complete.cases=TRUE, is.ckjxir=FALSE)
entire_routine_poisson(CK4sht, "CK4sht",  dopospos=FALSE, complete.cases=TRUE, is.ckjxir=TRUE)
entire_routine_poisson(CH4sht, "CH4sht",  dopospos=FALSE, complete.cases=TRUE, is.ckjxir=FALSE)
entire_routine_poisson(JX4sht, "JX4sht", dopospos=FALSE, complete.cases=TRUE, is.ckjxir=TRUE)
entire_routine_poisson(IR4sht, "IR4sht", dopospos=FALSE, complete.cases=TRUE, is.ckjxir=TRUE)



#BINOMIAL
TB4sht<- TB4 %>% dplyr::select(-aten_ceof) %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
TB4sht <- TB4sht[complete.cases(TB4sht),]

AP4sht<- AP4 %>% dplyr::select(-c(ext_ceof, Nit_val)) %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
AP4sht <- AP4sht[complete.cases(AP4sht),]

CK4sht<- CK4 %>% dplyr::select(-c(ext_ceof, winter_dis_ALL, prev_autumn_dis_ALL)) %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
CK4sht <- CK4sht[complete.cases(CK4sht),]

CH4sht<- CH4 %>% dplyr::select(-c(ext_ceof, Nit_val)) %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
CH4sht <- CH4sht[complete.cases(CH4sht),]

JX4sht<- JX4 %>% dplyr::select(-ext_ceof)%>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
JX4sht <- JX4sht[complete.cases(JX4sht),]

IR4sht<- IR4 %>% dplyr::select(-c(Nit_val, ext_ceof, atspawn_nitro, avg_last2_nitro, avg_last3_nitro))%>% dplyr::select(-c( atspawn_waterT, atspawn_salinity ))
IR4sht <- IR4sht[complete.cases(IR4sht),]


entire_routine_class(TB4sht, "TB4sht", complete.cases=TRUE)
entire_routine_class(AP4sht, "AP4sht", complete.cases=TRUE)
entire_routine_class(CK4sht, "CK4sht", complete.cases=TRUE)
entire_routine_class(CH4sht, "CH4sht", complete.cases=TRUE)
entire_routine_class(JX4sht, "JX4sht", complete.cases=TRUE)
entire_routine_class(IR4sht, "IR4sht", complete.cases=TRUE)


# .... ####
# Examine Correlations ####
# .... ####

library(corrplot)
df_train_sm <- TB %>% dplyr::select(-number)
df_train_min <- as.matrix(df_train_sm)
M <- cor(df_train_min, use="complete.obs")

corrplot(M,  method="square")


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(df_train_min)


corrplot(M, type="upper", 
         p.mat = p.mat, sig.level = 0.01, tl.cex=0.5, insig="blank")

#Lots of correlation happening between the the riverflow varaibles
#remove some _dis variables 

string <- c("spring_dis", "winter_dis", "summer_dis", "prev_autumn_dis")

df_train_sm <- as.matrix(df_train_sm %>% dplyr::dplyr::select(-one_of(string)))

M <- cor(df_train_sm, use="complete.obs")
p.mat <- cor.mtest(df_train_sm)

corrplot(M, type="upper", order="hclust", method="color",
         p.mat = p.mat, sig.level = 0.01, tl.cex=0.5, insig="blank")




# ...####
#JUST HABITAT VARS ####
# .... ####
TB <- read.csv("TB_all_env_with_lag.csv", header=TRUE, row.names=1)
string <- c("Z_val", "MaxT_val", "MinT_val")
TB <- TB[,-grep(paste(string, collapse="|"), colnames(TB))]

TB_rgrs <- TB
#TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro ))
#TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
#TB_rgrs <- TB_rgrs[,1:10]
table(TB_rgrs$number)
TB_rgrs <- TB_rgrs[TB_rgrs$month>4,]
TB_rgrs$number[TB_rgrs$number> 100] <- 100

# ONE HOT ENCODING JUST HABITAT VARS 
TB_rgrs <-  TB_rgrs %>% 
  mutate(bottom = ifelse(TB_rgrs$bStr ==1, "structure", ifelse(TB_rgrs$bSan>0 | TB_rgrs$bMud>0, "mudsand", "unknown")), 
         veg= ifelse(TB_rgrs$bveg == "SAVAlg", "SAV", ifelse(TB_rgrs$bveg == "Alg", "SAV", ifelse(TB_rgrs$bveg =="SAV", "SAV", "Noveg"))),
         shore = ifelse(substr(TB_rgrs$Shore,1,3)=="Eme", "Emerge", ifelse(substr(TB_rgrs$Shore,1,3) =="Man", "Mangrove", 
                                                                      ifelse(substr(TB_rgrs$Shore,1,3)=="Str", "Structure", ifelse(substr(TB_rgrs$Shore, 1,3)=="Ter", "Terrestrial", "Non")))))    %>%
  dplyr::select(-c(bStr, bSan, bMud, bveg, Shore)) %>% subset(!shore=="Non") %>% 
  mutate(avgDepth = mean(c(StartDepth, Enddepth)))


bottom <- model.matrix(~bottom-1,TB_rgrs)
veg <- model.matrix(~veg-1,TB_rgrs)
shore <- model.matrix(~shore-1,TB_rgrs)

habitat <- cbind(bottom, veg, shore)
data <- data.frame(cbind(TB_rgrs$number, habitat))
colnames(data)[1] <- "number"

#shuffle
set.seed(12345) #set the seed so that it always samples the same
data <- data[sample(nrow(data)),]

#split dataset into testing and training subset
numberOfTrainingSamples <- round(nrow(data) * .7)
datasslct<- data[1:numberOfTrainingSamples,]

#TB_rgrsslct <- TB_rgrs[sample(nrow(TB_rgrs),0.80*(nrow(TB_rgrs))),]

#training data - this statement is necessary for trian() below
df_train <- datasslct 

label_name="number"
train_features <- datasslct %>% dplyr::select(-c(number))
train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
train_label <- datasslct$number

#testing data 
TB_test <- data[-(1:numberOfTrainingSamples),]
test_data <- TB_test %>% dplyr::select(-c(number))
test_data <- as.matrix(mutate_all(test_data, function(x) as.numeric(as.character(x))))
test_label <- TB_test %>% dplyr::select(number)

# put our testing & training data into two seperates matrix objects
train_features <- as.matrix(train_features)
train_label <- as.matrix(train_label)

test_data <- as.matrix(test_data)
test_label <- as.matrix(test_label)

#convert cleaned dataframe to a dmatrix - xgboost naturally accepts sparse features format, feathres that are not presented in the sparse feature matrix are treated as missing

xgbMatrixTrain <- xgb.DMatrix(data = train_features, label = train_label, missing = "NA")
xgbMatrixTest <- xgb.DMatrix(data = test_data, label = test_label, missing = "NA")


#basic train
model_tuned_cp <- xgboost(data = xgbMatrixTrain, # the data           
                          max.depth = 6, # the maximum depth of each decision tree
                          nround = 2000, # max number of boosting iterations
                          objective = "count:poisson",
                          eval_metric = "poisson-nloglik",
                          eta=0.01,
                          gamma=0.1,
                          colsample_bytree =0.7,
                          subsample=0.8) # the objective function 


#View feature importance/influence from the learnt model
importance_matrix_cp <- xgb.importance(names(df_train %>%  dplyr::select(-number)), model=model_tuned_cp)
print(importance_matrix_cp)
print(importance_matrix_cp$Feature)
xgb.plot.importance(importance_matrix = importance_matrix_cp)
#xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()


# Predict ####
pred_cp <- predict(model_tuned_cp, xgbMatrixTest)

#evaluate model performance
#from caret package
RMSE(pred_cp, test_label) #from caret package
R2(pred_cp, test_label)


















# ..          #########
# .            . #########
# .      . ###########


#BRT ####
# Build runBrt function #### 
# that runs BRT at different levels of tc and lr 

# for all combinations of tree complexity (tc) and learning rate (lr) run on a smaller training set and then predict to 
# an independent test set. for each run capture the deviance of training and CV as well as the predictive deviance of the independent set
# With the best option do variable dplyr::selection to trim down to get more realistic number of variables. then build plots and explore variable contributions

# tc (will range from 5,7,10)
# lr (0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001)

runBRT= function(mat, dat) {
  #initialize an empty dataframe
  results= data.frame(matrix(data=NA, nrow=12, ncol=13))
  colnames(results) <- c("MeanDeviance", "SEDeviance", "MeanCorrelation", "SECorrelation", "Discrimination.MEAN", "Null", "MeanTotalDeviance", "Resid", "MeanResidualDeviance","Correlation","NTree", "tc", "lr")
  
  for (i in 1:nrow(mat)) {
    #i=1
    tc = mat[i,1]
    lr = mat[i,2]
  
    model <- gbm.step(dat, gbm.x=c(3:4, 6,7,11:50), gbm.y=2, family="poisson", tree.complexity=tc, learning.rate=lr, bag.fraction=0.5)

    out = cbind(data.frame(model$cv.statistics[1:5]), data.frame(model$self.statistics[1:5]), data.frame(model$n.tree), tc, lr)
    results[i,] <- out
  }
  results
  #colnames(results) <- c("MeanDeviance", "SEDeviance", "MeanCorrelation", "SECorrelation", "Discrimination.MEAN", "Null", "MeanTotalDeviance", "Resid", "MeanResidualDeviance","Correlation","NTree", "tc", "lr")

}

#Run Boosted Regression ####
# determine optimal number of trees with Elith's function that steps forward
# with tree complexity of 5 and learning rate of 0.01  and a bag fraction (stochasticity) of 0.5 
#below function is an alternative to the one that Ridgeway provides in library(gbm) 
# gbm.x = define column of predictor variables  with a vector consisting of the indexes for the data columns containing the predictors
# gbm.y = index of the response variable
# error structure of the response (because its presence absence they are using bernoulli. other options include poisson, laplace, gaussian 
# trying for a tree complexity of 5 to start out
# trying for learning rate of 0.01 to start out 
# bag fraction default is 0.75 but starting out at 0.5 -to make them deterministic you would set the bag fraction to 1 (i.e. all data would be dplyr::selected at every time)
## - note on bag fraction - unless you set the seed then the results are going to be slightly different each time because its dplyr::selecting a random 50% (in this case bag.fraction =0.5) of the training data at every tree building step

#build mat with tc and lr combinations
tc = matrix(rep(c(5,7,10), each=4))
lr = matrix(rep(c(0.01, 0.005, 0.0005, 0.0001), 3))
mat=cbind(tc, lr)

mat_shrt <- mat[1:2,]


#pull out a training set
#determining the size of the training set to the independent test set
#https://stackoverflow.com/questions/13610074/is-there-a-rule-of-thumb-for-how-to-divide-a-dataset-into-training-and-validatio

#For example,
#Taking the first rule of thumb (i.e.validation set should be inversely proportional to the square root of the number of free adjustable parameters), you can conclude that if you have 32 adjustable parameters, 
#the square root of 32 is ~5.65, the fraction should be 1/5.65 or 0.177 (v/t). Roughly 17.7% should be reserved for validation and 82.3% for training.

#I have 56 adjustable parameters so sqrt(56)=7.48,   1/7.48 = 0.133. 13.3 % should be reserved for validation and 86% for training. 

set.seed(12345) #set the seed so that it always samples the same

TB_train <- TB[sample(nrow(TB),0.86*(nrow(TB))),]

#run the BRT 
TB_BRTmodels <- runBRT(mat, TB_train)
write.csv(TB_BRTmodels, "BRT_testing_TB.csv")
TB_BRTmodels <- read.csv("BRT_testing_TB.csv")



#pull out the model with the greatest reduction in deviance
#use that formulation of tc and lr and then re-run to set up for model simplification

tc_use <- TB_BRTmodels$tc[TB_BRTmodels$MeanResidualDeviance == min(TB_BRTmodels$MeanResidualDeviance)]
lr_use <- TB_BRTmodels$lr[TB_BRTmodels$MeanResidualDeviance == min(TB_BRTmodels$MeanResidualDeviance)]

TBmodel <- gbm.step(TB_train, gbm.x=c(3:4, 6,7,11:50), gbm.y=2, family="poisson", tree.complexity=tc_use, learning.rate=lr_use, bag.fraction=0.5)
save(TBmodel, file = "TBmodel.R")
load(file="TBmodel.R")
TBmodel$contributions
# Simplify the model ####
# I have a lot of variables so I'm going to try to simplify the predictor set
# elimination of non-informative variables simplifies the model by dropping the least important predictor and then refitting the model and sequentially
# repeating the process until some stopping criterion is reached
# Results in reordering of the contributions for the retained predictors
# 

#try dropping a portion of them
#TB_simp <- gbm.simplify(TBmodel, n.drops=25)
#save(TB_simp, file="TBsimp.R")
#load(file="TBsimp.R")

#if you want to run this from the beginning when loading in TBmodel you need to load TB_train and TBmodel
TB_simp_all <- gbm.simplify(TBmodel, n.drops=40)
save(TB_simp_all, file="TB_simp_all.R")
load(file="TB_simp_all.R")

#determine predictors and find optimum number of predictors to drop
TB_simp_all$pred.list
ndrop <- as.numeric(gsub("drop.", "", as.character(rownames(TB_simp_all$deviance.summary[TB_simp_all$deviance.summary == min(TB_simp_all$deviance.summary[1]),]))))

#run the model using the variables remaining after ndrop are dropped
TB_dropped <- gbm.step(TB_train, gbm.x=TB_simp_all$pred.list[[ndrop]], gbm.y=2, family= "poisson", tree.complexity=tc_use, learning.rate=lr_use, bag.fraction=0.5)
save(TB_dropped, file="TB_dropped.R")
load(file="TB_dropped.R")

cont <- cbind(as.character(TB_dropped$contributions$var), TB_dropped$contributions$rel.inf)

#Plot BRT ####
#par(mfrow=c(3,4)) #for 12 plots 


#File <- ("U:/PhD_projectfiles/Exported_R_Datafiles/TB_gbmplots.tiff")
#if (file.exists(File)) stop(File, " already exists")
#dir.create(dirname(File), showWarnings = FALSE)

#tiff(File, units="in", width=5, height=5, res=300)

gbm.plot.liz(test, n.plots=6, smooth=T, y.label="predicted numbers", write.title=F, common.scale=F)

#Boostrapping original data set ####
#Perform variable dropping procedure

library(tictoc)
tic()
N=10
contributions <- NA


for(i in 1:N) {
  cont <- NA
  
  #resample
  TB_train <- TB[sample(nrow(TB),0.86*(nrow(TB))),]
  
  #run BRT
  TBmodel <- gbm.step(TB_train, gbm.x=c(3:4, 6,7,11:50), gbm.y=2, family="poisson", tree.complexity=tc_use, learning.rate=lr_use, bag.fraction=0.5)
  
  #run model dropping procedure
  TB_simp_all <- gbm.simplify(TBmodel, n.drops=40)
  ndrop <- as.numeric(gsub("drop.", "", as.character(rownames(TB_simp_all$deviance.summary[TB_simp_all$deviance.summary == min(TB_simp_all$deviance.summary[1]),]))))
  
  #run the model using the variables remaining after ndrop are dropped
  TB_dropped <- gbm.step(TB_train, gbm.x=TB_simp_all$pred.list[[ndrop]], gbm.y=2, family= "poisson", tree.complexity=tc_use, learning.rate=lr_use, bag.fraction=0.5)
  
  #collect important variables
  cont <- as.data.frame(cbind(as.character(TB_dropped$contributions$var), TB_dropped$contributions$rel.inf))
  cont$run <- as.character(i)

  contributions<- rbind(contributions, cont)
  
}
toc()

write.csv(contributions, "TB_contributions_1.csv")

colnames(contributions) <- c("Var.name", "Var.imp", "Run")
contributions$Var.name <- as.factor(contributions$Var.name)
contributions$Var.imp <- as.numeric(as.character(contributions$Var.imp))
contributions <- contributions[-1,]
summ <- contributions %>% group_by(Var.name) %>% summarize(mean=mean(Var.imp), sd=sd(Var.imp)) %>% arrange(desc(mean))

















#updated gbm plot function so that the response is plotted NOT scaled by the mean which is how Elith has it ####

"gbm.plot.liz" <-
  function(gbm.object,                # a gbm object - could be one from gbm.step
           variable.no = 0,               # the var to plot - if zero then plots all
           nt = gbm.object$n.trees,       # how many trees to use
           smooth = FALSE,                # should we add a smoothed version of the fitted function 
           rug = T,                       # plot a rug of deciles
           n.plots = length(pred.names),  # plot the first n most important preds
           common.scale = T,              # use a common scale on the y axis
           write.title = T,               # plot a title above the plot
           y.label = "fitted function",   # the default y-axis label
           x.label = NULL,                # the default x-axis label
           show.contrib = T,              # show the contribution on the x axis
           plot.layout = c(3,4),          # define the default layout for graphs on the page  
           rug.side = 3,                  # which axis for rug plot? default (3) is top; bottom=1
           rug.lwd = 1,                   # line width for rug plots
           rug.tick = 0.03,               # tick length for rug plots
           ...                            # other arguments to pass to the plotting 
           # useful options include cex.axis, cex.lab, etc.
  )
  {
   
    require(gbm)
    require(splines)
    
    gbm.call <- gbm.object$gbm.call
    gbm.x <- gbm.call$gbm.x
    pred.names <- gbm.call$predictor.names
    response.name <- gbm.call$response.name
    dataframe.name <- gbm.call$dataframe
    data <- eval(parse(text = dataframe.name))
    
    max.plots <- plot.layout[1] * plot.layout[2]
    plot.count <- 0
    n.pages <- 1
    
    if (length(variable.no) > 1) {stop("only one response variable can be plotted at a time")}
    
    if (variable.no > 0) {   #we are plotting all vars in rank order of contribution
      n.plots <- 1
    }
    
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      cat("warning - reducing no of plotted predictors to maximum available (",max.vars,")\n",sep="")
    }
    
    predictors <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    responses <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    
    for (j in c(1:n.plots)) {  #cycle through the first time and get the range of the functions
      if (n.plots == 1) {
        k <- variable.no
      }
      else k <- match(gbm.object$contributions$var[j],pred.names)
      
      if (is.null(x.label)) var.name <- gbm.call$predictor.names[k]
      else var.name <- x.label
      
      pred.data <- data[,gbm.call$gbm.x[k]]
      
      response.matrix <- plot.gbm.liz(gbm.object, i.var = k, n.trees = nt, return.grid = TRUE,...)
      
      predictors[[j]] <- response.matrix[,1]
      if (is.factor(data[,gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]],levels = levels(data[,gbm.call$gbm.x[k]]))
      }
     responses[[j]] <- response.matrix[,2] - mean(response.matrix[,2])
      responses[[j]] <- response.matrix[,2] #EH edit, want the response on the normal scale not mean adjusted
      
      if(j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
      }
      else {
        ymin = min(ymin,min(responses[[j]]))
        ymax = max(ymax,max(responses[[j]]))
      }
    }
    
    # now do the actual plots
    
    for (j in c(1:n.plots)) {
      
      if (plot.count == max.plots) {
        plot.count = 0
        n.pages <- n.pages + 1
      }
      
      if (plot.count == 0) {
        windows(width = 11, height = 8)
        par(mfrow = plot.layout)
      }
      
      plot.count <- plot.count + 1
      
      if (n.plots == 1) {
        k <- match(pred.names[variable.no],gbm.object$contributions$var)
        if (show.contrib) {
          x.label <- paste(var.name,"  (",round(gbm.object$contributions[k,2],1),"%)",sep="")
        }
      }
      else {
        k <- match(gbm.object$contributions$var[j],pred.names)
        var.name <- gbm.call$predictor.names[k]
        if (show.contrib) {
          x.label <- paste(var.name,"  (",round(gbm.object$contributions[j,2],1),"%)",sep="")
        }
        else x.label <- var.name
      }
      
      if (common.scale) {
        plot(predictors[[j]],responses[[j]],ylim=c(ymin,ymax), type='l',
             xlab = x.label, ylab = y.label, ...)
      }
      else {
        plot(predictors[[j]],responses[[j]], type='l', 
             xlab = x.label, ylab = y.label, ...)
      }
      if (smooth & is.vector(predictors[[j]])) {
        temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.3)
        lines(predictors[[j]],fitted(temp.lo), lty = 2, col = 2)
      }
      if (plot.count == 1) {
        if (write.title) {
          title(paste(response.name," - page ",n.pages,sep=""))
        }
        if (rug & is.vector(data[,gbm.call$gbm.x[k]])) {
          rug(quantile(data[,gbm.call$gbm.x[k]], probs = seq(0, 1, 0.1), na.rm = TRUE), side = rug.side, lwd = rug.lwd, ticksize = rug.tick)
        }
      }
      else {
        if (write.title & j == 1) {
          title(response.name)
        }
        if (rug & is.vector(data[,gbm.call$gbm.x[k]])) {
          rug(quantile(data[,gbm.call$gbm.x[k]], probs = seq(0, 1, 0.1), na.rm = TRUE), side = rug.side, lwd = rug.lwd, ticksize = rug.tick)
        }
      }
    }
  }























































#Interrogate and plot interactions ####
#code will list 5 most important pairwise interactions

find.int <- gbm.interactions(test)
find.int$rank.list

gbm.perspec(test, 6,1,z.range=c(0,20),theta=30, phi=30, expand=0.5, col='lightblue', ltheta=120, shade=0.2, ticktype='detailed') -> res





# Predict to independent test set ####
TB_test <- anti_join(TB, TB_train)
preds <- predict.gbm(model, TB_test, n.trees= model$gbm.call$best.trees, type="response" )

#Prediction statistics ####
calc.deviance(TB_test$number, preds, calc.mean=T)
roc(TB_test$number, preds)



# Plot predictor and response data ####
#PREDICTORS
# use gather to get it into format expected for dplyr summarize 
#salinity, temperature, Nit-val, MeanMonthlyRF, river_flow
selec_enviro <- cleaned_TB %>% dplyr::select("year", "month", "salinity", "temperature", "Nit_val", "MeanMonthlyRF", "river_flow") %>% 
  transmute(year=year, month=month, sal_scal = scale(salinity), temp_scal = scale(temperature), N_scal = scale(Nit_val), RF_scal=scale(MeanMonthlyRF), river_scal = scale(river_flow)) %>% 
  gather(sal_scal, temp_scal, N_scal,RF_scal, river_scal, key="variable", value="observed")

sum_env <- selec_enviro %>% group_by(variable, year, month) %>% summarise(N=sum(!is.na(observed)), mean=mean(observed, na.rm=TRUE), sd=sd(observed, na.rm=TRUE), se = sd/sqrt(N))

ggplot(sum_env, aes(year, mean)) + geom_line()+geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  facet_grid(month~variable)+
  scale_y_continuous(breaks=seq(0,120,20), labels=seq(0,120,20))+
  scale_x_continuous(breaks=seq(1988, 2014,4), labels=seq(1988, 2014,4))+
  geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  xlab("Year")+
  ylab("Zero-centered mean Value")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 									
        panel.background=element_rect(fill='white', colour='black'),
        axis.title.y = element_text(colour="black", size=10), # changing font of y axis title
        axis.title.x = element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black", size=10, angle=45), #changing  colour and font of x axis text
        axis.text.y=element_text(colour="black", size=10),
        strip.text.y = element_text(size=10))  #changing colour and font of y axis

#Z_anomaly, maxT, minT
cd_ENV <- cleaned_TB %>% dplyr::select("year", "month", "Z_anom", "MaxT_anom", "MinT_anom") %>%
  gather(Z_anom, MaxT_anom, MinT_anom, key="variable", value="observed")

cd_sum <- cd_ENV %>% group_by(variable, year, month) %>% summarise(N=sum(!is.na(observed)), mean=mean(observed, na.rm=TRUE), sd=sd(observed, na.rm=TRUE), se = sd/sqrt(N))

ggplot(cd_sum, aes(year, mean)) + geom_line()+geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  facet_grid(month~variable)+
  #scale_y_continuous(breaks=seq(0,120,20), labels=seq(0,120,20))+
  scale_x_continuous(breaks=seq(1988, 2014,4), labels=seq(1988, 2014,4))+
  #geom_ribbon(aes(ymin=mean-se, ymax=mean+se), alpha=0.1)+
  xlab("Year")+
  ylab("Zero-centered mean Value")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), 									
        panel.background=element_rect(fill='white', colour='black'),
        axis.title.y = element_text(colour="black", size=10), # changing font of y axis title
        axis.title.x = element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black", size=10, angle=45), #changing  colour and font of x axis text
        axis.text.y=element_text(colour="black", size=10),
        strip.text.y = element_text(size=10))  #changing colour and font of y axis


#response
num <- cleaned_TB %>% dplyr::select("year", "month", "number") %>% group_by(year, month) %>% summarise(N=length(number), sum=sum(number), mean=mean(number), num_per_haul = sum/N, sd= sd(number, na.rm=TRUE), se=sd/sqrt(N))

ggplot(num, aes(year, num_per_haul)) + geom_line()+
  geom_ribbon(aes(ymin=num_per_haul-se, ymax=num_per_haul+se), alpha=0.1) + facet_grid(month~.)


#......#####
#OLD____REGRESSION ####
#https://www.kaggle.com/climbercarmich/data-exploration-and-xgboost-model-r
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/

#Process and create train and test ####
TB_rgrs <- TB
#TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro ))
TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
table(TB_rgrs$number)


#table the number of NAs by variable to see if I should keep them in
Num_NA<-sapply(TB_rgrs,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(TB_rgrs),Count=Num_NA)

#shuffle
set.seed(12345) #set the seed so that it always samples the same
TB_rgrsslct <- TB_rgrs[sample(nrow(TB_rgrs),0.80*(nrow(TB_rgrs))),]

#split dataset into testing and training subset

#training data - this statement is necessary for trian() below
df_train <- TB_rgrsslct 

label_name="number"
train_features <- TB_rgrsslct %>% dplyr::select(-c(number))
train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
train_label <- TB_rgrsslct$number

#testing data 
TB_test <- anti_join(TB_rgrs, TB_rgrsslct)
test_data <- TB_test %>% dplyr::select(-c(number))
test_data <- as.matrix(mutate_all(test_data, function(x) as.numeric(as.character(x))))
test_label <- TB_test %>% dplyr::select(number)

# put our testing & training data into two seperates matrix objects
train_features <- as.matrix(train_features)
train_label <- as.matrix(train_label)

test_data <- as.matrix(test_data)
test_label <- as.matrix(test_label)

#convert cleaned dataframe to a dmatrix - xgboost naturally accepts sparse features format, feathres that are not presented in the sparse feature matrix are treated as missing

xgbMatrixTrain <- xgb.DMatrix(data = train_features, label = train_label, missing = "NA")
xgbMatrixTest <- xgb.DMatrix(data = test_data, label = test_label, missing = "NA")

#fit the model with the arbitrary parameters using either xgboost or xgb.train
#Dont necessarily have to do this. Better to first determine nrounds with xgb.cv and then hyperparameter tuning and THEN train the model
#from example on web: how to tune
# xgb_1 = xgboost(data = as.matrix(df_train %>%
#                                    dplyr::select(-SeriousDlqin2yrs)),
#                 label = df_train$SeriousDlqin2yrs, params = xgb_params_1,
#                 nrounds = 100,                                                 # max number of trees to build
#                 verbose = TRUE,                                         
#                 print.every.n = 1,
#                 early.stop.round = 10                                          # stop if no improvement within 10 trees
# )



#Build basic xgboost model ####
#Basic xgboost model- will be used in parameter tuning step
#xgboost parameters starting values 
xgb_params <- list(colsample_bytree = 0.7 , #similar to max features of GBM, denotes the fraction of columns to be randomly sampled for each tree, typical is 0.5 to 1 
                   subsample= 0.7  , #same as the subsample of gbm, like bag fraction of GBM, denotes the fraction of observations to be randomly sampled for ecah tree, lower values make the algorithm more convservative and prevents overfitting 
                   booster= "gbtree",  #whether to do trees or regression
                   max_depth=5, #max depth of a tree, should be tuned using CV, typical values 3-10
                   eta = eta, #analogous to learning rate in gbm
                   eval_metric = "rmse", # default according to the objective, default is rmse for regression and error for classification
                   objective = "reg:linear") # default, defines the loss function to be minimized)


#determine best number of trees to use in the tuning steps                   
xgb_cv <- xgb.cv(xgb_params, xgbMatrixTrain,
                 early_stopping_rounds = 10, #monitors the performece of the model and stops the procedure once the performance has not improved after a fixed number of training iterations
                 nfold = 10, #k-fold CV,  
                 nrounds=500) # number of trees

#where is minimum rmse 
minrmse <- which.min(xgb_cv$evaluation_log[ ,test_rmse_mean])
maxrmse <- which.max(xgb_cv$evaluation_log[ ,test_rmse_mean])

#same thing but different way
ntree <- xgb_cv$best_ntreelimit

#plot
xgb_cv$evaluation_log %>% dplyr::select(1,2,4) %>% 
  gather(type,value, -iter) %>% 
  ggplot(aes(iter,value)) + geom_line(aes(colour = type))

#minimum iterations for training and test rmse mean will happen at different places but thats because the xgb_cv is doing k fold so inherently there is a training set that is then tested on the test set. the results from the test set say whats best

#Hyperparameter tuning ####
#search using train() in caret package
#use caret package to do hyperparameter grid search

#General method for tuning
#https://www.analyticsvidhya.com/blog/2016/03/complete-guide-parameter-tuning-xgboost-with-codes-python/
#1. Chose a relative high learning rate (0.05 to 0.3). Determine the optimum number of trees for this learning rate/
#2. Tune tree-specific parameters (maxdepth, min child weight, gamma, subsample, colsample by tree) for decided learning rate and number of trees
#3. Tune regularization parameters which can help reduce model complexity and enhance performance
#4. Lower the learning rate and decide optimal parameters

# Other really good resources:Top one gives general layout, bottom one has set up exactly what it needs to be
#https://stats.stackexchange.com/questions/171043/how-to-tune-hyperparameters-of-xgboost-trees
#https://github.com/topepo/caret/issues/507


#1. Tune max depth and min child weight ####
#if nrounds (trees) is acceptible then use here to tune tree using nrounds determined above
#set up the cross validated hyper parameter search. These are all the values I want to search over. 
# eta = c(0.01,0.05,0.1),max_depth = c(2,4,6,8,10,14), min_child_weight =c(6,8,10,12), gamma=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), colsample_bytree =c(0.4, 0.6, 0.7,0.8,1.0),subsample =c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0))


#pack the training control parameters
cv.ctrl <- trainControl(method = "cv", number = 10, 
                        #summaryFunction = defaultSummary, #for regression (this does mean squared error and R-squared)
                        allowParallel=T, verboseIter=T)

xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(2,4,6,8,10,12,14),
                        gamma = c(0.001),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(6,8,10,12),
                        subsample = c(0.8 ))


#train the model for each parameter combination in the grid, using CV to evaluate 
label_var = "number"
xgb_tune <-train(x=xgbMatrixTrain,
                 #y= train_label, #if you use this format it thinks you are doing classification because you specify label, idkw
                 y=TB_rgrsslct %>% pull(label_var),
                 method = 'xgbTree',
                 #objective = 'reg:linear',
                 #eval_metric = "poisson-nloglik",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid)

# scatter plot of the rmse against max_depth and eta - from example 
ggplot(xgb_tune$results, aes(x = as.factor(min_child_weight), y = max_depth, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

max_depth <- xgb_tune$results[xgb_tune$results$RMSE == max(xgb_tune$results$RMSE),]$max_depth
min_child_weight <- xgb_tune$results[xgb_tune$results$RMSE == max(xgb_tune$results$RMSE),]$min_child_weight


#2. Tune gamma ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(0.001, 0.1, 0.2, 0.3, 0.4),
                        colsample_bytree = c(0.8),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.8 ))

xgb_tune <-train(x=xgbMatrixTrain, 
                 y=TB_rgrsslct %>% pull(label_var),
                 method = 'xgbTree',
                 #objective = 'reg:linear',
                 #eval_metric = "poisson-nloglik",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid)

ggplot(xgb_tune$results, aes(x = as.factor(gamma), y = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

gamma<- xgb_tune$results[xgb_tune$results$RMSE == max(xgb_tune$results$RMSE),]$gamma


#RMSE is lowest when gamma = 0.1 

#3. Tune subsample and colsample by tree ####
xgb.grid <- expand.grid(nrounds=c(ntree),
                        eta = c(eta),
                        max_depth = c(max_depth),
                        gamma = c(gamma),
                        colsample_bytree = c(0.6,0.7, 0.8, 0.9),
                        min_child_weight =c(min_child_weight),
                        subsample = c(0.6,0.7, 0.8, 0.9))

xgb_tune <-train(x=xgbMatrixTrain, 
                 y=TB_rgrsslct %>% pull(label_var),
                 method = 'xgbTree',
                 #objective = 'reg:linear',
                 #eval_metric = "poisson-nloglik",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid)

ggplot(xgb_tune$results, aes(x = as.factor(colsample_bytree), y = subsample, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

colsample_bytree<- xgb_tune$results[xgb_tune$results$RMSE == max(xgb_tune$results$RMSE),]$colsample_bytree
subsample <- xgb_tune$results[xgb_tune$results$RMSE == max(xgb_tune$results$RMSE),]$subsample


#this will determine best parameters...then use those parameters with prediction

#4. Train model with found parameters ####
#train the model using these best parameters found above

model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = max_depth, # the maximum depth of each decision tree
                       nround = ntree, # max number of boosting iterations
                       objective = "reg:linear",
                       eta=eta,
                       gamma=gamma,
                       colsample_bytree =colsample_bytree,
                       subsample=subsample) # the objective function 


#View feature importance/influence from the learnt model
importance_matrix <- t(data.frame(xgb.importance(names(df_train %>%  dplyr::select(-number)), model=model_tuned)))
print(importance_matrix)
print(importance_matrix$Feature)
xgb.plot.importance(importance_matrix = importance_matrix)
#xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()


# Predict ####
pred <- predict(model_tuned, xgbMatrixTest)

#evaluate model performance
#from caret package
RMSE(pred, test_label) #from caret package
R2(pred, test_label)

