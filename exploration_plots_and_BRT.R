# code to do boosted regression trees on the data produced in Join_Env_Vars_with_FIM.R

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
  setwd(data)
  source("~/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2/brt.functions.R")
} else {
  #work_comp = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/FIMData/NEWNov7"
  #phys_dat = "U:/PhD_projectfiles/Raw_Data/Seatrout_FIM_Data/Raw Data from fimaster-data-sas-inshore"
  #enviro_data = "U:/PhD_projectfiles/Raw_Data/Environmental_Data"
  data =   "U:/PhD_projectfiles/Exported_R_Datafiles/Seatrout_ENV_Chapter2"
  source_location = "U:/R_For_School/Seatrout_ENV_Chapter2"
  source("U:/R_For_School/Seatrout_ENV_Chapter2/brt.functions.R")
  setwd(data)
}

library(gbm)
library(ggplot2)
library(tidyverse)

#select only variables that are important (response and predictor variables )
# number -34
# 
# salinity  (FIM-at moment) - 43
# temperature (FIM - at moment) - 54
# river_flow (mean monthly flow rate) - 70
# Nit_val (month) - 71
# Phos_val (month) - 72
# Sal_val  (month) - 73
# WatTemp_val  (month) - 74
# Z_val    (month)
# Z_anom   (month)
# MaxT_val  (month)
# MaxT_anom  (month)
# MinT_val   (month)
# MinT_anom   (month)
# MeanMonthlyRF  (month)
# ext_coef
# spring_dis
# summer_dis
# winter_dis
# prev_autumn_dis
# spring_Z_val
# spring_Z_anom
# spring_MaxT_val	
# spring_MaxT_anom	
# spring_MinT_val	
# spring_MinT_anom	
# summer_Z_val	
# summer_Z_anom	
# summer_MaxT_val	
# summer_MaxT_anom	
# summer_MinT_val	
# summer_MinT_anom	
# winter_Z_val	
# winter_Z_anom	
# winter_MaxT_val	
# winter_MaxT_anom	
# winter_MinT_val	
# winter_MinT_anom	
# prev_autumn_Z_val	
# prev_autumn_Z_anom	
# prev_autumn_MaxT_val	
# prev_autumn_MaxT_anom	
# prev_autumn_MinT_val	
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
#Import data ####
TB <- read.csv("TB_all_env_with_lag.csv", header=TRUE, row.names=1)
TB <- TB[,c(30, 34,43,54,61, 70:129)]

# Plot predictor and response data ####
#PREDICTORS
# use gather to get it into format expected for dplyr summarize 
#salinity, temperature, Nit-val, MeanMonthlyRF, river_flow
selec_enviro <- TB %>% select("year", "month", "salinity", "temperature", "Nit_val", "MeanMonthlyRF", "river_flow") %>% 
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
cd_ENV <- TB %>% select("year", "month", "Z_anom", "MaxT_anom", "MinT_anom") %>%
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
num <- TB %>% select("year", "month", "number") %>% group_by(year, month) %>% summarise(N=length(number), sum=sum(number), mean=mean(number), num_per_haul = sum/N, sd= sd(number, na.rm=TRUE), se=sd/sqrt(N))

ggplot(num, aes(year, num_per_haul)) + geom_line()+
  geom_ribbon(aes(ymin=num_per_haul-se, ymax=num_per_haul+se), alpha=0.1) + facet_grid(month~.)

# Build runBrt function #### 
# that runs BRT at different levels of tc and lr 

# for all combinations of tree complexity (tc) and learning rate (lr) run on a smaller training set and then predict to 
# an independent test set. for each run capture the deviance of training and CV as well as the predictive deviance of the independent set
# With the best option do variable selection to trim down to get more realistic number of variables. then build plots and explore variable contributions

# tc (will range from 5,7,10)
# lr (0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001)

runBRT= function(mat, dat) {
  #initialize an empty dataframe
  results= data.frame(matrix(data=NA, nrow=100, ncol=13))


  for (i in 1:nrow(mat)) {
    tc = mat[i,1]
    lr = mat[i,2]
  
    model <- gbm.step(dat, gbm.x=c(3:4, 6,7,11:65), gbm.y=2, family="poisson", tree.complexity=tc, learning.rate=lr, bag.fraction=0.5)
    
    out = cbind(data.frame(model$cv.statistics[1:5]), data.frame(model$self.statistics[1:5]), data.frame(model$n.tree), tc, lr)
    results[i,] <- out
  }
  results

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
# bag fraction default is 0.75 but starting out at 0.5 -to make them deterministic you would set the bag fraction to 1 (i.e. all data would be selected at every time)
## - note on bag fraction - unless you set the seed then the results are going to be slightly different each time because its selecting a random 50% (in this case bag.fraction =0.5) of the training data at every tree building step

#build mat with tc and lr combinations
tc = matrix(rep(c(5,7,10), each=5))
lr = matrix(rep(c(0.05, 0.01, 0.005, 0.0005, 0.0001), 3))
mat=cbind(tc, lr)

#pull out a training set
#determining the size of the training set to the independent test set
#https://stackoverflow.com/questions/13610074/is-there-a-rule-of-thumb-for-how-to-divide-a-dataset-into-training-and-validatio

#For example,
#Taking the first rule of thumb (i.e.validation set should be inversely proportional to the square root of the number of free adjustable parameters), you can conclude that if you have 32 adjustable parameters, 
#the square root of 32 is ~5.65, the fraction should be 1/5.65 or 0.177 (v/t). Roughly 17.7% should be reserved for validation and 82.3% for training.

#I have 56 adjustable parameters so sqrt(56)=7.48,   1/7.48 = 0.133. 13.3 % should be reserved for validation and 86% for training. 

set.seed(12345) #set the seed so that it always samples the same

TB_train <- TB[sample(nrow(TB),0.86*(nrow(TB)),]

#run the BRT 
runBRT(mat, TB_train)




set.seed(12345)



#plot

par(mfrow=c(3,4))
gbm.plot(TB.tc5.lr0005_long, n.plots=12, smooth=T, write.title=F)

# Simplify the model ####
# I have a lot of variables so I'm going to try to simplify the predictor set
# elimination of non-informative variables simplifies the model by dropping the least important predictor and then refitting the model and sequentially
# repeating the process until some stopping criterion is reached
# Results in reordering of the contributions for the retained predictors
# 

#try dropping all of them
TB.lr0005_simp <- gbm.simplify(TB.tc5.lr0005_long, n.drops=58)
TB.lr0005_simp$pred.list
#use the red vertical line or look up results in the _simp object 
#says dropping all variables up to 57 is best but I want to explore a dataset with a few more included variables






# Predict to independent test set ####