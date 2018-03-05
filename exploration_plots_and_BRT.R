# code to do boosted regression trees on the data produced in Join_Env_Vars_with_FIM.R

# Set Working Directory ####
#must change working directory for data when working on personal vs work computer
rm(list=ls())

# Set Location
IS_HOME = TRUE

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
#import ####
TB <- read.csv("TB_all_env_with_lag.csv", header=TRUE, row.names=1)
TB <- TB[,c(30, 34,43,54,61, 70:129)]

# plot predictor and response data ####
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

# Boosted Regression ####
# for all combinations of tree complexity (tc) and learning rate (lr) run on a smaller training set and then predict to 
# an independent test set. for each run capture the deviance of training and CV as well as the predictive deviance of the independent set
# With the best option do variable selection to trim down to get more realistic number of variables. then build plots and explore variable contributions

# tc (will range from 5,7,10)
# lr (0.01, 0.005, 0.001, 0.0005, 0.0001)
tc=matrix(c(5,7), nrow=2)
lr=matrix(c(0.001, 0.0005, 0.0001),nrow=3)
set.seed(12345)

#evaluate at a tc of 5 (lr=0.001, 0.0005, 0.0001) and tc = 7 (lr=0.001, 0.0005, 0.0001)
mat =matrix(c(5, 0.001, 5, 0.0005, 5, 0.0001,7,0.001, 7, 0.0005, 7, 0.0001), ncol=2, byrow=TRUE)
TB_train <- TB[sample(nrow(TB),1000),]

runBRT= function(mat, dat) {
  #initialize an empty dataframe
results= data.frame(matrix(data=NA, nrow=100, ncol=13))
#row_index = 1

for (i in 1:nrow(mat)) {
  tc = mat[i,1]
  lr = mat[i,2]
  
  model <- gbm.step(dat, gbm.x=c(3:4, 6,7,11:65), gbm.y=2, family="poisson", tree.complexity=tc, learning.rate=lr, bag.fraction=0.5)
    
  out = cbind(data.frame(model$cv.statistics[1:5]), data.frame(model$self.statistics[1:5]), data.frame(model$n.tree), tc, lr)
  results[i,] <- out
}
results
#colnames(results) = colnames(data.frame(model$cv.statistics))
}

runBRT(mat, TB_train)









TB.tc5.lr01_long<- gbm.step(data=TB_test, gbm.x=c(3:4, 6,7, 11:65), gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.001, bag.fraction=0.5)


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



set.seed(12345)
TB_test <- TB[sample(nrow(TB),1000),]

TB.tc5.lr001<- gbm.step(data=TB_test, gbm.x=c(3:4, 6,7, 11:18), gbm.y =2, family="poisson", tree.complexity=5, learning.rate=0.001, bag.fraction=0.5)
# fitting final gbm model with a fixed number of  1550  trees for  number 
# 
# mean total deviance = 6.579 
# mean residual deviance = 4.903 
# 
# estimated cv deviance = 6.221 ; se = 0.636 
# 
# training data correlation = 0.513 
# cv correlation =  0.159 ; se = 0.047 
# 
# elapsed time -  0.38 minutes 

# > TB.tc5.lr001$contributions
# var    rel.inf
# river_flow       river_flow 32.3112917
# Nit_val             Nit_val 24.5198429
# salinity           salinity 16.3783093
# temperature     temperature 10.3784727
# MeanMonthlyRF MeanMonthlyRF  3.7138596
# MinT_val           MinT_val  2.8229897
# Z_val                 Z_val  2.2821023
# MinT_anom         MinT_anom  2.2714244
# MaxT_val           MaxT_val  2.0058365
# MaxT_anom         MaxT_anom  1.7024251
# Z_anom               Z_anom  1.4263084
# aten_ceof         aten_ceof  0.1871373
# 
# 

#all variables
TB.tc5.lr01_long<- gbm.step(data=TB_test, gbm.x=c(3:4, 6,7, 11:65), gbm.y =2, family="poisson", tree.complexity=5, learning.rate=0.001, bag.fraction=0.5)
# fitting final gbm model with a fixed number of  2400  trees for  number 
# 
# mean total deviance = 6.579 
# mean residual deviance = 1.406 
# 
# estimated cv deviance = 2.702 ; se = 0.52 
# 
# training data correlation = 0.701 
# cv correlation =  0.494 ; se = 0.043 
# 
# elapsed time -  1.27 minutes 
TB.tc5.lr01_long$contributions 
# var      rel.inf
# atspawn_nitro                 atspawn_nitro 18.599744502
# river_flow                       river_flow 14.573265832
# atspawn_salinity           atspawn_salinity 10.834320095
# atspawn_waterT               atspawn_waterT  8.546931218
# Nit_val                             Nit_val  6.452599121
# avg_last2_nitro             avg_last2_nitro  6.250294240
# avg_last3_nitro             avg_last3_nitro  5.797889513
# salinity                           salinity  3.598311116
# temperature                     temperature  3.280716614
# spring_dis                       spring_dis  2.896317769
# second_spawn_salinity second_spawn_salinity  2.426642503
# spring_RF                         spring_RF  1.735616818
# MeanMonthlyRF                 MeanMonthlyRF  1.666977110
# MinT_anom                         MinT_anom  1.590680049
# Z_val                                 Z_val  1.445424944
# spring_MinT_val             spring_MinT_val  1.321768742
# second_spawn_waterT     second_spawn_waterT  1.019598867
# MaxT_anom                         MaxT_anom  0.953213782
# first_spawn_waterT       first_spawn_waterT  0.944026167
# spring_dis_ALL               spring_dis_ALL  0.724669669
# spring_MaxT_val             spring_MaxT_val  0.663730899
# MaxT_val                           MaxT_val  0.646790436
# Z_anom                               Z_anom  0.634804971
# spring_Z_val                   spring_Z_val  0.589273275
# MinT_val                           MinT_val  0.468472669
# summer_dis                       summer_dis  0.465439490
# summer_MinT_val             summer_MinT_val  0.400747725
# first_spawn_salinity   first_spawn_salinity  0.303528071
# summer_RF                         summer_RF  0.281149495
# third_spawn_waterT       third_spawn_waterT  0.273342525
# summer_Z_val                   summer_Z_val  0.188725683
# third_spawn_salinity   third_spawn_salinity  0.129298132
# summer_MaxT_val             summer_MaxT_val  0.124133318
# spring_MinT_anom           spring_MinT_anom  0.119192177
# summer_dis_ALL               summer_dis_ALL  0.051454401
# prev_autumn_MaxT_val   prev_autumn_MaxT_val  0.000908062
# aten_ceof                         aten_ceof  0.000000000
# winter_dis                       winter_dis  0.000000000
# prev_autumn_dis             prev_autumn_dis  0.000000000
# spring_Z_anom                 spring_Z_anom  0.000000000
# spring_MaxT_anom           spring_MaxT_anom  0.000000000
# summer_Z_anom                 summer_Z_anom  0.000000000
# summer_MaxT_anom           summer_MaxT_anom  0.000000000
# summer_MinT_anom           summer_MinT_anom  0.000000000
# winter_Z_val                   winter_Z_val  0.000000000
# winter_Z_anom                 winter_Z_anom  0.000000000
# winter_MaxT_val             winter_MaxT_val  0.000000000
# winter_MaxT_anom           winter_MaxT_anom  0.000000000
# winter_MinT_val             winter_MinT_val  0.000000000
# winter_MinT_anom           winter_MinT_anom  0.000000000
# prev_autumn_Z_val         prev_autumn_Z_val  0.000000000
# prev_autumn_Z_anom       prev_autumn_Z_anom  0.000000000
# prev_autumn_MaxT_anom prev_autumn_MaxT_anom  0.000000000
# prev_autumn_MinT_val   prev_autumn_MinT_val  0.000000000
# prev_autumn_MinT_anom prev_autumn_MinT_anom  0.000000000
# winter_RF                         winter_RF  0.000000000
# prev_autumn_RF               prev_autumn_RF  0.000000000
# winter_dis_ALL               winter_dis_ALL  0.000000000
# prev_autumn_dis_ALL     prev_autumn_dis_ALL  0.000000000

TB.tc5.lr0001_long<- gbm.step(data=TB_test, gbm.x=c(3:4, 6,7, 11:65), gbm.y =2, family="poisson", tree.complexity=5, learning.rate=0.0001, bag.fraction=0.5)
#maximum tree limit reached- refit with faster learning rate

TB.tc5.lr0005_long<- gbm.step(data=TB_test, gbm.x=c(3:4, 6,7, 11:65), gbm.y =2, family="poisson", tree.complexity=5, learning.rate=0.0005, bag.fraction=0.5)

# fitting final gbm model with a fixed number of  4750  trees for  number 
# 
# mean total deviance = 6.579 
# mean residual deviance = 1.414 
# 
# estimated cv deviance = 2.711 ; se = 0.783 
# 
# training data correlation = 0.699 
# cv correlation =  0.552 ; se = 0.047 
# 
# elapsed time -  2.3 minutes 
TB.tc5.lr0005_long$contributions 

# atspawn_nitro                 atspawn_nitro 1.844379e+01
# river_flow                       river_flow 1.416936e+01
# atspawn_salinity           atspawn_salinity 1.133811e+01
# atspawn_waterT               atspawn_waterT 8.511908e+00
# Nit_val                             Nit_val 6.328975e+00
# avg_last3_nitro             avg_last3_nitro 5.982488e+00
# avg_last2_nitro             avg_last2_nitro 5.618517e+00
# temperature                     temperature 3.581179e+00
# salinity                           salinity 3.548959e+00
# spring_dis                       spring_dis 2.974720e+00
# second_spawn_salinity second_spawn_salinity 2.240291e+00
# MeanMonthlyRF                 MeanMonthlyRF 1.803275e+00
# spring_RF                         spring_RF 1.689192e+00
# MinT_anom                         MinT_anom 1.490997e+00
# Z_val                                 Z_val 1.321969e+00
# spring_MinT_val             spring_MinT_val 1.298389e+00
# second_spawn_waterT     second_spawn_waterT 1.185868e+00
# MaxT_anom                         MaxT_anom 1.032038e+00
# first_spawn_waterT       first_spawn_waterT 8.980710e-01
# spring_MaxT_val             spring_MaxT_val 8.301243e-01
# Z_anom                               Z_anom 7.809466e-01
# spring_dis_ALL               spring_dis_ALL 6.910207e-01
# spring_Z_val                   spring_Z_val 6.409544e-01
# MaxT_val                           MaxT_val 6.161890e-01
# summer_MinT_val             summer_MinT_val 4.921534e-01
# MinT_val                           MinT_val 4.751502e-01
# summer_dis                       summer_dis 4.730949e-01
# first_spawn_salinity   first_spawn_salinity 3.469835e-01
# summer_RF                         summer_RF 3.179904e-01
# summer_Z_val                   summer_Z_val 2.647100e-01
# third_spawn_waterT       third_spawn_waterT 2.163746e-01
# summer_MaxT_val             summer_MaxT_val 1.350016e-01
# third_spawn_salinity   third_spawn_salinity 1.145366e-01
# spring_MinT_anom           spring_MinT_anom 1.131452e-01
# summer_dis_ALL               summer_dis_ALL 3.154179e-02
# aten_ceof                         aten_ceof 1.508393e-03
# prev_autumn_MaxT_val   prev_autumn_MaxT_val 4.815030e-04
# prev_autumn_dis             prev_autumn_dis 2.887610e-07
# winter_dis                       winter_dis 0.000000e+00
# spring_Z_anom                 spring_Z_anom 0.000000e+00
# spring_MaxT_anom           spring_MaxT_anom 0.000000e+00
# summer_Z_anom                 summer_Z_anom 0.000000e+00
# summer_MaxT_anom           summer_MaxT_anom 0.000000e+00
# summer_MinT_anom           summer_MinT_anom 0.000000e+00
# winter_Z_val                   winter_Z_val 0.000000e+00
# winter_Z_anom                 winter_Z_anom 0.000000e+00
# winter_MaxT_val             winter_MaxT_val 0.000000e+00
# winter_MaxT_anom           winter_MaxT_anom 0.000000e+00
# winter_MinT_val             winter_MinT_val 0.000000e+00
# winter_MinT_anom           winter_MinT_anom 0.000000e+00
# prev_autumn_Z_val         prev_autumn_Z_val 0.000000e+00
# prev_autumn_Z_anom       prev_autumn_Z_anom 0.000000e+00
# prev_autumn_MaxT_anom prev_autumn_MaxT_anom 0.000000e+00
# prev_autumn_MinT_val   prev_autumn_MinT_val 0.000000e+00
# prev_autumn_MinT_anom prev_autumn_MinT_anom 0.000000e+00
# winter_RF                         winter_RF 0.000000e+00
# prev_autumn_RF               prev_autumn_RF 0.000000e+00
# winter_dis_ALL               winter_dis_ALL 0.000000e+00
# prev_autumn_dis_ALL     prev_autumn_dis_ALL 0.000000e+00

#plot

par(mfrow=c(3,4))
gbm.plot(TB.tc5.lr0005_long, n.plots=12, smooth=T, write.title=F)

#simplify the model ####
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

TB.lr0005.simp48 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[48]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
# fitting final gbm model with a fixed number of  1850  trees for  number 
# 
# mean total deviance = 6.72 
# mean residual deviance = 1.532 
# 
# estimated cv deviance = 2.087 ; se = 0.128 
# 
# training data correlation = 0.676 
# cv correlation =  0.495 ; se = 0.011 
# 
# elapsed time -  2.91 minutes 
TB.lr0005.simp48$contributions

# avg_last3_nitro             avg_last3_nitro 30.752230
# salinity                           salinity 14.395814
# Nit_val                             Nit_val 11.771654
# spring_dis                       spring_dis  9.232397
# temperature                     temperature  8.615432
# avg_last2_nitro             avg_last2_nitro  5.628199
# river_flow                       river_flow  5.341309
# atspawn_waterT               atspawn_waterT  4.635692
# atspawn_salinity           atspawn_salinity  4.245433
# atspawn_nitro                 atspawn_nitro  3.838398
# second_spawn_salinity second_spawn_salinity  1.543443

TB.lr0005.simp49 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[49]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)

# fitting final gbm model with a fixed number of  1250  trees for  number 
# 
# mean total deviance = 6.72 
# mean residual deviance = 1.657 
# 
# estimated cv deviance = 2.107 ; se = 0.225 
# 
# training data correlation = 0.632 
# cv correlation =  0.507 ; se = 0.015 
# 
# elapsed time -  2.35 minutes 
TB.lr0005.simp49$contributions

# var   rel.inf
# avg_last3_nitro   avg_last3_nitro 34.522112
# salinity                 salinity 13.772249
# Nit_val                   Nit_val 11.477750
# spring_dis             spring_dis  9.525590
# temperature           temperature  7.804223
# avg_last2_nitro   avg_last2_nitro  5.299159
# river_flow             river_flow  4.654218
# atspawn_salinity atspawn_salinity  4.338082
# atspawn_nitro       atspawn_nitro  4.303791
# atspawn_waterT     atspawn_waterT  4.302826

TB.lr0005.simp50 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[50]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
# fitting final gbm model with a fixed number of  1600  trees for  number 
# 
# mean total deviance = 6.72 
# mean residual deviance = 1.602 
# 
# estimated cv deviance = 2.077 ; se = 0.128 
# 
# training data correlation = 0.654 
# cv correlation =  0.503 ; se = 0.013 
# 
# elapsed time -  2.33 minutes 
TB.lr0005.simp50$contributions

# var   rel.inf
# avg_last3_nitro   avg_last3_nitro 35.737353
# salinity                 salinity 15.840826
# Nit_val                   Nit_val 12.695369
# temperature           temperature  9.869546
# river_flow             river_flow  6.934428
# atspawn_waterT     atspawn_waterT  6.174306
# avg_last2_nitro   avg_last2_nitro  4.817186
# atspawn_salinity atspawn_salinity  4.780510
# atspawn_nitro       atspawn_nitro  3.150475

TB.lr0005.simp51 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[51]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
# fitting final gbm model with a fixed number of  1100  trees for  number 
# 
# mean total deviance = 6.72 
# mean residual deviance = 1.749 
# 
# estimated cv deviance = 2.142 ; se = 0.111 
# 
# training data correlation = 0.608 
# cv correlation =  0.488 ; se = 0.011 
# 
# elapsed time -  1.9 minutes

TB.lr0005.simp51$contributions
# var   rel.inf
# avg_last3_nitro   avg_last3_nitro 39.415674
# salinity                 salinity 17.458457
# Nit_val                   Nit_val 13.749666
# river_flow             river_flow  6.974888
# atspawn_waterT     atspawn_waterT  6.693676
# avg_last2_nitro   avg_last2_nitro  6.233570
# atspawn_salinity atspawn_salinity  5.151190
# atspawn_nitro       atspawn_nitro  4.322879

TB.lr0005.simp52 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[52]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
fitting final gbm model with a fixed number of  900  trees for  number 

mean total deviance = 6.72 
mean residual deviance = 1.87 

estimated cv deviance = 2.193 ; se = 0.132 

training data correlation = 0.573 
cv correlation =  0.483 ; se = 0.007 

elapsed time -  1.64 minutes




TB.lr0005.simp52$contributions
# var   rel.inf
# avg_last3_nitro   avg_last3_nitro 44.265438
# Nit_val                   Nit_val 17.644567
# river_flow             river_flow  9.400984
# atspawn_waterT     atspawn_waterT  8.317728
# avg_last2_nitro   avg_last2_nitro  7.728439
# atspawn_salinity atspawn_salinity  7.413735
# atspawn_nitro       atspawn_nitro  5.229109

TB.lr0005.simp53 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[53]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
TB.lr0005.simp54 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[54]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
TB.lr0005.simp55 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[55]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
TB.lr0005.simp56 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[56]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
# fitting final gbm model with a fixed number of  1050  trees for  number 
# 
# mean total deviance = 6.72 
# mean residual deviance = 2.002 
# 
# estimated cv deviance = 2.306 ; se = 0.157 
# 
# training data correlation = 0.533 
# cv correlation =  0.465 ; se = 0.016 
# 
# elapsed time -  1.1 minutes

TB.lr0005.simp56$contributions
# var  rel.inf
# atspawn_nitro       atspawn_nitro 62.89121
# river_flow             river_flow 20.78388
# atspawn_salinity atspawn_salinity 16.32491

TB.lr0005.simp57 <- gbm.step(TB, gbm.x=TB.lr0005_simp$pred.list[[57]], gbm.y=2, family="poisson", tree.complexity=5, learning.rate=0.005)
# fitting final gbm model with a fixed number of  700  trees for  number 
# 
# mean total deviance = 6.72 
# mean residual deviance = 2.099 
# 
# estimated cv deviance = 2.323 ; se = 0.136 
# 
# training data correlation = 0.505 
# cv correlation =  0.463 ; se = 0.017 
# 
# elapsed time -  0.83 minutes 
TB.lr0005.simp57$contributions
# var  rel.inf
# atspawn_nitro atspawn_nitro 81.19686
# river_flow       river_flow 18.80314
