# Boosted regression tree Elith et al tutorial 

library(gbm)

#source brt.functions from Elith
source("~/Desktop/PhD project/Projects/Seatrout/Seatrout_ENV_Chapter2/brt.functions.R")

#read data stored in the supplementary file 
model.data <- read.csv("~/Desktop/PhD project/PhD Literature/JANE_1390_sm_AppendixS3/data/model.data.csv")

#Anguas = binary response variable (presence/absece variable)
#All other variables (12 of them) are acting as predictor variables 

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

# all other options specified in their gbm.step function are defined in the source code file. 

anguas <- gbm.step(data=model.data, gbm.x=c(3:14), gbm.y =2, family="bernoulli", tree.complexity=5, learning.rate=0.01, bag.fraction=0.5)

# to access all of the variable use names
names(anguas)

#the above was made with a first guess at settings using the rules of thumb but it made only 550 trees so the next step would be to reduce the learning rate ( because reduced learning rate means more trees are built because overall contribution of each tree is reduced therefor it needs more overall trees to get to the conclusion)

anguas.tc5.lr005 <- gbm.step(data=model.data, gbm.x=c(3:14), gbm.y =2, family="bernoulli", tree.complexity=5, learning.rate=0.005, bag.fraction=0.5)

#simplify the above model and remove the predictors- appendix 2 
# here we assess the value in simplifying the model built with a lr=0.005 and drop up to 5 variables
# the n.drop argument is a defulat so if you dont specify how many to drop then it will continue until the average change in predictive deviance exceeds the original standard error as calculated in the original gbm.step

anguas.simp <- gbm.simplify(anguas.tc5.lr005, n.drops=5)
