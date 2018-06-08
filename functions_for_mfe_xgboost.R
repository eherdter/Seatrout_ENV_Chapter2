#functions for xgboost
# this script accompanies exploration_plots_BRT.R


initial_poisson_xgboost = function(TB, dopospos=FALSE){
  
          TB_rgrs <- TB
          if(dopospos==TRUE) {
            TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
          } 
          
          #shuffle
          set.seed(12345) #set the seed so that it always samples the same
          TB_rgrs <- TB_rgrs[sample(nrow(TB_rgrs)),]
          
          #split dataset into testing and training subset
          numberOfTrainingSamples <- round(nrow(TB_rgrs) * .7)
          TB_rgrsslct<- TB_rgrs[1:numberOfTrainingSamples,]
          
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
          
          #Build basic xgboost model 
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
          
          #Hyperparameter tuning 
          
          #1. Tune max depth and min child weight 
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
          
          
          max_depth <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$max_depth
          min_child_weight <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$min_child_weight
          
          #2. Tune gamma 
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
          
          gamma<- xgb_tune_cp2$results[xgb_tune_cp2$results$RMSE == max(xgb_tune_cp2$results$RMSE),]$gamma
          
          #3. Tune subsample and colsample by tree
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
          
          colsample_bytree<- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$colsample_bytree
          subsample <- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$subsample
          
          #this will determine best parameters...then use those parameters with prediction
          
          #4. Train model with found parameters
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
          #xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()
          
          # Predict
          pred_cp <- predict(model_tuned_cp, xgbMatrixTest)
          
          #deviance explained
          #calculate null deviance of Poisson distribution for the intercept model
          #https://stats.stackexchange.com/questions/140044/null-deviance-in-glm-r
          new_label <- test_label +0.001
          #lf=sum(new_label * log(new_label) - new_label-log(factorial(new_label)))
          #lnull = sum(new_label * log(mean(new_label)) - mean(new_label) - log(factorial(new_label)))
          #null_dev = 2*(lf-lnull)
          
          #null deviance for NO intercept
          lnull = sum(-1 - log(factorial(new_label))) 
          lf = sum(new_label * log(new_label) - new_label - log(factorial(new_label))) 
          null_dev = 2*(lf-lnull)
          
          #calculate residual deviance
          #important that calc.mean=FALSE if not it will give you mean residual deviance which isnt what standard glms give you. 
          res_dev = calc.deviance(as.matrix(new_label), as.matrix(pred_cp), family="poisson", calc.mean=FALSE)
          
          #deviance explained
          dev_exp <- (null_dev - res_dev) / null_dev
          mae= mean(abs(as.numeric(test_label) - pred_cp))
          newList <- list("test_label"=test_label, "predictions" = pred_cp, "importance_matrix"= importance_matrix_cp, "dev_exp"= dev_exp, "mae" = mae)
          
          print(newList)
}


initial_class_xgboost=function(TB, features){
  
          TB_class <- TB
          TB_class$number[TB_class$number >=1] <- 1
          #remove variables that only depend on the animal having been present
          TB_class <- TB_class %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
          
          #shuffle
          set.seed(12345) #set the seed so that it always samples the same
          TB_class <- TB_class[sample(nrow(TB_class)),]
          
          #split dataset into testing and training subset
          numberOfTrainingSamples <- round(nrow(TB_class) * .7)
          TB_clsslct<- TB_class[1:numberOfTrainingSamples,]
          
          #training data - this statement is necessary for trian() below
          df_train <- TB_clsslct %>% mutate(number = factor(number, labels=c("Absent", "Present")))
          
          
          label_name="number"
          train_features <- TB_clsslct %>% dplyr::select(-c(number))
          train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
          train_label <- TB_clsslct$number
          
          #testing data 
          TB_test <- TB_class[-(1:numberOfTrainingSamples),]
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
          
          
          #Build basic xgboost model 
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
          
          
          #Hyperparameter tuning
          #1. Tune max depth and min child weight 
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
          
          max_depth <- xgb_tune1$results[xgb_tune1$results$ROC == max(xgb_tune1$results$ROC),]$max_depth
          min_child_weight <- xgb_tune1$results[xgb_tune1$results$ROC == max(xgb_tune1$results$ROC),]$min_child_weight
          
          #2. Tune gamma
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
          
          gamma<- xgb_tune2$results[xgb_tune2$results$ROC == max(xgb_tune2$results$ROC),]$gamma
          
          
          #3. Tune subsample and colsample by tree 
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
          
          colsample_bytree<- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$colsample_bytree
          subsample <- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$subsample
          
          #best colsample by tree =0.6, subsample =0.7 (0.6 or 0.9, also)
          
          #4. Train model with found parameters
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
          
          # Predict
          pred <- predict(model_tuned, xgbMatrixTest)
          
          # get & print the classification error
          err <- mean(as.numeric(pred > 0.5) != test_label) #same as mae
          #mae <- mean(abs(as.numeric(test_label)- as.numeric(round(pred))))
          newList <- list("test_label"=test_label, "predictions" = pred, "importance_matrix"= importance_matrix, "error"= err, "mae" =mae)
          
          print(newList)
}


manualrfe_poisson = function(data, dopospos=FALSE, complete.cases=FALSE, is.ckjxir=FALSE){

          results <- matrix(data=NA, nrow=45, ncol=5)
          
          #Process and create train and test
          TB_rgrs <- data
          if(dopospos == TRUE) {
            TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
            
            if (is.ckjxir == TRUE) {
              TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
            
              if(complete.cases==TRUE){
                TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                
              } else if(complete.cases==FALSE) {
                TB_rgrs <- TB_rgrs
              }
            }
              else if (is.ckjxir==FALSE) {
                
                if(complete.cases==TRUE){
                  TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                  
                } else if(complete.cases==FALSE) {
                  TB_rgrs <- TB_rgrs
                }
              }
          }

          else if(dopospos == FALSE) {
          TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
          
          if (complete.cases==TRUE){
               TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
               
             } else if(complete.cases==FALSE){
               TB_rgrs <- TB_rgrs
             }
          }
          
          
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
          
          
          max_depth <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$max_depth
          min_child_weight <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$min_child_weight
          
          #2. Tune gamma 
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
          
          gamma<- xgb_tune_cp2$results[xgb_tune_cp2$results$RMSE == max(xgb_tune_cp2$results$RMSE),]$gamma
          
          #3. Tune subsample and colsample by tree
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
          
          colsample_bytree<- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$colsample_bytree
          subsample <- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$subsample
          
          #this will determine best parameters...then use those parameters with prediction
          
          #4. Train model with found parameters
          #train the model using these best parameters found above
          
          
          model_tuned_cp <- xgboost(data = xgbMatrixTrain, # the data           
                                    max.depth = max_depth, # the maximum depth of each decision tree
                                    min_child_weight= min_child_weight,
                                    nround = ntree, # max number of boosting iterations
                                    objective = "count:poisson",
                                    eval_metric = "poisson-nloglik",
                                    eta=eta,
                                    gamma=gamma,
                                    colsample_bytree =colsample_bytree,
                                    subsample=subsample) # the objective function 
          
          
          importance_matrix_cp <- xgb.importance(names(df_train %>%  dplyr::select(-number)), model=model_tuned_cp)
          features <- importance_matrix_cp$Feature
          
          #TB_rgrs <- TB_rgrs %>% dplyr::select(number, features)
          
          for (i in 5:length(features))
          {
            #Process and create train and test
            TB_rgrs <- data

            if(dopospos == TRUE) {
              TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
              
              if (is.ckjxir == TRUE) {
                TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
                
                if(complete.cases==TRUE){
                  TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                  
                } else if(complete.cases==FALSE) {
                  TB_rgrs <- TB_rgrs
                }
              }
              else if (is.ckjxir==FALSE) {
                
                if(complete.cases==TRUE){
                  TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                  
                } else if(complete.cases==FALSE) {
                  TB_rgrs <- TB_rgrs
                }
              }
            }
            
            else if(dopospos == FALSE) {
              TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
              
              if (complete.cases==TRUE){
                TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                
              } else if(complete.cases==FALSE){
                TB_rgrs <- TB_rgrs
              }
            }
            

            TB_rgrs <- TB_rgrs %>% dplyr::select(number, features)
            TB_rgrs <- TB_rgrs[,1:i]
            
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
            
            
            max_depth <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$max_depth
            min_child_weight <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$min_child_weight
            
            #2. Tune gamma 
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
            
            gamma<- xgb_tune_cp2$results[xgb_tune_cp2$results$RMSE == max(xgb_tune_cp2$results$RMSE),]$gamma
            
            #3. Tune subsample and colsample by tree
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
            
            colsample_bytree<- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$colsample_bytree
            subsample <- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$subsample
            
            #this will determine best parameters...then use those parameters with prediction
            
            #4. Train model with found parameters
            #train the model using these best parameters found above
            
            
            model_tuned_cp <- xgboost(data = xgbMatrixTrain, # the data           
                                      max.depth = max_depth, # the maximum depth of each decision tree
                                      min_child_weight=min_child_weight,
                                      nround = ntree, # max number of boosting iterations
                                      objective = "count:poisson",
                                      eval_metric = "poisson-nloglik",
                                      eta=eta,
                                      gamma=gamma,
                                      colsample_bytree =colsample_bytree,
                                      subsample=subsample) # the objective function 
            
            pred_cp <- predict(model_tuned_cp, xgbMatrixTest)
            
            results[i,1] <- RMSE(pred_cp, test_label) #from caret package
            results[i,2] <- R2(pred_cp, test_label)
            results[i,3]  <- i 
            #results[i,4]  <- sum(-log(dpois(as.matrix(test_label), lambda=pred_cp))) #roughly equivalent to deviance
            
            #deviance explained
            #calculate null deviance of Poisson distribution for the intercept model
            #https://stats.stackexchange.com/questions/140044/null-deviance-in-glm-r
            new_label <- test_label +0.001
            #lf=sum(new_label * log(new_label) - new_label-log(factorial(new_label)))
            #lnull = sum(new_label * log(mean(new_label)) - mean(new_label) - log(factorial(new_label)))
            #null_dev = 2*(lf-lnull)
            
            #null deviance for NO intercept
            lnull = sum(-1 - log(factorial(new_label))) 
            lf = sum(new_label * log(new_label) - new_label - log(factorial(new_label))) 
            null_dev = 2*(lf-lnull)
            
            #calculate residual deviance
            library(dismo) #- by hand below because calc.deviance from dismo masks everything!!!
            #important that calc.mean=FALSE if not it will give you mean residual deviance which isnt what standard glms give you. 
            
            res_dev = calc.deviance(as.matrix(new_label), as.matrix(pred_cp), family="poisson", calc.mean=FALSE)
            
            #deviance explained
            dev_exp <- (null_dev - res_dev) / null_dev
            results[i,4] <- dev_exp
            results[i,5] <- mean(abs(as.numeric(test_label) - pred_cp))
          }
          newList <- list("features" = features, "results" = results)
  
  
}


manualrfe_class = function(data, full=TRUE){
          results <- matrix(data=NA, nrow=45, ncol=3)
          
          #Process and create train and test 
          TB_class <- data
          TB_class$number[TB_class$number >=1] <- 1
           #remove variables that only depend on the animal having been present
        
          if (full == TRUE){
          TB_class <- TB_class %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
          }
          
          #shuffle
          #shuffle
          set.seed(12345) #set the seed so that it always samples the same
          TB_class <- TB_class[sample(nrow(TB_class)),]
          
          #split dataset into testing and training subset
          numberOfTrainingSamples <- round(nrow(TB_class) * .7)
          TB_clsslct<- TB_class[1:numberOfTrainingSamples,]
          
          #training data - this statement is necessary for trian() below
          df_train <- TB_clsslct %>% mutate(number = factor(number, labels=c("Absent", "Present")))
          
          
          label_name="number"
          train_features <- TB_clsslct %>% dplyr::select(-c(number))
          train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
          train_label <- TB_clsslct$number
          
          #testing data 
          TB_test <- TB_class[-(1:numberOfTrainingSamples),]
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
          
          #Build basic xgboost model 
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
          
            #1. Tune max depth and min child weight 
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
          
          #2. Tune gamma
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
        
          gamma<- xgb_tune2$results[xgb_tune2$results$ROC == max(xgb_tune2$results$ROC),]$gamma
          
          
          #3. Tune subsample and colsample by tree 
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
          
        
          colsample_bytree<- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$colsample_bytree
          subsample <- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$subsample
          
          #best colsample by tree =0.6, subsample =0.7 (0.6 or 0.9, also)
          
          #4. Train model with found parameters
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
          
          features <- importance_matrix$Feature
          
          for (i in 5:length(features))
          {
            
            #loop through
            #Process and create train and test 
            TB_class <- data
            TB_class$number[TB_class$number >=1] <- 1
            
            #remove variables that only depend on the animal having been present
            TB_class <- TB_class %>% dplyr::select(number, features)
            TB_class <- TB_class[,1:i]
            
            #shuffle
            set.seed(12345) #set the seed so that it always samples the same
            TB_class <- TB_class[sample(nrow(TB_class)),]
            
            #split dataset into testing and training subset
            numberOfTrainingSamples <- round(nrow(TB_class) * .7)
            TB_clsslct<- TB_class[1:numberOfTrainingSamples,]
            
            #training data - this statement is necessary for trian() below
            df_train <- TB_clsslct %>% mutate(number = factor(number, labels=c("Absent", "Present")))
            
            label_name="number"
            train_features <- TB_clsslct %>% dplyr::select(-c(number))
            train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
            train_label <- TB_clsslct$number
            
            #testing data 
            TB_test <- TB_class[-(1:numberOfTrainingSamples),]
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
            
            #Build basic xgboost model
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
            pred <- predict(model_tuned, xgbMatrixTest)
            
            results[i,1] <- mean(as.numeric(pred > 0.5) != test_label) #from caret package, same as MAE
       
            #results[i,1] <- mean(abs(as.numeric(test_label) - as.numeric(pred)))
            #results[i,2] <- model_tuned$results$ROC
            results[i,3] <- i
              }
          newList <- list("features" = features, "results" = results)
  
  
}




feature_poisson_xgboost = function(data, features, dopospos=FALSE, complete.cases=FALSE, is.ckjxir=FALSE){
  
                  TB_rgrs <- data
                  
                  if(dopospos == TRUE) {
                    TB_rgrs <- TB_rgrs[TB_rgrs$number >=1,]
                    
                    if (is.ckjxir == TRUE) {
                      TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
                      
                      if(complete.cases==TRUE){
                        TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                        
                      } else if(complete.cases==FALSE) {
                        TB_rgrs <- TB_rgrs
                      }
                    }
                    else if (is.ckjxir==FALSE) {
                      
                      if(complete.cases==TRUE){
                        TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                        
                      } else if(complete.cases==FALSE) {
                        TB_rgrs <- TB_rgrs
                      }
                    }
                  }
                  
                  else if(dopospos == FALSE) {
                    TB_rgrs <- TB_rgrs %>% dplyr::select(-c( atspawn_nitro ,avg_last2_nitro,avg_last3_nitro, atspawn_waterT, atspawn_salinity ))
                    
                    if (complete.cases==TRUE){
                      TB_rgrs <- TB_rgrs[complete.cases(TB_rgrs),]
                      
                    } else if(complete.cases==FALSE){
                      TB_rgrs <- TB_rgrs
                    }
                  }
                  

                  var_names <- as.character(features)
                  #TB_rgrs <- TB_rgrs %>% dplyr::select(number, features)
                  TB_rgrs <- TB_rgrs %>% dplyr::select(number, var_names)

                  #shuffle
                  set.seed(12345) #set the seed so that it always samples the same
                  TB_rgrs <- TB_rgrs[sample(nrow(TB_rgrs)),]
                  
                  #split dataset into testing and training subset
                  numberOfTrainingSamples <- round(nrow(TB_rgrs) * .7)
                  TB_rgrsslct<- TB_rgrs[1:numberOfTrainingSamples,]
                  
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
               
                  #Build basic xgboost model 
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
                  
                  #Hyperparameter tuning 
                  
                  #1. Tune max depth and min child weight 
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
                  
                  
                  max_depth <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$max_depth
                  min_child_weight <- xgb_tune_cp1$results[xgb_tune_cp1$results$RMSE == max(xgb_tune_cp1$results$RMSE),]$min_child_weight
                  
                  #2. Tune gamma 
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
                  
                  gamma<- xgb_tune_cp2$results[xgb_tune_cp2$results$RMSE == max(xgb_tune_cp2$results$RMSE),]$gamma
                  
                  #3. Tune subsample and colsample by tree
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
                  
                  colsample_bytree<- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$colsample_bytree
                  subsample <- xgb_tune_cp3$results[xgb_tune_cp3$results$RMSE == max(xgb_tune_cp3$results$RMSE),]$subsample
                  
                  #this will determine best parameters...then use those parameters with prediction
                  
                  #4. Train model with found parameters
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
                  #xgb.ggplot.importance(importance_matrix = importance_matrix) +theme_minimal()
          
                  # Predict
                  pred_cp <- predict(model_tuned_cp, xgbMatrixTest)
                  
                  #deviance explained
                  #calculate null deviance of Poisson distribution for the intercept model
                  #https://stats.stackexchange.com/questions/140044/null-deviance-in-glm-r
                  new_label <- test_label +0.001
                  #lf=sum(new_label * log(new_label) - new_label-log(factorial(new_label)))
                  #lnull = sum(new_label * log(mean(new_label)) - mean(new_label) - log(factorial(new_label)))
                  #null_dev = 2*(lf-lnull)
                  
                  #null deviance for NO intercept
                  lnull = sum(-1 - log(factorial(new_label))) 
                  lf = sum(new_label * log(new_label) - new_label - log(factorial(new_label))) 
                  null_dev = 2*(lf-lnull)
                  
                  #calculate residual deviance
                  #important that calc.mean=FALSE if not it will give you mean residual deviance which isnt what standard glms give you. 
                  res_dev = calc.deviance(as.matrix(new_label), as.matrix(pred_cp), family="poisson", calc.mean=FALSE)
                  
                  #deviance explained
                  dev_exp <- (null_dev - res_dev) / null_dev
                  mae <- mean(abs(as.numeric(test_label) - pred_cp))
                  
                  newList <- list("test_label" = test_label, "predictions" = pred_cp, "importance_matrix"= importance_matrix_cp, "dev_exp"= dev_exp, "mae" =mae)

                  print(newList)
}

feature_class_xgboost=function(data, features){

            TB_class <- data
            TB_class$number[TB_class$number >=1] <- 1
            
            TB_class <- TB_class %>% dplyr::select(number, features)
            
            #shuffle
            set.seed(12345) #set the seed so that it always samples the same
            TB_class <- TB_class[sample(nrow(TB_class)),]
            
            #split dataset into testing and training subset
            numberOfTrainingSamples <- round(nrow(TB_class) * .7)
            TB_clsslct<- TB_class[1:numberOfTrainingSamples,]
            
            #training data - this statement is necessary for trian() below
            df_train <- TB_clsslct %>% mutate(number = factor(number, labels=c("Absent", "Present")))
            
            
            label_name="number"
            train_features <- TB_clsslct %>% dplyr::select(-c(number))
            train_features <- mutate_all(train_features, function(x) as.numeric(as.character(x)))
            train_label <- TB_clsslct$number
            
            #testing data 
            TB_test <- TB_class[-(1:numberOfTrainingSamples),]
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
            

            #Build basic xgboost model 
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
            
          
            #Hyperparameter tuning
               #1. Tune max depth and min child weight 
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
          
            max_depth <- xgb_tune1$results[xgb_tune1$results$ROC == max(xgb_tune1$results$ROC),]$max_depth
            min_child_weight <- xgb_tune1$results[xgb_tune1$results$ROC == max(xgb_tune1$results$ROC),]$min_child_weight
            
            #2. Tune gamma
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
            
            gamma<- xgb_tune2$results[xgb_tune2$results$ROC == max(xgb_tune2$results$ROC),]$gamma
            
            
            #3. Tune subsample and colsample by tree 
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
            
            colsample_bytree<- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$colsample_bytree
            subsample <- xgb_tune3$results[xgb_tune3$results$ROC == max(xgb_tune3$results$ROC),]$subsample
            
            #best colsample by tree =0.6, subsample =0.7 (0.6 or 0.9, also)
            
            #4. Train model with found parameters
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
          
            # Predict
            pred <- predict(model_tuned, xgbMatrixTest)
            
            # get & print the classification error
            err <- mean(as.numeric(pred > 0.5) != test_label) #same as mean absolute error -MAE
            #mae = mean(abs(as.numeric(test_label)- as.numeric(pred)))
            
            newList <- list("test_label"=test_label, "predictions" = pred, "importance_matrix"= importance_matrix, "error"= err, "mae"=mae)
            
            print(newList)
}



filter_TB = function(month){

            TB <- read.csv("TB_all_env_with_lag_plus_clor.csv", header=TRUE, row.names=1)
            string <- c("Z_val", "MaxT_val", "MinT_val")
            TB <- TB[,-grep(paste(string, collapse="|"), colnames(TB))]
            
            #remove structural zeros
            TB <-  TB %>% 
              mutate(shore = ifelse(substr(TB$Shore,1,3)=="Eme", "Emerge", 
                                    ifelse(substr(TB$Shore,1,3) =="Man", "Mangrove", 
                                           ifelse(substr(TB$Shore,1,3)=="Str", "Structure", 
                                                  ifelse(substr(TB$Shore, 1,3)=="Ter", "Terrestrial", "Non"))))) %>%
              dplyr::select(-Shore) %>% subset(!shore=="Non") %>% 
              mutate(avgDepth = mean(c(StartDepth, Enddepth)))
            
            #month removal is based on 
            # 1. if within that month the proportion of observed 0s to all other numbers is greater than 90%
            #prop.table(xtabs(~number+month, TB, 2))
            
            TB <- TB[TB$month >4, ]
            
            #remove Zone 
            TB <- TB[TB$Zone !="L",]
            
            #remove some observations from habitat that are noisy 
            #bveg
            #TB <- TB[TB$bveg != "",]
            #TB <- TB[TB$bveg != "Alg",]
            
            #TB <- TB[TB$bottom != "SanStr",]
            TB <- TB[TB$bottom != "Str",]
            TB <- TB[TB$bottom != "Unk",]
            
            
            #TB <- TB[TB$shore != "Emerge",]
            #TB <- TB[TB$shore != "Terrestrial", ]
            
            TB$number[TB$number>50] <- 50
            
            #try to remove salinity below 5 ppt
            TB$Salinity <- as.numeric(as.character(TB$Salinity))
            #TB <- TB %>% subset(salinity > 4)
            
            
            #Set Timing and variable names
            #make different datasets based on haul timing and to minimize the number of NAs present
            #prev autumn and winter >= 3
            #spring/ >= 6
            #summer >=9
            # 1000 = no month breakage
            # 9999 
            
            #about evaluating the influence of seasonal varaibles 
            #first evaluate previous autumn and winter
            #then evaluate previous autumn, winter, and spring
            #then evaluate previous autumn, winter, spring, and summer 
            
            #option for no seasonal evaluation (code 1000)
            
            
            if (month == 4) {
              
              var_names =c("number", "year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                           "aten_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                           "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                           "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                           "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                           "first_spawn_waterT",  "DissolvedO2", "StartDepth",     
                           "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "Clor_val", "allrivers") 
              
              TB <- TB[TB$month >= month,]
              TB <- TB %>% dplyr::select(var_names)
              TB$number <- as.numeric(TB$number)
              TB$Nit_val <- as.numeric(TB$Nit_val)
              
              
            } else if (month == 6) {
              
              var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                            "aten_ceof",            "spring_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                            "spring_MaxT_anom",     "spring_MinT_anom",  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                            "prev_autumn_MinT_anom","spring_RF",           "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",    "DissolvedO2", "StartDepth",    
                            "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                            "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , 
                            "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "Clor_val", "allrivers") 
              
              TB <- TB[TB$month >= month,]
              TB <- TB %>% dplyr::select(var_names)
              TB$number <- as.numeric(TB$number)
              TB$Nit_val <- as.numeric(TB$Nit_val)
              
              
            } else if (month == 9) {
              var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                            "aten_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                            "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                            "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                            "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                            "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                            "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                            "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "Clor_val", "allrivers") 
              
              TB <- TB[TB$month >= month,]
              TB <- TB %>% dplyr::select(var_names)
              TB$number <- as.numeric(TB$number)
              TB$Nit_val <- as.numeric(TB$Nit_val)
              
            } else if (month == 1000) {
              var_names =c("number", "year", "month","salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                           "aten_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                           "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                           "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                           "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                           "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                           "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                           "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "Clor_val", "allrivers") 
              
              TB <- TB %>% dplyr::select(var_names)
              TB$number <- as.numeric(TB$number)
              TB$Nit_val <- as.numeric(TB$Nit_val)
              
            }
            TB
}

filter_IR = function(month){
  
  IR <- read.csv("IR_all_env_with_lag.csv", header=TRUE, row.names=1)
  string <- c("Z_val", "MaxT_val", "MinT_val")
  IR <- IR[,-grep(paste(string, collapse="|"), colnames(IR))]
  
  #remove structural zeros
  IR <-  IR %>% 
    mutate(shore = ifelse(substr(IR$Shore,1,3)=="Eme", "Emerge", 
                          ifelse(substr(IR$Shore,1,3) =="Man", "Mangrove", 
                                 ifelse(substr(IR$Shore,1,3)=="Str", "Structure", 
                                        ifelse(substr(IR$Shore, 1,3)=="Ter", "Terrestrial", "Non"))))) %>%
    dplyr::select(-Shore) %>% subset(!shore=="Non") %>% 
    mutate(avgDepth = mean(c(StartDepth, Enddepth)))
  
  #remove month
  IR <- IR[IR$month > 5, ]
  
  #remove Zone
  IR <- IR[IR$Zone != "F",]
  
  #remove some observations from habitat that are noisy 
  #bveg
  #IR <- IR[IR$bveg != "",]
  #IR <- IR[IR$bveg != "Alg",]
  
  #IR <- IR[IR$bottom != "",]
  IR <- IR[IR$bottom != "Str",]
  #IR <- IR[IR$bottom != "MudStr",]
  #IR <- IR[IR$bottom != "MudSanStr",]
  
  IR$number[IR$number>50] <- 50
  
  #try to remove salinity below 5 ppt
  IR$Salinity <- as.numeric(as.character(IR$Salinity))
  #IR <- IR %>% subset(salinity > 4)
  
  
  #Set Timing and variable names
  #make different datasets based on haul timing and to minimize the number of NAs present
  #prev autumn and winter >= 3
  #spring/ >= 6
  #summer >=9
  # 1000 = no month breakage
  # 9999 
  
  #about evaluating the influence of seasonal varaibles 
  #first evaluate previous autumn and winter
  #then evaluate previous autumn, winter, and spring
  #then evaluate previous autumn, winter, spring, and summer 
  
  #option for no seasonal evaluation (code 1000)
  
  
  if (month == 4) {
    
    var_names =c("number", "year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                 "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                 "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                 "first_spawn_waterT",  "DissolvedO2", "StartDepth",     
                 "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
    
    IR <- IR[IR$month >= month,]
    IR <- IR %>% dplyr::select(var_names)
    IR$number <- as.numeric(IR$number)
    IR$Nit_val <- as.numeric(IR$Nit_val)
    
    
  } else if (month == 6) {
    
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                  "prev_autumn_MinT_anom","spring_RF",           "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",    "DissolvedO2", "StartDepth",    
                  "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                  "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , 
                  "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
    
    IR <- IR[IR$month >= month,]
    IR <- IR %>% dplyr::select(var_names)
    IR$number <- as.numeric(IR$number)
    IR$Nit_val <- as.numeric(IR$Nit_val)
    
    
  } else if (month == 9) {
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                  "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                  "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                  "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                  "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
    
    IR <- IR[IR$month >= month,]
    IR <- IR %>% dplyr::select(var_names)
    IR$number <- as.numeric(IR$number)
    IR$Nit_val <- as.numeric(IR$Nit_val)
    
  } else if (month == 1000) {
    var_names =c("number", "year", "month","salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                 "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                 "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                 "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                 "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                 "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
    
    IR <- IR %>% dplyr::select(var_names)
    IR$number <- as.numeric(IR$number)
    IR$Nit_val <- as.numeric(IR$Nit_val)
    
  }
  IR
}


filter_CH = function(month){
  
            CH <- read.csv("CH_all_env_with_lag.csv", header=TRUE, row.names=1)
            string <- c("Z_val", "MaxT_val", "MinT_val")
            CH <- CH[,-grep(paste(string, collapse="|"), colnames(CH))]
            
            #remove structural zeros
            CH <-  CH %>% 
              mutate(shore = ifelse(substr(CH$Shore,1,3)=="Eme", "Emerge", 
                                    ifelse(substr(CH$Shore,1,3) =="Man", "Mangrove", 
                                           ifelse(substr(CH$Shore,1,3)=="Str", "Structure", 
                                                  ifelse(substr(CH$Shore, 1,3)=="Ter", "Terrestrial", "Non"))))) %>%
              dplyr::select(-Shore) %>% subset(!shore=="Non") %>% 
              mutate(avgDepth = mean(c(StartDepth, Enddepth)))
            
            #remove month
            CH <- CH[CH$month >4, ]
            
            #remove zone
            CH <- CH[CH$Zone != "H", ]
            
            #remove some observations from habitat that are noisy 
            #bveg
            CH <- CH[CH$bveg != "",]
            
            #bottom type 
            CH <- CH[CH$bottom != "Str",]
          
            #shore type
            # no shore type 
            
            CH$number[CH$number>50] <- 50
            
            #try to remove salinity below 5 ppt
            CH$Salinity <- as.numeric(as.character(CH$Salinity))
            
            
            #Set Timing and variable names
            #make different datasets based on haul timing and to minimize the number of NAs present
            #prev autumn and winter >= 3
            #spring/ >= 6
            #summer >=9
            # 1000 = no month breakage
            # 9999 
            
            #about evaluating the influence of seasonal varaibles 
            #first evaluate previous autumn and winter
            #then evaluate previous autumn, winter, and spring
            #then evaluate previous autumn, winter, spring, and summer 
            
            #option for no seasonal evaluation (code 1000)
            
            
            if (month == 4) {
              
              var_names =c("number", "year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                           "ext_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                           "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                           "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                           "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                           "first_spawn_waterT",  "DissolvedO2", "StartDepth",     
                           "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
              
              CH <- CH[CH$month >= month,]
              CH <- CH %>% dplyr::select(var_names)
              CH$number <- as.numeric(CH$number)
              CH$Nit_val <- as.numeric(CH$Nit_val)
              
              
            } else if (month == 6) {
              
              var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                            "ext_ceof",            "spring_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                            "spring_MaxT_anom",     "spring_MinT_anom",  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                            "prev_autumn_MinT_anom","spring_RF",           "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",    "DissolvedO2", "StartDepth",    
                            "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                            "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , 
                            "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
              
              CH <- CH[CH$month >= month,]
              CH <- CH %>% dplyr::select(var_names)
              CH$number <- as.numeric(CH$number)
              CH$Nit_val <- as.numeric(CH$Nit_val)
              
              
            } else if (month == 9) {
              var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                            "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                            "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                            "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                            "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                            "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                            "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                            "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
              
              CH <- CH[CH$month >= month,]
              CH <- CH %>% dplyr::select(var_names)
              CH$number <- as.numeric(CH$number)
              CH$Nit_val <- as.numeric(CH$Nit_val)
              
            } else if (month == 1000) {
              var_names =c("number", "year", "month","salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                           "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                           "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                           "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                           "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                           "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                           "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                           "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro", "allrivers") 
              
              CH <- CH %>% dplyr::select(var_names)
              CH$number <- as.numeric(CH$number)
              CH$Nit_val <- as.numeric(CH$Nit_val)
              
            }
            CH
}

filter_AP = function(month){
  
  AP <- read.csv("AP_all_env_with_lag.csv", header=TRUE, row.names=1)
  string <- c("Z_val", "MaxT_val", "MinT_val")
  AP <- AP[,-grep(paste(string, collapse="|"), colnames(AP))]
  
  #remove structural zeros
  AP <-  AP %>% 
    mutate(shore = ifelse(substr(AP$Shore,1,3)=="Eme", "Emerge", 
                          ifelse(substr(AP$Shore,1,3) =="Man", "Mangrove", 
                                 ifelse(substr(AP$Shore,1,3)=="Str", "Structure", 
                                        ifelse(substr(AP$Shore, 1,3)=="Ter", "Terrestrial", "Non"))))) %>%
    dplyr::select(-Shore) %>% subset(!shore=="Non") %>% 
    mutate(avgDepth = mean(c(StartDepth, Enddepth)))
  
  #remove month
  AP <- AP[AP$month >6, ]
  AP <- AP[AP$month <11,]
  
  #remove zone
  AP <- AP[AP$Zone != "C",]
  AP <- AP[AP$Zone != "D",]
  
  #remove some observations from habitat that are noisy 
  #bveg
  AP <- AP[AP$bveg != "SAVAlg",]
   #bottom type
  AP <- AP[AP$bottom != "Str",]
  #shore type-no shore to remove
 

  AP$number[AP$number>50] <- 50
  
  #try to remove salinity below 5 ppt
  AP$Salinity <- as.numeric(as.character(AP$Salinity))
  
  
  #Set Timing and variable names
  #make different datasets based on haul timing and to minimize the number of NAs present
  #prev autumn and winter >= 3
  #spring/ >= 6
  #summer >=9
  # 1000 = no month breakage
  # 9999 
  
  #about evaluating the influence of seasonal varaibles 
  #first evaluate previous autumn and winter
  #then evaluate previous autumn, winter, and spring
  #then evaluate previous autumn, winter, spring, and summer 
  
  #option for no seasonal evaluation (code 1000)
  
  
  if (month == 4) {
    
    var_names =c("number", "year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                 "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                 "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                 "first_spawn_waterT",  "DissolvedO2", "StartDepth",     
                 "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro") 
    
    AP <- AP[AP$month >= month,]
    AP <- AP %>% dplyr::select(var_names)
    AP$number <- as.numeric(AP$number)
    AP$Nit_val <- as.numeric(AP$Nit_val)
    
    
  } else if (month == 6) {
    
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                  "prev_autumn_MinT_anom","spring_RF",           "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",    "DissolvedO2", "StartDepth",    
                  "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                  "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , 
                  "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro") 
    
    AP <- AP[AP$month >= month,]
    AP <- AP %>% dplyr::select(var_names)
    AP$number <- as.numeric(AP$number)
    AP$Nit_val <- as.numeric(AP$Nit_val)
    
    
  } else if (month == 9) {
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                  "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                  "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                  "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                  "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro") 
    
    AP <- AP[AP$month >= month,]
    AP <- AP %>% dplyr::select(var_names)
    AP$number <- as.numeric(AP$number)
    AP$Nit_val <- as.numeric(AP$Nit_val)
    
  } else if (month == 1000) {
    var_names =c("number", "year", "month","salinity", "temperature", "riv_flow", "Nit_val", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                 "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                 "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                 "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL",  "atspawn_salinity",     "atspawn_waterT",      "first_spawn_salinity" ,
                 "first_spawn_waterT",   "second_spawn_salinity","second_spawn_waterT" , "third_spawn_salinity", "third_spawn_waterT",   
                 "atspawn_nitro",        "avg_last2_nitro",      "avg_last3_nitro") 
    
    AP <- AP %>% dplyr::select(var_names)
    AP$number <- as.numeric(AP$number)
    AP$Nit_val <- as.numeric(AP$Nit_val)
    
  }
  AP
}

filter_CK = function(month){
  
  CK <- read.csv("CK_all_env_with_lag.csv", header=TRUE, row.names=1)
  string <- c("Z_val", "MaxT_val", "MinT_val")
  CK <- CK[,-grep(paste(string, collapse="|"), colnames(CK))]
  
  #remove structural zeros
  CK <-  CK %>% 
    mutate(shore = ifelse(substr(CK$Shore,1,3)=="Eme", "Emerge", 
                          ifelse(substr(CK$Shore,1,3) =="Man", "Mangrove", 
                                 ifelse(substr(CK$Shore,1,3)=="Str", "Structure", 
                                        ifelse(substr(CK$Shore, 1,3)=="Ter", "Terrestrial", "Non"))))) %>%
    dplyr::select(-Shore) %>% subset(!shore=="Non") %>% 
    mutate(avgDepth = mean(c(StartDepth, Enddepth)))
  
  #remove month
  CK <- CK[CK$month >5, ]
  
  #remove zone
  CK <- CK[CK$Zone != "C",]
  CK <- CK[CK$Zone != "F",]
  
  #remove some observations from habitat that are noisy 
  #bveg
  CK <- CK[CK$bveg != "",]
  CK <- CK[CK$bveg != "Alg",]
  #bottom
  CK <- CK[CK$bottom != " ",]
  CK <- CK[CK$bottom != "Str",]
  CK <- CK[CK$bottom != "MudUnk",]
  #shore type
  CK <- CK[CK$shore != "Mangrove",]
  CK <- CK[CK$shore != "Terrestrial", ]
  
 
  CK$number[CK$number>50] <- 50
  
  #try to remove salinity below 5 ppt
  CK$Salinity <- as.numeric(as.character(CK$Salinity))
  
  
  #Set Timing and variable names
  #make different datasets based on haul timing and to minimize the number of NAs present
  #prev autumn and winter >= 3
  #spring/ >= 6
  #summer >=9
  # 1000 = no month breakage
  # 9999 
  
  #about evaluating the influence of seasonal varaibles 
  #first evaluate previous autumn and winter
  #then evaluate previous autumn, winter, and spring
  #then evaluate previous autumn, winter, spring, and summer 
  
  #option for no seasonal evaluation (code 1000)
  
  
  if (month == 4) {
    
    var_names =c("number", "year", "month", "salinity", "temperature", "riv_flow", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                 "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                 "winter_dis_ALL",       "prev_autumn_dis_ALL",  
                   "DissolvedO2", "StartDepth", "allrivers")     
                              
    
    CK <- CK[CK$month >= month,]
    CK <- CK %>% dplyr::select(var_names)
    CK$number <- as.numeric(CK$number)
    
    
  } else if (month == 6) {
    
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                  "prev_autumn_MinT_anom","spring_RF",           "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",    "DissolvedO2", "StartDepth",    
                  "winter_dis_ALL",       "prev_autumn_dis_ALL", "allrivers") 
    
    CK <- CK[CK$month >= month,]
    CK <- CK %>% dplyr::select(var_names)
    CK$number <- as.numeric(CK$number)
    
    
  } else if (month == 9) {
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                  "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                  "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL", "allrivers") 
                  
                  
    
    CK <- CK[CK$month >= month,]
    CK <- CK %>% dplyr::select(var_names)
    CK$number <- as.numeric(CK$number)
    
  } else if (month == 1000) {
    var_names =c("number", "year", "month","salinity", "temperature", "riv_flow",  "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                 "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                 "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                 "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL", "allrivers")  
                  
                
    
    CK <- CK %>% dplyr::select(var_names)
    CK$number <- as.numeric(CK$number)
    
  }
  CK
}

filter_JX = function(month){
  
  JX <- read.csv("JX_all_env_with_lag.csv", header=TRUE, row.names=1)
  string <- c("Z_val", "MaxT_val", "MinT_val")
  JX <- JX[,-grep(paste(string, collapse="|"), colnames(JX))]
  
  #remove structural zeros
  JX <-  JX %>% 
    mutate(shore = ifelse(substr(JX$Shore,1,3)=="Eme", "Emerge", 
                          ifelse(substr(JX$Shore,1,3) =="Man", "Mangrove", 
                                 ifelse(substr(JX$Shore,1,3)=="Str", "Structure", 
                                        ifelse(substr(JX$Shore, 1,3)=="Ter", "Terrestrial", "Non"))))) %>%
    dplyr::select(-Shore) %>% subset(!shore=="Non") %>% 
    mutate(avgDepth = mean(c(StartDepth, Enddepth)))
  
  #remove month
  JX <- JX[JX$month >5, ]
  JX <- JX[JX$month <11, ]
  
  #remove zone
  JX <- JX[JX$Zone != "E", ]
  JX <- JX[JX$Zone != "F", ]
  #remove some observations from habitat that are noisy 
  
  #bveg
  JX <- JX[JX$bveg != "",]
  JX <- JX[JX$bveg != "Alg",]
  JX <- JX[JX$bveg != "SAVAlg",]
  JX <- JX[JX$bveg != "SAV",]
  JX <- JX[JX$bveg != "SAVNon",]
  
  #bottom
  JX <- JX[JX$bottom != " ",]
  JX <- JX[JX$bottom != "San",]
  JX <- JX[JX$bottom != "SanStr",]
  JX <- JX[JX$bottom != "Str",]
  JX <- JX[JX$bottom != "Unk",]
  
  #shore
  JX <- JX[JX$shore != "Terrestrial", ]
  
  #JX$number[JX$number>75] <- 75
  
  #try to remove salinity below 5 ppt
  JX$Salinity <- as.numeric(as.character(JX$Salinity))
  
  
  #Set Timing and variable names
  #make different datasets based on haul timing and to minimize the number of NAs present
  #prev autumn and winter >= 3
  #spring/ >= 6
  #summer >=9
  # 1000 = no month breakage
  # 9999 
  
  #about evaluating the influence of seasonal varaibles 
  #first evaluate previous autumn and winter
  #then evaluate previous autumn, winter, and spring
  #then evaluate previous autumn, winter, spring, and summer 
  
  #option for no seasonal evaluation (code 1000)
  
  
  if (month == 4) {
    
    var_names =c("number", "year", "month", "salinity", "temperature", "riv_flow", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",           "winter_dis" ,          "prev_autumn_dis",            
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                 "prev_autumn_MinT_anom", "winter_RF",            "prev_autumn_RF",             
                 "winter_dis_ALL",       "prev_autumn_dis_ALL",  
                 "DissolvedO2", "StartDepth", "allrivers")     
    
    
    JX <- JX[JX$month >= month,]
    JX <- JX %>% dplyr::select(var_names)
    JX$number <- as.numeric(JX$number)
    
    
  } else if (month == 6) {
    
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom",
                  "prev_autumn_MinT_anom","spring_RF",           "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",    "DissolvedO2", "StartDepth",    
                  "winter_dis_ALL",       "prev_autumn_dis_ALL", "allrivers") 
    
    JX <- JX[JX$month >= month,]
    JX <- JX %>% dplyr::select(var_names)
    JX$number <- as.numeric(JX$number)
    
    
  } else if (month == 9) {
    var_names =c( "number","year", "month", "salinity", "temperature", "riv_flow", "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                  "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                  "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                  "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                  "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                  "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL", "allrivers") 
    
    
    
    JX <- JX[JX$month >= month,]
    JX <- JX %>% dplyr::select(var_names)
    JX$number <- as.numeric(JX$number)
    
  } else if (month == 1000) {
    var_names =c("number", "year", "month","salinity", "temperature", "riv_flow",  "Z_anom", "MaxT_anom", "MinT_anom", "TotalMonthlyRF",       
                 "ext_ceof",            "spring_dis",           "summer_dis",           "winter_dis" ,          "prev_autumn_dis",      "spring_Z_anom",        
                 "spring_MaxT_anom",     "spring_MinT_anom",     "summer_Z_anom" ,       "summer_MaxT_anom",     "summer_MinT_anom",     
                 "winter_Z_anom",        "winter_MaxT_anom",     "winter_MinT_anom" ,    "prev_autumn_Z_anom",   "prev_autumn_MaxT_anom","DissolvedO2", "StartDepth", 
                 "prev_autumn_MinT_anom","spring_RF",            "summer_RF",            "winter_RF",            "prev_autumn_RF",       "spring_dis_ALL",       
                 "summer_dis_ALL" ,      "winter_dis_ALL",       "prev_autumn_dis_ALL", "allrivers")  
    
    
    
    JX <- JX %>% dplyr::select(var_names)
    JX$number <- as.numeric(JX$number)
    
  }
  JX
}


write_out_pos4= function(TB_ppos4, name){
  write.csv(TB_ppos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos4_labels.csv", sep=""), sep="/"))
            write.csv(TB_ppos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos4_importance.csv", sep=""), sep="/"))
                      write.csv(TB_ppos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos4_preds.csv", sep=""), sep="/"))
                                write.csv(TB_ppos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos4_dev.csv", sep=""), sep="/"))
}

write_out_pos6= function(TB_ppos6, name) {
  write.csv(TB_ppos6$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos6_labels.csv", sep=""), sep="/"))
            write.csv(TB_ppos6$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos6_importance.csv", sep=""), sep="/"))
                      write.csv(TB_ppos6$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos6_preds.csv", sep=""), sep="/"))
                                write.csv(TB_ppos6$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos6_dev.csv", sep=""), sep="/"))
}

write_out_pos9= function(TB_ppos9, name) {
  write.csv(TB_ppos9$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos9_labels.csv", sep=""), sep="/"))
            write.csv(TB_ppos9$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos9_importance.csv", sep=""), sep="/"))
                      write.csv(TB_ppos9$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos9_preds.csv", sep=""), sep="/"))
                                write.csv(TB_ppos9$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_ppos9_dev.csv", sep=""), sep="/"))
}


write_out_clas4 = function(TB_class4, name){
  write.csv(TB_class4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas4_labels.csv", sep=""), sep="/"))
  write.csv(TB_class4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas4_importance.csv", sep=""), sep="/"))
  write.csv(TB_class4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas4_preds.csv", sep=""), sep="/"))
  write.csv(TB_class4$error, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas4_error.csv", sep=""), sep="/"))
}

write_out_clas6 = function(TB_class6, name){
  write.csv(TB_class6$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas6_labels.csv", sep=""), sep="/"))
            write.csv(TB_class6$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas6_importance.csv", sep=""), sep="/"))
                      write.csv(TB_class6$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas6_preds.csv", sep=""), sep="/"))
                                write.csv(TB_class6$error, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas6_error.csv", sep=""), sep="/"))
}

write_out_clas9 = function(TB_class9, name){
  write.csv(TB_class9$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas9_labels.csv", sep=""), sep="/"))
            write.csv(TB_class9$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas9_importance.csv", sep=""), sep="/"))
                      write.csv(TB_class9$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas9_preds.csv", sep=""), sep="/"))
                                write.csv(TB_class9$error, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(name), "_clas9_error.csv", sep=""), sep="/"))
}



#runs manual recursive feature elimination (addition)
#out puts the results and features from that
# runs the best model again through the xgboost routine
# out puts the importance matrix, predictions and deviance explained from that
entire_routine_poisson = function(data, dataname,  dopospos=FALSE, complete.cases=FALSE, is.ckjxir=FALSE){
  
  if (dopospos == FALSE){
    if (complete.cases == FALSE) {
      if (is.ckjxir==FALSE) {
        
        TBpos4 <- manualrfe_poisson(data, dopospos=FALSE, complete.cases=FALSE, is.ckjxir=FALSE)
        write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_results.csv", sep=""), sep="/")) 
        write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pos_features.csv", sep=""), sep="/"))
        TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
        TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
        
        TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=FALSE, complete.cases=FALSE, is.ckjxir=FALSE)
        write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_labels.csv", sep=""), sep="/"))
        write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_importance.csv", sep=""), sep="/"))
        write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_preds.csv", sep=""), sep="/"))
        write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_dev.csv", sep=""), sep="/"))
        
      }
        else if (is.ckjxir==TRUE) {
          TBpos4 <- manualrfe_poisson(data, dopospos=FALSE, complete.cases=FALSE, is.ckjxir=TRUE)
          write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_results.csv", sep=""), sep="/")) 
          write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pos_features.csv", sep=""), sep="/"))
          TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
          TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
          
          TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=FALSE, complete.cases=FALSE, is.ckjxir=TRUE)
          write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_labels.csv", sep=""), sep="/"))
          write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_importance.csv", sep=""), sep="/"))
          write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_preds.csv", sep=""), sep="/"))
          write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_dev.csv", sep=""), sep="/"))
          
        }
      }
    
    else if (complete.cases == TRUE){
      if (is.ckjxir==FALSE) {

      TBpos4 <- manualrfe_poisson(data, dopospos=FALSE, complete.cases=TRUE, is.ckjxir=FALSE)
      write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_results_CC.csv", sep=""), sep="/")) 
      write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pos_features_CC.csv", sep=""), sep="/"))
      TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
      TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
      
      TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=FALSE, complete.cases=TRUE, is.ckjxir=FALSE)
      
      write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_labels_CC.csv", sep=""), sep="/"))
      write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_importance_CC.csv", sep=""), sep="/"))
      write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_preds_CC.csv", sep=""), sep="/"))
      write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_dev_CC.csv", sep=""), sep="/"))
      
      }
      else if (is.ckjxir==TRUE) {
        TBpos4 <- manualrfe_poisson(data, dopospos=FALSE, complete.cases=TRUE, is.ckjxir=TRUE)
        write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_results_CC.csv", sep=""), sep="/")) 
        write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pos_features_CC.csv", sep=""), sep="/"))
        TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
        TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
        
        TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=FALSE, complete.cases=TRUE, is.ckjxir=TRUE)
        
        write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_labels_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_importance_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_preds_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pos_dev_CC.csv", sep=""), sep="/"))
        
      }
    }
  }
  else if (dopospos == TRUE){
    if (complete.cases == TRUE) {
      if (is.ckjxir==FALSE) {
        TBpos4 <- manualrfe_poisson(data, dopospos=TRUE, complete.cases=TRUE, is.ckjxir=FALSE)
        write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_results_CC.csv", sep=""), sep="/")) 
        write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pospos_features_CC.csv", sep=""), sep="/"))
        TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
        TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
        
        TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=TRUE, complete.cases=TRUE, is.ckjxir=FALSE)
        write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_labels_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_importance_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_preds_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_dev_CC.csv", sep=""), sep="/"))
        
      }
        else if (is.ckjxir==TRUE) {
        TBpos4 <- manualrfe_poisson(data, dopospos=TRUE, complete.cases=TRUE, is.ckjxir=TRUE)
        write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_results_CC.csv", sep=""), sep="/")) 
        write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pospos_features_CC.csv", sep=""), sep="/"))
        TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
        TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
        
        TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=TRUE, complete.cases=TRUE, is.ckjxir=TRUE)
        write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_labels_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_importance_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_preds_CC.csv", sep=""), sep="/"))
        write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_dev_CC.csv", sep=""), sep="/"))
        
        } 
    }
    else if (complete.cases == FALSE){
      if (is.ckjxir==FALSE) {
        TBpos4 <- manualrfe_poisson(data, dopospos=TRUE, complete.cases=FALSE, is.ckjxir = FALSE)
        write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_results.csv", sep=""), sep="/")) 
        write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pospos_features.csv", sep=""), sep="/"))
        TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
        TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
        
        TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=TRUE, complete.cases=FALSE, is.ckjxir=FALSE)
        write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_labels.csv", sep=""), sep="/"))
        write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_importance.csv", sep=""), sep="/"))
        write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_preds.csv", sep=""), sep="/"))
        write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_dev.csv", sep=""), sep="/"))
        
      }
       else if (is.ckjxir==TRUE){
         TBpos4 <- manualrfe_poisson(data, dopospos=TRUE, complete.cases=FALSE, is.ckjxir = TRUE)
         write.csv(TBpos4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_results.csv", sep=""), sep="/")) 
         write.csv(TBpos4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname),"_pospos_features.csv", sep=""), sep="/"))
         TBres_pos4 <- TBpos4$results[TBpos4$results[,5] == min(TBpos4$results[,5], na.rm=T),]
         TBfeat_pos4 <- TBpos4$features[1:max(TBres_pos4, na.rm=T)]
         
         TB_pos4 <- feature_poisson_xgboost(data, TBfeat_pos4, dopospos=TRUE, compelte.cases=FALSE, is.ckjxir=TRUE)
         write.csv(TB_pos4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_labels.csv", sep=""), sep="/"))
         write.csv(TB_pos4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_importance.csv", sep=""), sep="/"))
         write.csv(TB_pos4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_preds.csv", sep=""), sep="/"))
         write.csv(TB_pos4$dev_exp, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_pospos_dev.csv", sep=""), sep="/"))
         
       }
    }
  }
  
}

  

entire_routine_class = function(data, dataname, complete.cases=FALSE){
  
  if (complete.cases==FALSE) {
    
    TBclas4 <- manualrfe_class(data, full=TRUE)
    TBres_clas4 <- TBclas4$results[TBclas4$results[,1] == min(TBclas4$results[,1], na.rm=T),][,3]
    TBfeat_clas4 <- TBclas4$features[1:max(TBres_clas4, na.rm=T)]
    
    TB_class4 <- feature_class_xgboost(data,TBfeat_clas4)
    
    write.csv(TBclas4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_results.csv", sep=""), sep="/"))
    write.csv(TBclas4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_features.csv", sep=""), sep="/"))
    write.csv(TB_class4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_labels.csv", sep=""), sep="/"))
    write.csv(TB_class4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_importance.csv", sep=""), sep="/"))
    write.csv(TB_class4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_preds.csv", sep=""), sep="/"))
    write.csv(TB_class4$error, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_error.csv", sep=""), sep="/"))
    
  }

 else if (complete.cases ==TRUE){
  
  TBclas4 <- manualrfe_class(data, full=FALSE)
  TBres_clas4 <- TBclas4$results[TBclas4$results[,1] == min(TBclas4$results[,1], na.rm=T),][,3]
  TBfeat_clas4 <- TBclas4$features[1:max(TBres_clas4, na.rm=T)]
  
  TB_class4 <- feature_class_xgboost(data,TBfeat_clas4)
  
  write.csv(TBclas4$results, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_results_CC.csv", sep=""), sep="/"))
  write.csv(TBclas4$features, paste(out, "Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_features_CC.csv", sep=""), sep="/"))
  write.csv(TB_class4$test_label, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_labels_CC.csv", sep=""), sep="/"))
  write.csv(TB_class4$importance_matrix, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_importance_CC.csv", sep=""), sep="/"))
  write.csv(TB_class4$predictions, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_preds_CC.csv", sep=""), sep="/"))
  write.csv(TB_class4$error, paste(out,"Seatrout_ENV_Chapter2/xgboost_results", paste(as.character(dataname), "_clas_error_CC.csv", sep=""), sep="/"))
}
}

