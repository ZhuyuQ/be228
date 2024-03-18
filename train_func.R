### train_func.R ###############
library('caret');
library('PRROC');
library('pROC');

####################################################################
# FUNCTION:train.func
# train and test glm models and provide evaluation metrics
# INPUT: formula, data.train, data.test, filename
# OUTPUT: df of prediction result using test data
####################################################################

train.func <- function(formula, data.train, data.test, filename) {
  # train model
  set.seed(55555);
  fitControl <- trainControl(
    method = 'repeatedcv',
    number = 5,
    repeats = 10,
    summaryFunction = twoClassSummary, 
    classProbs = TRUE,
    savePredictions = T
  );
  trainfit <- train(
    formula,
    data = data.train, 
    method = 'glm',
    preProcess = c('center', 'scale'),
    trControl = fitControl,
    metric = 'ROC'
  );
  # get confusion matrix
  conf.train <- confusionMatrix(data = trainfit$pred$pred, reference = trainfit$pred$obs);
  print(conf.train);
  
  mod.inf <- glm(formula, data = data.train, family = binomial);
  
  train.result <- trainfit$pred[,1:4];
  # plot roc prc
  curve.func(pred.data = train.result, filename = paste0(filename, '_train'));
  
  # test the model
  pred.value <- cbind(
    predict(trainfit, data.test, type = 'prob'),
    predict(trainfit, data.test, type = 'raw'),
    data.test$cabingflg
  );
  colnames(pred.value)[3:4] <- c('pred', 'obs');
  
  # plot roc prc
  curve.func(pred.data = pred.value, filename = paste0(filename, '_test'));
  # get confusion matrix
  conf.test <- confusionMatrix(data = pred.value$pred, reference = pred.value$obs);
  print(conf.test);
  
  result <- list(pred.value,trainfit, mod.inf);
  return(result);
}

####################################################################
# FUNCTION: curve.func
# use prediction result to create roc curve and pr curve
# INPUT: pred.data, filename 
# OUTPUT: pdf of roc and prc
####################################################################
curve.func <- function(pred.data, filename) {
  pred.data$pred <- factor(pred.data$pred, levels = c('No', 'Yes'));
  fg <- pred.data[which(pred.data$obs == 'Yes'),]$Yes;
  bg <- pred.data[which(pred.data$obs == 'No'),]$Yes;
  
  # ROC Curve    
  roc.curve_roc <- PRROC::roc.curve(scores.class0 = fg, scores.class1 = bg, curve=TRUE);
  pdf(paste0(filename, '_roc.pdf'));
  plot(roc.curve_roc);
  dev.off();
  roc <- pROC::roc(pred.data$obs, pred.data$Yes);
  
  # PR Curve
  pr.curve_pr <- PRROC::pr.curve(scores.class0 = fg, scores.class1 = bg, curve=TRUE);
  pdf(paste0(filename, '_prc.pdf'));
  plot(pr.curve_pr);
  dev.off();
  
  return();
}
