### train_func.R ###############
library('caret');
library('PRROC');
library('pROC');
library('forestplot');

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

####################################################################
# FUNCTION: forset.func
# Use the glm model result to create forest plots
# INPUT: 
# data: include lnOR, p value, 95%ci
# var: variable names
# filename: include the path for output
# OUTPUT: png
####################################################################
forset.func <- function(data, var, filename) {
  forest.data <- tibble(
    mean = as.numeric(data[,1]),
    lower = as.numeric(data[,3]),
    upper = as.numeric(data[,4]),
    OR = as.character(round(as.numeric(data[,1]),3)),
    Features = var,
    Pvalue = as.character(round(data[,2], 3))
  );
  
  header <- tibble(
    Features = c('Features'),
    OR = c('ln(OR)'),
    Pvalue = c('P value')
  );
  
  plot.df <- bind_rows(header, forest.data);
  
  png(file = paste0(filename,'.png'), width = 300, height = 10 * nrow(forest.data) + 5, units='mm', res = 300);
  print(
    plot.df %>%
      forestplot(
        labeltext = c(Features, OR, Pvalue),
        lineheight = unit(1,'cm'),
        colgap = unit(3,'mm'),
        lwd.ci = 2,
        boxsize = 0.2,
        xlab = c('ln(OR)'),
        ci.vertices = TRUE,
        clip = c(-15, 15),
        graphwidth = unit(9,'cm')
      )
  );
  dev.off();
  return();
}

