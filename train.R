### train.R ###############
library(ggplot2);
library(tidyverse);
library(caret);
library(PRROC);

work.dir <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/be228/';
path.in <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/data/';
path.out <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/output/';

source('/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/be228/train_func.R');

# read data
data.train <- read.delim(
  file = paste0(path.in, '2022_asian18final_binge_train.txt'),
  header = TRUE,
  as.is = TRUE
);
data.test <- read.delim(
  file = paste0(path.in, '2022_asian18final_binge_test.txt'),
  header = TRUE,
  as.is = TRUE
);

# code 
data.train$cabingflg <- factor(data.train$cabingflg, labels = c('No', 'Yes'));
data.test$cabingflg <- factor(data.test$cabingflg, labels = c('No', 'Yes'));

data.train[, 2:(ncol(data.train)-1)] <- apply(data.train[, 2:(ncol(data.train)-1)], 2, factor);
data.test[, 2:(ncol(data.test)-1)] <- apply(data.test[, 2:(ncol(data.test)-1)], 2, factor);

# create balanced data
# smote.train <- SMOTE(
#   target = data.train[, 'cabingflg'],
#   X = data.train[, -which(colnames(data.train) == 'cabingflg')]
#   )$data;
# colnames(smote.train)[ncol(smote.train)] <- 'cabingflg';

# define models
model1 <- as.formula('cabingflg ~ CATAG3 + eduhighcat + irsex');
model2 <- as.formula('cabingflg ~ CATAG3 + eduhighcat + irsex + cigever + cocever + cbdhmpevr');
model3 <- as.formula('cabingflg ~ CATAG3 + eduhighcat + irsex + cigever + cocever + cbdhmpevr + irimpgout + DSTHOP30 + IRDSTNRV30');

# do roc comparisons
mod1.pred <- train.func(
  formula = model1,
  data.train = data.train,
  data.test = data.test,
  filename = paste0(path.out, 'mod1')
);
mod2.pred <- train.func(
  formula = model2,
  data.train = data.train,
  data.test = data.test,
  filename = paste0(path.out, 'mod2')
);
mod3.pred <- train.func(
  formula = model3,
  data.train = data.train,
  data.test = data.test,
  filename = paste0(path.out, 'mod3')
);

# compare model auroc&auprc
p.roc12 <- roc.test(
  roc1 = pROC::roc(mod1.pred[[1]]$obs, mod1.pred[[1]]$Yes),
  roc2 = pROC::roc(mod2.pred[[1]]$obs, mod2.pred[[1]]$Yes)
    ); # p-value = 2.631e-10

p.roc13 <- roc.test(
  roc1 = pROC::roc(mod1.pred[[1]]$obs, mod1.pred[[1]]$Yes),
  roc2 = pROC::roc(mod3.pred[[1]]$obs, mod3.pred[[1]]$Yes)
); #2.922e-09

p.roc23 <- roc.test(
  roc1 = pROC::roc(mod2.pred[[1]]$obs, mod2.pred[[1]]$Yes),
  roc2 = pROC::roc(mod3.pred[[1]]$obs, mod3.pred[[1]]$Yes)
); # 0.3498

# show the effect size in the forest plot
inf.mod1 <- cbind(
  as.data.frame(coef(summary(mod1.pred[[3]]))[, c(1,4)]),
  as.data.frame(confint(mod1.pred[[3]]))
)[-1,];
inf.mod2 <- cbind(
  as.data.frame(coef(summary(mod2.pred[[3]]))[, c(1,4)]),
  as.data.frame(confint(mod2.pred[[3]]))
)[-1,];
inf.mod3 <- cbind(
  as.data.frame(coef(summary(mod3.pred[[3]]))[, c(1,4)]),
  as.data.frame(confint(mod3.pred[[3]]))
)[-1,];

# create forest plot
forset.func(
  data = inf.mod1,
  var = c(
    '26-34 vs 18-25',
    '35-49 vs 18-25',
    '>=50 vs 18-25',
    'High school grad vs less high school',
    'Some coll/Assoc Dg vs less high school',
    'College grad vs less high school',
    'Female vs Male'
    ),
  filename = paste0(path.out, 'forest_mod1')
);

forset.func(
  data = inf.mod2,
  var = c(
    '26-34 vs 18-25',
    '35-49 vs 18-25',
    '>=50 vs 18-25',
    'High school grad vs Less high school',
    'Some coll/Assoc Dg vs Less high school',
    'College grad vs Less high school',
    'Female vs Male',
    'No smoke vs Smoke',
    'No used cocaine vs Used Cocaine',
    'No used CBD/HEMP vs Used CBD/HEMP'
  ),
  filename = paste0(path.out, 'forest_mod2')
);

forset.func(
  data = inf.mod3,
  var = c(
    '26-34 vs 18-25',
    '35-49 vs 18-25',
    '>=50 vs 18-25',
    'High school grad vs Less high school',
    'Some coll/Assoc Dg vs Less high school',
    'College grad vs Less high school',
    'Female vs Male',
    'No smoke vs Smoke',
    'No used cocaine vs Used Cocaine',
    'No used CBD/HEMP vs Used CBD/HEMP',
    'Mild difficulty going out vs No difficulty',
    'Moderate difficulty going out vs No difficulty',
    'Severe difficulty going out vs No difficulty',
    "Don't leave house on your own vs No difficulty",
    'Skip vs No difficulty',
    'Feel hopeless most of the time vs All of the time',
    'Feel hopeless some of the time vs All of the time',
    'Feel hopeless a little of the time vs All of the time',
    'Feel hopeless none of the time vs All of the time',
    'Feel nerves most of the time vs All of the time',
    'Feel nerves some of the time vs All of the time',
    'Feel nerves a little of the time vs All of the time',
    'Feel nerves none of the time vs All of the time'
  ),
  filename = paste0(path.out, 'forest_mod3')
);
