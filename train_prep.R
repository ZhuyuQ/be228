### train_prer.R ###############
library(ggplot2);
library(tidyverse);
library(caret);

work.dir <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/be228/';
path.in <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/data/';
path.out <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BE223B/final/output/';

# read data
data <- read.delim(
  file = paste0(path.in, '2022_asian18final_binge.txt'),
  header = TRUE,
  as.is = TRUE
);

# check all variables with missing values
table(data$CATAG3); # 808:555:759:399 
table(data$eduhighcat); # 155:334:510:1522 
table(data$irsex); #1190:1331
table(data$cigever); #781:1740
table(data$cocever); #128:2393
table(data$cbdhmpevr); #511:2006:4 - 94 = don't know
table(data$irimpgout); # 1277:374:168:70:54:578
table(data$DSTHOP30); # 26:75:319:557:1482:(9:4:49) - 94,97,98 need to be excluded
table(data$IRDSTNRV30); # 57:154:610:859:841
table(data$cabingflg); #2154:351:2(11 will be combined with 1 binge):14(- 85need to be excluded)

# remove data with missing value
data.clean <- data[which(data$cbdhmpevr != 94 & !data$DSTHOP30 %in% c(94, 97, 98) & data$cabingflg != 85),]; #1962
data.clean$cabingflg[11 == data.clean$cabingflg] <- 1;

write.table(
  data.clean, 
  file = paste0(path.in, '2022_asian18final_binge_naremove.txt'),
  sep = "\t", 
  row.names = T
);

# split the data into training and validation based on the outcome variable: 0.8 train, 0.2 test
set.seed(55555);
part.data <- createDataPartition(
  data.clean[, 'cabingflg'],
  p = 0.8
);

data.train <- data.clean[part.data[[1]],];
data.test <- data.clean[which(!data.clean$QUESTID2 %in% data.train$QUESTID2),];
write.table(
  data.train, 
  file = paste0(path.in, '2022_asian18final_binge_train.txt'),
  sep = "\t", 
  row.names = T
);
write.table(
  data.test, 
  file = paste0(path.in, '2022_asian18final_binge_test.txt'),
  sep = "\t", 
  row.names = T
);
