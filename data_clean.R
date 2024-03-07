### data_clean.R ###############
library(ggplot2);
library(tidyverse);
library(ggpubr);
library(DescTools);
library(caret);

work.dir <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/code/';
path.in <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/data/';
path.out <- '/Users/zqiu/Documents/graduate_school/2024WINTER/BE223B/output/';

# read data
test <- load(file = '/Users/zqiu/Documents/graduate_school/2024WINTER/BEM228/final/data/NSDUH_2022.Rdata')

for (nm in test) {
  x <- get(nm)
}

# original size: 59069*2605
write.table(
  x, 
  file = paste0(path.in, 'nmhss_2022.txt'),
  sep = "\t", 
  row.names = T
);

# subset asian gt&eq 18
x.asian <- x[which(x$NEWRACE2 == 5),];
x.asian.18 <- x.asian[x.asian$AGE3 >3,]; #  2521 2605

# select all variables for analysis
final.x <- x.asian.18[,c('QUESTID2', 'CATAG3', 'eduhighcat','irsex', 'cigever', 'cocever', 'cbdhmpevr', 'irimpgout', 'DSTHOP30', 'IRDSTNRV30', 'cabingflg')]

table(final.x$cabingflg); # imbalanced

# output data
write.table(
  final.x, 
  file = paste0(path.in, 'nmhss_2022_asian18final.txt'),
  sep = "\t", 
  row.names = T
);
