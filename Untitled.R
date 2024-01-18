
setwd("/Users/saiperenyilmaz/R_WD")
pisa <-read.csv("pisa.csv", header=TRUE)
nlsy <-read.csv("nlsy.csv", header=TRUE)


head(nlsy)
library(tidyr)

nlsy_wide_reshape <-reshape(data=nlsy,
                            v.names = c("weight", "health"),
                            timevar = "year",
                            idvar = c("id","sex","ethnicity"),
                            direction = "wide")

weight <- nlsy[,1:5]
nlsy_wide_reshape2 <-dcast(data=weight,
                           formula = id + sex + ethnicity ~ year,
                           value.var = "weight")

nlsy_wide_tidyr <-spread(data=weight,
                           key=year,
                         value=weight)
colnames(nlsy_wide_tidyr)[4:6] <-paste0("weight", colnames(nlsy_wide_tidyr)[4:6])
