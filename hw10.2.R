library(randomForest)
heart <- read.csv('shrooms.csv',header=FALSE)
heart$levels <- as.factor(heart$V1)
heartforest.allvals <- randomForest(formula=levels~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23,data=heart,type='classification',mtry=2)
#thisfitstoalllevels
#IgottheCCMbytyping
heartforest.allvals
