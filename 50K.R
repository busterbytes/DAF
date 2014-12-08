library(FNN)
training = read.csv("50Ktrain.csv")
test = read.csv("50Ktest.csv")
cl = read.csv("cl.csv")
cl = cl[,1]
knn(training, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)