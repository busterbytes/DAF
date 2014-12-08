training = read.csv("training.csv")
test = read.csv("test.csv")
validate = read.csv("validation.csv")
correctnessFrame = data.frame(correct=rep(NA,), constant=rep(NA,), numberOfTests=rep(NA,),stringsAsFactors=FALSE)
a <- rep(1, times = 9)
b <- 1

total_tests <- 0
correct_tests <- 0
row <- 0

for (e in 0:3)
{
	
	reg_constant <- 10^(-e)
	total_tests <- 0
	correct_tests <- 0
	a <- rep(1, times = 9)
	b <- 1
	for (i in 1:50 ) 
	{
		step_length <- 1/(.5*(e+1) + 1)

		for (j in 1:100)
		{
			random <- sample(1:499, 1)
			y_s <- training[random, 2]
			x_s <- c(training[random, 3],training[random, 4],training[random, 5],training[random, 6],training[random, 7],training[random, 8],training[random, 9],training[random, 10],training[random,11])

			inner_product = sum(a*x_s)

			if (sign(inner_product + b) == 1 && y_s == 1){
				correct_tests<-correct_tests+1
			}
			if (sign(inner_product + b) == -1 && y_s == -1){
				correct_tests<-correct_tests+1
			}

			total_tests <- total_tests+1

			if (total_tests%%10 == 0){
				percent_correct=correct_tests/total_tests
				row=row+1
				correctnessFrame[row,]<-c(percent_correct,reg_constant,total_tests)
				
			}			

			if (y_s * (inner_product + b) >= 1)
			{
				a = a -step_length * reg_constant * a

			}
			else 
			{
				a = a - step_length*(reg_constant*a - y_s*x_s)
				b = b + step_length * y_s
			}
		}
	}
}

total_validations <- 0
correct_validations <- 0
reg_constant = .01
for (i in 1:300)
{
	random <- sample(1:99, 1)
	y_s <- validate[random, 2]
	x_s <- c(validate[random, 3],validate[random, 4],validate[random, 5],validate[random, 6],validate[random, 7],validate[random, 8],validate[random, 9],validate[random, 10],validate[random,11])
	
	inner_product = sum(a*x_s)

	if (sign(inner_product+b) == 1 && y_s == 1){
		correct_validations <- correct_validations + 1
	}
	if (sign(inner_product+b) == -1 && y_s == -1){
		correct_validations <- correct_validations + 1
	}
	total_validations<-total_validations + 1
}

percent_correct = correct_validations/total_validations * 100
print(percent_correct) 


png('~/DAF/hw9/hw9_graph.png')
library("ggplot2")
g<-ggplot(correctnessFrame, aes(x=numberOfTests,y=correct, color=factor(constant))) +geom_point()
g
dev.off()