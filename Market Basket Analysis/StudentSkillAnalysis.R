install.packages("arules")
require(arules)

setwd("/Users/srigovind/Desktop/Personal/MSSE/Capstone/Market Basket Analysis/")
Students.Skills <- read.transactions("sample.txt", sep = ",")

setwd("/Users/srigovind/Desktop/Personal/Hadoop/Cloudera Certification/Market Basket Analysis/")
# Look at the statistics of the data
summary(Students.Skills)
# View the inputed data file
inspect(Students.Skills[1:3]) 
# View the support of each skill in alphabetical order
itemFrequency(Students.Skills[,1:3])
# C has appeared with 12(total students) * 0.41666667 (C suport) = 5 students
itemFrequencyPlot(Students.Skills, support = 0.20)
itemFrequencyPlot(Students.Skills, topN = 10) 
# Building Confidence of a Rule. Example: conf ({a,b} -> {c}) 
# A rule having high support ond high confidence is ranked accordingly
# Build the Model 
# Model has lhs, rhs, support, confidence, lift
# For our project we would compare student's skills with the lhs part 
# and recommend all the rhs which has the lhs matched
# By default with no values it gives support = 1% and confidence = 8%
model <- apriori(Students.Skills)
model
summary(model)
inspect(model)
inspect(model[1:3])
inspect(sort(model, by="lift")[1:10])
# Model with support = 30% and confidence = 25%
model1 <- apriori(Students.Skills, parameter = list(support = 0.30, confidence = 0.25, minlen = 2))
model1
summary(model1)
data <- inspect(model1[1:3])

class(data)
inspect(model1)
 