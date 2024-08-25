set.seed(1101)


###reading the data
library(class)
library(ggplot2)
setwd("C:/Users/Muhammad Salman/Documents/NUS/Course/DSA1101/DATA R")
data = read.csv("diabetes_5050.csv")
attach(data)



###Choosing the input variables
OR_HighBP = (table(Diabetes_binary, HighBP)[1]* table(Diabetes_binary, HighBP)[4])/(table(Diabetes_binary, HighBP)[2]*table(Diabetes_binary, HighBP)[3])

OR_HighChol = (table(Diabetes_binary, HighChol)[1]*table(Diabetes_binary, HighChol)[4])/(table(Diabetes_binary, HighChol)[2]*table(Diabetes_binary, HighChol)[3])

OR_CholCheck = (table(Diabetes_binary, CholCheck)[1]*table(Diabetes_binary, CholCheck)[4])/(table(Diabetes_binary, CholCheck)[2]*table(Diabetes_binary, CholCheck)[3])

ggplot(data, aes(x = factor(Diabetes_binary), y = BMI)) +
  geom_boxplot()

OR_Smoker = (table(Diabetes_binary, Smoker)[1]*table(Diabetes_binary, Smoker)[4])/(table(Diabetes_binary, Smoker)[2]*table(Diabetes_binary, Smoker)[3])

OR_Stroke = (table(Diabetes_binary, Stroke)[1]*table(Diabetes_binary, Stroke)[4])/(table(Diabetes_binary, Stroke)[2]*table(Diabetes_binary, Stroke)[3])

OR_HearthDiseaseorAttack = (table(Diabetes_binary, HeartDiseaseorAttack)[1]*table(Diabetes_binary, HeartDiseaseorAttack)[4])/(table(Diabetes_binary, HeartDiseaseorAttack)[2]*table(Diabetes_binary, HeartDiseaseorAttack)[3])

OR_PhysActivity = (table(Diabetes_binary, PhysActivity)[1]*table(Diabetes_binary, PhysActivity)[4])/(table(Diabetes_binary, PhysActivity)[2]*table(Diabetes_binary, PhysActivity)[3])

OR_Fruits = (table(Diabetes_binary, Fruits)[1]*table(Diabetes_binary, Fruits)[4])/(table(Diabetes_binary, Fruits)[2]*table(Diabetes_binary, Fruits)[3])

OR_Veggies = (table(Diabetes_binary, Veggies)[1]*table(Diabetes_binary, Veggies)[4])/(table(Diabetes_binary, Veggies)[2]*table(Diabetes_binary, Veggies)[3])

OR_HvyAlcoholConsump = (table(Diabetes_binary, HvyAlcoholConsump)[1]*table(Diabetes_binary, HvyAlcoholConsump)[4])/(table(Diabetes_binary, HvyAlcoholConsump)[2]*table(Diabetes_binary, HvyAlcoholConsump)[3])

OR_AnyHealthcare = (table(Diabetes_binary, AnyHealthcare)[1]*table(Diabetes_binary, AnyHealthcare)[4])/(table(Diabetes_binary, AnyHealthcare)[2]*table(Diabetes_binary, AnyHealthcare)[3])

OR_NoDocbcCost = (table(Diabetes_binary, NoDocbcCost)[1]*table(Diabetes_binary, NoDocbcCost)[4])/(table(Diabetes_binary, NoDocbcCost)[2]*table(Diabetes_binary, NoDocbcCost)[3])

ggplot(data, aes(x = factor(Diabetes_binary), y = GenHlth)) +
  geom_boxplot()

ggplot(data, aes(x = factor(Diabetes_binary), y = MentHlth)) +
  geom_boxplot()

ggplot(data, aes(x = factor(Diabetes_binary), y = PhysHlth)) +
  geom_boxplot()

OR_Diffwalk = (table(Diabetes_binary, DiffWalk)[1]*table(Diabetes_binary, DiffWalk)[4])/(table(Diabetes_binary, DiffWalk)[2]*table(Diabetes_binary, DiffWalk)[3])

OR_Sex = (table(Diabetes_binary, Sex)[1]*table(Diabetes_binary, Sex)[4])/(table(Diabetes_binary, Sex)[2]*table(Diabetes_binary, Sex)[3])

ggplot(data, aes(x = factor(Diabetes_binary), y = Age)) +
  geom_boxplot()

ggplot(data, aes(x = factor(Diabetes_binary), y = Education)) +
  geom_boxplot()

ggplot(data, aes(x = factor(Diabetes_binary), y =Income)) +
  geom_boxplot()


#Variables to drop = either above 0.3 or below 3 for categorical data
#and use the boxplot for numerical data
# variables to drop = Smoker, Physactivity, Fruits
# Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, PhysHlth, Sex, Education



###Splitting the dataset to train data and test data with the sampe proportion
index0 = which(data$Diabetes_binary == 0)
data_0 = data[index0,]
data_1 = data[-index0,]
number_of_data = dim(data_1)[1]
index_random = sample(rep(1:5, length.out = number_of_data))

data1 = rbind(data_0[which(index_random == 1),], data_1[which(index_random == 1),])
data2 = rbind(data_0[which(index_random == 2),], data_1[which(index_random == 2),])
data3 = rbind(data_0[which(index_random == 3),], data_1[which(index_random == 3),])
data4 = rbind(data_0[which(index_random == 4),], data_1[which(index_random == 4),])
data5 = rbind(data_0[which(index_random == 5),], data_1[which(index_random == 5),])
testdata1 = rbind(data2,data3,data4,data5)
testdata2 = rbind(data1,data3,data4,data5)
testdata3 = rbind(data1,data2,data4,data5)
testdata4 = rbind(data1,data2,data3,data5)
testdata5 = rbind(data1,data2,data3,data4)



###Building the classifier



###KNN

#I will perform knn with different values of k, from 1 to 20 and 
# also k equal to square root of total data (around 237 and 238)
#I will use accuracy as the goodness of fit for each classifier
accuracy_knn = numeric(22)
accuracy_knn1 = numeric(22)
accuracy_knn2 = numeric(22)
accuracy_knn3 = numeric(22)
accuracy_knn4 = numeric(22)
accuracy_knn5 = numeric(22)

###preparing the data for knn
train_knn = c(2,3,4,5,7,8,15,16,18,19,21)

#use data1 as test data

for(a in 1:20){
  knn.pred1 = knn(scale(testdata1[,train_knn]), scale(data1[,train_knn]),testdata1[,1], k = a)
  table = table(knn.pred1, data1[,1])
  accuracy_knn1[a] = (table[1] + table[4])/sum(table)
}

#k = 237
knn.pred1 = knn(scale(testdata1[,train_knn]), scale(data1[,train_knn]),testdata1[,1], k = 237)
table = table(knn.pred1, data1[,1])
accuracy_knn1[21] = (table[1] + table[4])/sum(table)

#k = 238
knn.pred1 = knn(scale(testdata1[,train_knn]), scale(data1[,train_knn]),testdata1[,1], k = 238)
table = table(knn.pred1, data1[,1])
accuracy_knn1[22] = (table[1] + table[4])/sum(table)


#use data2 as test data

for(b in 1:20){
  knn.pred2 = knn(scale(testdata2[,train_knn]), scale(data2[,train_knn]),testdata2[,1], k = b)
  table = table(knn.pred2, data2[,1])
  accuracy_knn2[b] = (table[1] + table[4])/sum(table)
}

#k = 237
knn.pred2 = knn(scale(testdata2[,train_knn]), scale(data2[,train_knn]),testdata2[,1], k = 237)
table = table(knn.pred2, data2[,1])
accuracy_knn2[21] = (table[1] + table[4])/sum(table)

#k = 238
knn.pred2 = knn(scale(testdata2[,train_knn]), scale(data2[,train_knn]),testdata2[,1], k = 238)
table = table(knn.pred2, data2[,1])
accuracy_knn2[22] = (table[1] + table[4])/sum(table)


#use data3 as test data

for(c in 1:20){
  knn.pred3 = knn(scale(testdata3[,train_knn]), scale(data3[,train_knn]),testdata3[,1], k = c)
  table = table(knn.pred3, data3[,1])
  accuracy_knn3[c] = (table[1] + table[4])/sum(table)
}

#k = 237
knn.pred3 = knn(scale(testdata3[,train_knn]), scale(data3[,train_knn]),testdata3[,1], k = 237)
table = table(knn.pred3, data3[,1])
accuracy_knn3[21] = (table[1] + table[4])/sum(table)

#k = 238
knn.pred3 = knn(scale(testdata3[,train_knn]), scale(data3[,train_knn]),testdata3[,1], k = 238)
table = table(knn.pred3, data3[,1])
accuracy_knn3[22] = (table[1] + table[4])/sum(table)


#use data4 as test data
for(d in 1:20){
  knn.pred4 = knn(scale(testdata4[,train_knn]), scale(data4[,train_knn]),testdata4[,1], k = d)
  table = table(knn.pred4, data4[,1])
  accuracy_knn4[d] = (table[1] + table[4])/sum(table)
}

#k = 237
knn.pred4 = knn(scale(testdata4[,train_knn]), scale(data4[,train_knn]),testdata4[,1], k = 237)
table = table(knn.pred4, data4[,1])
accuracy_knn4[21] = (table[1] + table[4])/sum(table)

#k = 238
knn.pred4 = knn(scale(testdata4[,train_knn]), scale(data4[,train_knn]),testdata4[,1], k = 238)
table = table(knn.pred4, data4[,1])
accuracy_knn4[22] = (table[1] + table[4])/sum(table)


#use data5 as test data
for(e in 1:20){
  knn.pred5 = knn(scale(testdata5[,train_knn]), scale(data5[,train_knn]),testdata5[,1], k = e)
  table = table(knn.pred5, data5[,1])
  accuracy_knn5[e] = (table[1] + table[4])/sum(table)
}

#k = 237
knn.pred5 = knn(scale(testdata5[,train_knn]), scale(data5[,train_knn]),testdata5[,1], k = 237)
table = table(knn.pred5, data5[,1])
accuracy_knn5[21] = (table[1] + table[4])/sum(table)

#k = 238
knn.pred5 = knn(scale(testdata5[,train_knn]), scale(data5[,train_knn]),testdata5[,1], k = 238)
table = table(knn.pred5, data5[,1])
accuracy_knn5[22] = (table[1] + table[4])/sum(table)

#calculation the average of accuracy
for(count in 1:22){
  accuracy_knn[count] = (accuracy_knn1[count] +accuracy_knn2[count] + accuracy_knn3[count] + accuracy_knn4[count] +accuracy_knn5[count])/5
}

#we will choose k = 238 which is the square root of total number of the data 
#with accuracy equal to 0.7362078



###Decision Tree

library(rpart)
library(rpart.plot)

accuracy_DT = numeric(11)
accuracy_DT1 = numeric(11)
accuracy_DT2 = numeric(11)
accuracy_DT3 = numeric(11)
accuracy_DT4 = numeric(11)
accuracy_DT5 = numeric(11)


cp = 10^(-5:5)


#using data1 as test data
for(j in 1:length(cp)){
  
  fit <- rpart(Diabetes_binary~.,
               method="class",
               data=testdata1,
               control=rpart.control(cp = cp[j]),
               parms=list(split='information')
  )
  pred_fit = predict(fit, data1[,-1], type = 'class')
  table_DT = table(pred_fit, data1[,1])
  accuracy_DT1[j] = (table_DT[1]+table_DT[4])/sum(table_DT)
}

#using data2 as test data
for(j in 1:length(cp)){
  
  fit <- rpart(Diabetes_binary~.,
               method="class",
               data=testdata2,
               control=rpart.control(cp = cp[j]),
               parms=list(split='information')
  )
  pred_fit = predict(fit, data2[,-1], type = 'class')
  table_DT = table(pred_fit, data2[,1])
  accuracy_DT2[j] = (table_DT[1]+table_DT[4])/sum(table_DT)
}

#using data3 as test data
for(j in 1:length(cp)){
  
  fit <- rpart(Diabetes_binary~.,
               method="class",
               data=testdata3,
               control=rpart.control(cp = cp[j]),
               parms=list(split='information')
  )
  pred_fit = predict(fit, data3[,-1], type = 'class')
  table_DT = table(pred_fit, data3[,1])
  accuracy_DT3[j] = (table_DT[1]+table_DT[4])/sum(table_DT)
}

#using data4 as test data
for(j in 1:length(cp)){
  
  fit <- rpart(Diabetes_binary~.,
               method="class",
               data=testdata4,
               control=rpart.control(cp = cp[j]),
               parms=list(split='information')
  )
  pred_fit = predict(fit, data4[,-1], type = 'class')
  table_DT = table(pred_fit, data4[,1])
  accuracy_DT4[j] = (table_DT[1]+table_DT[4])/sum(table_DT)
}

#using data5 as test data
for(j in 1:length(cp)){
  
  fit <- rpart(Diabetes_binary~.,
               method="class",
               data=testdata5,
               control=rpart.control(cp = cp[j]),
               parms=list(split='information')
  )
  pred_fit = predict(fit, data5[,-1], type = 'class')
  table_DT = table(pred_fit, data5[,1])
  accuracy_DT5[j] = (table_DT[1]+table_DT[4])/sum(table_DT)
}

for(x in 1:length(accuracy_DT)){
  accuracy_DT[x] = (accuracy_DT1[x] + accuracy_DT2[x] + accuracy_DT3[x] + accuracy_DT4[x] + accuracy_DT5[x] )/5
}

fit <- rpart(Diabetes_binary~.,
             method="class",
             data=data,
             control=rpart.control(cp = cp[3]),
             parms=list(split='information')
)

rpart.plot(fit, type=4)

#the maximum accuracy of this decision tree is when the value of cp is equal to 
#1e-04 with accuracy 0.7420925



###Naive Bayes


library(e1071)

threshold = seq(0,1,0.05)
accuracy_NB = numeric(21)
accuracy_NB1 = numeric(21)
accuracy_NB2 = numeric(21)
accuracy_NB3 = numeric(21)
accuracy_NB4 = numeric(21)
accuracy_NB5 = numeric(21)

NaiveBayes_input = c(1,2,3,4,5,7,8,15,16,18,19,21)
NaiveBayes_test = c(2,3,4,5,7,8,15,16,18,19,21)

#use data1 as test data
model <- naiveBayes(Diabetes_binary ~., testdata1[,NaiveBayes_input])
result = predict(model, data1[,NaiveBayes_test],"raw")

for(x in 1:21){
  acc = ifelse(result[,2] >= threshold[x] , 1,0)
  table = table(acc, data1[,1])
  accuracy_NB1[x] = (table[1] + table[4])/sum(table)
}

#use data2 as test data
model <- naiveBayes(Diabetes_binary ~., testdata2[,NaiveBayes_input])
result = predict(model, data2[,NaiveBayes_test],"raw")

for(x in 1:21){
  acc = ifelse(result[,2] >= threshold[x] , 1,0)
  table = table(acc, data2[,1])
  accuracy_NB2[x] = (table[1] + table[4])/sum(table)
}

#use data3 as test data
model <- naiveBayes(Diabetes_binary ~., testdata3[,NaiveBayes_input])
result = predict(model, data3[,NaiveBayes_test],"raw")

for(x in 1:21){
  acc = ifelse(result[,2] >= threshold[x] , 1,0)
  table = table(acc, data3[,1])
  accuracy_NB3[x] = (table[1] + table[4])/sum(table)
}

#use data4 as test data
model <- naiveBayes(Diabetes_binary ~., testdata4[,NaiveBayes_input])
result = predict(model, data4[,NaiveBayes_test],"raw")

for(x in 1:21){
  acc = ifelse(result[,2] >= threshold[x] , 1,0)
  table = table(acc, data4[,1])
  accuracy_NB4[x] = (table[1] + table[4])/sum(table)
}

#use data5 as test data
model <- naiveBayes(Diabetes_binary ~., testdata5[,NaiveBayes_input])
result = predict(model, data5[,NaiveBayes_test],"raw")

for(x in 1:21){
  acc = ifelse(result[,2] >= threshold[x] , 1,0)
  table = table(acc, data5[,1])
  accuracy_NB5[x] = (table[1] + table[4])/sum(table)
}

for(x in 1:21){
  accuracy_NB[x] = (accuracy_NB1[x] + accuracy_NB2[x] + accuracy_NB3[x] + accuracy_NB4[x] + accuracy_NB5[x])/5
}

#use 0.3 as threshold will give the maximum value of accuracy




###Logistic

check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping smoker since it has a very large p-value
data = data[,-6]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping NoDocbcCost since it has a very large p-value
data = data[,-13]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping AnyHealthcare since it has a large p-value
data = data[,-12]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping PhysActivity since it has a large p-value
data = data[,-8]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping Fruits since it has a large p-value
data = data[,-8]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping Veggies since it has a relatively large p-value
data = data[,-8]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping MentHlth since it has a relatively large p-value
data = data[,-10]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping Education since it has a relatively large p-value
data = data[,-14]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping Stroke since it has a relatively large p-value
data = data[,-6]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping Diffwalk since it has a relatively large p-value
data = data[,-10]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#dropping PhysHlth since it has a relatively large p-value
data = data[,-9]
check = glm(Diabetes_binary ~., data = data, family = binomial(link = "logit"))
summary(check)

#changing the categorical variable to factor
data$HighBP = as.factor(HighBP)
data$HighChol = as.factor(HighChol)
data$CholCheck = as.factor(CholCheck)
data$HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack)
data$HvyAlcoholConsump = as.factor(HvyAlcoholConsump)

index0 = which(data$Diabetes_binary == 0)
data_0 = data[index0,]
data_1 = data[-index0,]
number_of_data = dim(data_1)[1]
index_random = sample(rep(1:5, length.out = number_of_data))

data1 = rbind(data_0[which(index_random == 1),], data_1[which(index_random == 1),])
data2 = rbind(data_0[which(index_random == 2),], data_1[which(index_random == 2),])
data3 = rbind(data_0[which(index_random == 3),], data_1[which(index_random == 3),])
data4 = rbind(data_0[which(index_random == 4),], data_1[which(index_random == 4),])
data5 = rbind(data_0[which(index_random == 5),], data_1[which(index_random == 5),])
testdata1 = rbind(data2,data3,data4,data5)
testdata2 = rbind(data1,data3,data4,data5)
testdata3 = rbind(data1,data2,data4,data5)
testdata4 = rbind(data1,data2,data3,data5)
testdata5 = rbind(data1,data2,data3,data4)

###I will be using 10 input variables for the logistic

threshold_logit = seq(0,1,0.05)
acc_logit = numeric(21)
acc_logit1 = numeric(21)
acc_logit2 = numeric(21)
acc_logit3 = numeric(21)
acc_logit4 = numeric(21)
acc_logit5 = numeric(21)


#data1 as test data
logit = glm(Diabetes_binary ~., data = testdata1, family = binomial(link = "logit"))
logit_pred = predict(logit, data1[,-1], type = "response")

for(x in 1:length(threshold_logit)){
  val = ifelse(logit_pred >= threshold_logit[x], 1, 0)
  table = table(val, data1[,1])
  acc_logit1[x] = (table[1] + table[4])/sum(table)
}

#data2 as test data
logit = glm(Diabetes_binary ~., data = testdata2, family = binomial(link = "logit"))
logit_pred = predict(logit, data2[,-1], type = "response")

for(x in 1:length(threshold_logit)){
  val = ifelse(logit_pred >= threshold_logit[x], 1, 0)
  table = table(val, data2[,1])
  acc_logit2[x] = (table[1] + table[4])/sum(table)
}

#data3 as test data
logit = glm(Diabetes_binary ~., data = testdata3, family = binomial(link = "logit"))
logit_pred = predict(logit, data3[,-1], type = "response")

for(x in 1:length(threshold_logit)){
  val = ifelse(logit_pred >= threshold_logit[x], 1, 0)
  table = table(val, data3[,1])
  acc_logit3[x] = (table[1] + table[4])/sum(table)
}

#data4 as test data
logit = glm(Diabetes_binary ~., data = testdata4, family = binomial(link = "logit"))
logit_pred = predict(logit, data4[,-1], type = "response")

for(x in 1:length(threshold_logit)){
  val = ifelse(logit_pred >= threshold_logit[x], 1, 0)
  table = table(val, data4[,1])
  acc_logit4[x] = (table[1] + table[4])/sum(table)
}

#data5 as test data
logit = glm(Diabetes_binary ~., data = testdata5, family = binomial(link = "logit"))
logit_pred = predict(logit, data5[,-1], type = "response")

for(x in 1:length(threshold_logit)){
  val = ifelse(logit_pred >= threshold_logit[x], 1, 0)
  table = table(val, data5[,1])
  acc_logit5[x] = (table[1] + table[4])/sum(table)
}

for(y in 1:21){
  acc_logit[y] = (acc_logit1[y] + acc_logit2[y] + acc_logit3[y] + acc_logit4[y] + acc_logit5[y])/5
}

#accuracy maximum is obtained when threshold is equal to 0.45 (0.7484016)

