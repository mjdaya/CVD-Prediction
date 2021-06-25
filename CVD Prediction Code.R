# Set directory
#setwd("/Volumes/Backup Plus/MSBA/Classes/Spring/Predictive Analytics/Project")
# Read data
dat = read.csv("cardio_train.csv", sep=";")

#Libraries

library(e1071)
#install.packages("caTools")
#install.packages("caret")
library(caTools)
library(caret)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library("corrplot")
library(dplyr)
library(pROC)
library(class)
# install.packages("rpart")
# install.packages("rpart.plot")	
library(rpart)
library(rpart.plot)
# https://www.gormanalysis.com/blog/decision-trees-in-r-using-rpart/
#install.packages("rattle")
library(rattle) # fancyRpartPlot
# https://bradleyboehmke.github.io/HOML/DT.html
#install.packages("vip")
library(vip)# for feature importance
# using package "tree"
library(tree)
library(party)



# Step 1: Data Cleaning & Understanding the Data

# checking for NA values 
sum(is.na(dat)) 

# turning days into age
dat$age = round(dat$age/365, 0)

#creating a raw data set for use in visualization
raw <- dat
table(raw$cardio) # tabeling the outcome variable to check distribution, the data looks balanced.

# converting cholesterol and glucose to factors
dat$cholesterol = as.factor(dat$cholesterol)
dat$gluc = as.factor(dat$gluc)

# Original Data:  Gender 1 - Female, 2 - Male
# Converting gender = 2 to 0 (Males)
dat$gender[dat$gender == 2] = 0 

# trying to figure out what a realistic high blood pressure is, then determining how to subset
dat2 = subset(dat, ap_hi <= 180 & ap_hi > 0)  
dat2 = subset(dat2, ap_lo <= 120 & ap_lo > 0)
dat3 = subset(dat2, height > 120 & height <= 210)


####EDA###
res <- cor(raw[, c(2,3,4,5,6,7,8,9,10,11,12,13)])
round(res, 2)
corrplot(res, method="circle")
corrplot(res, method="number")



###plotting age distribution 
ageplot <- raw %>%
  ggplot( aes(x=cardio,y=age,fill=factor(cardio))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  geom_boxplot(width=0.1)+
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Age distribution CardioVascualer Disease") +
  xlab("Cardio")
ageplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))



###plotting height distribution 
heightplot <- raw %>%
  ggplot( aes(x=cardio,y=height,fill=factor(cardio))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_boxplot(width=0.1)+
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Height distribution CardioVascualer Disease") +
  xlab("Cardio")
heightplot+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))

ggarrange(ageplot, heightplot, ncol = 1, nrow = 2)

###plotting weight distribution 
weightplot <- raw %>%
  ggplot( aes(x=cardio,y=weight,fill=factor(cardio))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_boxplot(width=0.1)+
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Weight distribution CardioVascualer Disease") +
  xlab("Cardio")
weightplot+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))


###plotting cholesterol distribution 
chplot <- raw %>%
  ggplot( aes(x=cardio,y=cholesterol,fill=factor(cardio))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("cholesterol distribution CardioVascualer Disease") +
  xlab("Cardio")
chplot+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))


ggarrange(weightplot, chplot, ncol = 1, nrow = 2)

#####Plotting gender distribution
gen <- ggplot(raw, 
              aes(x = as.factor(gender), 
                  fill = as.factor(cardio))) + 
  geom_bar(position = "dodge")+
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Gender distribution in CardioVascualer Disease") +
  xlab("Gender")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))
gen+ guides(fill=guide_legend(title="Cardio"))

###plotting blood pressure ap_hi distribution 
###WE have used cleaned data as the blood pressure had outlier showing abnormal
###scales of the distribution which couldn't be plotted
aphi <- dat2 %>%
  ggplot( aes(x=cardio,y=ap_hi,fill=factor(cardio))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Systolic BP distribution - CardioVascualer Disease") +
  xlab("Cardio") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  guides(fill=guide_legend(title="Cardio"))
aphi

###plotting blood pressure ap_lo distribution 
###WE have used cleaned data as the blood pressure had outlier showing abnormal
###scales of the distribution which couldn't be plotted
aplow <- dat2 %>%
  ggplot( aes(x=cardio,y=ap_lo,fill=factor(cardio))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.8, option="A") +
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Diastolic BP distribution - CardioVascualer Disease") +
  xlab("Cardio")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  guides(fill=guide_legend(title="Cardio"))
aplow


ggarrange(aphi, aplow, ncol = 2, nrow = 1)

######age as per cardio disease

cf <- as.factor(raw$cardio)
p <- raw %>%
  ggplot( aes(x=age, fill=cf)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p


######glucose as per cardio disease
glucose <- ggplot(raw, 
                  aes(x = as.factor(gluc), 
                      fill = as.factor(cardio))) + 
  geom_bar(position = "dodge")+
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Glucose level distribution in CardioVascualer Disease") +
  xlab("Glucose Level")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))
glucose + guides(fill=guide_legend(title="Cardio"))

######smoking as per cardio disease
smoking <- ggplot(raw, 
                  aes(x = as.factor(smoke), 
                      fill = as.factor(cardio))) + 
  geom_bar(position = "dodge")+
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Smoking distribution \n in CardioVascualer Disease") +
  xlab("Smoking")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))
smoking + guides(fill=guide_legend(title="Cardio"))

######alcohol as per cardio disease
alcohol <- ggplot(raw, 
                  aes(x = as.factor(alco), 
                      fill = as.factor(cardio))) + 
  geom_bar(position = "dodge")+
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  scale_fill_manual(values=c("#f4ccccff", "#c9d9e0ff"))+
  ggtitle("Alcohol distribution \n in CardioVascualer Disease") +
  xlab("Alcohol")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"))
alcohol + guides(fill=guide_legend(title="Cardio"))

ggarrange(smoking, alcohol, ncol = 2, nrow = 1)

##############End of data cleaning & Vizualization ############################
#logistic regression

set.seed(123456)
options(scipen = 999)
lr_dat <- dat3
sample = sample.split(lr_dat$cardio, SplitRatio = 0.7)
dat.train = subset(lr_dat, sample == TRUE)
dat.test  = subset(lr_dat, sample == FALSE)

logreg <- glm(cardio ~ . - id, data = dat.train, 
              family = "binomial")
summary(logreg)

yhat.test <- predict(logreg, dat.test, 
                     type = "response")

yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)

tab.lr1.test <- table(dat.test$cardio, 
                      yhat.test.class, 
                      dnn = c("Actual","Predicted"))
tab.lr1.test

sum.lr.all <- summary(logreg)
sum.lr.all$coefficients[sum.lr.all$coefficients[,4]<0.05,]

lr.2.train <- glm(cardio ~ . - id - gender, 
                  data = dat.train, 
                  family = "binomial")
summary(lr.2.train)

yhat.2.test <- predict(lr.2.train, dat.test, 
                       type = "response")

yhat.2.test.cl <- ifelse(yhat.2.test > 0.5, 1, 0)

tab.2.test <- table(dat.test$cardio, yhat.2.test.cl, 
                    dnn = c("Actual","Predicted"))
tab.2.test

lr.3.train <- glm(cardio ~ . - id - gender
                  + (age*cholesterol) + (age*gluc) + (ap_hi*cholesterol)
                  + (ap_lo*cholesterol) + (ap_hi*age) + (ap_lo*age)
                  + (ap_hi*gluc) + (ap_lo*gluc) , 
                  data = dat.train, 
                  family = "binomial")
summary(lr.3.train)

yhat.3.test <- predict(lr.3.train, dat.test, 
                       type = "response")

yhat.3.test.cl <- ifelse(yhat.3.test > 0.4, 1, 0)

tab.3.test <- table(dat.test$cardio, yhat.3.test.cl, 
                    dnn = c("Actual","Predicted"))

tab.3.test

#################Logistic Regression Ends Here##################################

#########knn
dat.train.x <- dat.train[,2:12]
dat.train.y <- dat.train[,13]
dat.test.x <- dat.test[,2:12]
dat.test.y <- dat.test[,13]

i=1
k.optm=1
for (i in 1:30){
  knn.mod <- knn(train = dat.train.x, test = dat.test.x,
                 cl = dat.train.y, k = i)
  k.optm[i] <- 100 * sum(dat.test.y == knn.mod)/NROW(dat.test.y)
  k=i
  cat(k,'=',k.optm[i],' 
')}

plot(k.optm, type="b", xlab="K Value",ylab="Accuracy")

####KNN with 23 K's
out23 <- knn(dat.train.x, dat.test.x, dat.train.y, k=23)

tab.knn23 <- table(dat.test.y, out23,
                   dnn = c("Actual", "Predicted"))
tab.knn23

#####KNN with 11 K's
out11 <- knn(dat.train.x, dat.test.x, dat.train.y, k=11)
tab.knn11 <- table(dat.test.y, out11,
                   dnn = c("Actual", "Predicted"))
tab.knn11

#####KNN with 5 K's
out5 <- knn(dat.train.x, dat.test.x, dat.train.y, k=5)
tab.knn5 <- table(dat.test.y, out5,
                  dnn = c("Actual", "Predicted"))
tab.knn5

#####KNN with 10 K's
out10 <- knn(dat.train.x, dat.test.x, dat.train.y, k=10)
tab.knn10 <- table(dat.test.y, out10,
                   dnn = c("Actual", "Predicted"))
tab.knn10

#####################KNN  ENDS HERE###########################################

######Decision Tree

dt_dat <- dat3[,-1]
### Decision Tree
# https://www.guru99.com/r-decision-trees.html
# prepare the data for decision tree classifier 
# need to discrete the following variables: 


set.seed(123456)
# Doing Train-Test Split
sample = sample.split(dt_dat$cardio, SplitRatio = 0.7)
train_dt = subset(dt_dat, sample == TRUE)
test_dt  = subset(dt_dat, sample == FALSE)


# checking distribution of Y variable in original data and train, test sets
prop.table(table(dt_dat$cardio))
prop.table(table(train_dt$cardio))
prop.table(table(test_dt$cardio))



fit1 <- rpart(cardio~., data = train_dt, method = 'class', cp = 0.001)
#rpart.plot(fit1, extra = "auto")

# 49% of cases in our data set have CVD
# 60% of patients have Systoli BP under 130 and have a 22% probability of having CVD

fancyRpartPlot(fit1, caption = NULL) 
# node 1 shows 51% 0, 49% 1
# node 2 is split based on ap_hi < 130, then you have 0.60% no, 40% yes

y_hat_rpart <-predict(fit1, test_dt, type = 'class')
tab_rpart = table(test_dt$cardio, y_hat_rpart)
accuracy_Test <- sum(diag(tab_rpart)) / sum(tab_rpart)
#accuracy_Test

#summary(fit1)
#rsq.rpart(fit1) #produces 2 plots
#plotcp(fit1) # visualize cross-validation results 


Feat_imp= vip(fit1, num_features = 11, bar = FALSE)
Feat_imp_dat = Feat_imp$data
Feat_imp_dat$Variable = with(Feat_imp_dat, reorder(Variable, Importance))

#library("ggplot2")
ggplot(data=Feat_imp_dat, aes(x=Importance, y=Variable)) +
  geom_bar(stat="identity", fill="white") + 
  labs(title="Feature Importance")+
  theme_classic() + 
  theme(plot.background = element_rect(fill = "#ea9999ff"), 
        panel.background = element_rect(fill = "#ea9999ff",colour = "#ea9999ff"),
        text=element_text(color="white"),
        axis.text.x = element_text(color="white"),
        axis.text.y = element_text(color="white"),
        axis.line = element_line(colour = "white"),
        axis.ticks = element_line(colour = "white"))




# using package "tree"
#library(tree)
tree1<- tree(cardio~., data = train_dt)
summary(tree1) # misclassification is 0.194

plot(tree1)
text(tree1, pretty = 0)
#summary(tree1)

y.hat.tree1 <- predict(tree1, test_dt, type = "class")
tab_tree1 = table(test_dt$cardio, y.hat.tree1)
accuracy.tree.1 <- sum(diag(tab_tree1)) / sum(tab_tree1)
#accuracy.tree.1

cv.tree1 = cv.tree(tree1)
plot(cv.tree1)

# https://stats.stackexchange.com/questions/12140/conditional-inference-trees-vs-traditional-decision-trees
# ctree uses a significance test procedure in order to select variables instead 
# of selecting the variable that maximizes an information measure (e.g. Gini coefficient).

# so generally the main difference seems to be that ctree uses a covariate selection 
# scheme that is based on statistical theory (i.e. selection by permutation-based significance tests) 
# and thereby avoids a potential bias in rpart, otherwise they seem similar; e.g. 
# conditional inference trees can be used as base learners for Random Forests.

#  conditional inference trees adapt the significance test procedures to select 
# variables rather than selecting variables by maximizing information measures (rpart employs a Gini coefficient).
ctree1 = ctree(cardio  ~ .,  train_dt, control=ctree_control(maxdepth=5))
plot(ctree1, type = "simple",
     terminal_panel=node_terminal(ctree1,
                                  abbreviate = TRUE,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE))


y.hat.ctree1 = predict(ctree1, newdata=test_dt)
tab_ctree1 = table(test_dt$cardio, y.hat.ctree1)
accuracy.ctree.1 <- sum(diag(tab_ctree1)) / sum(tab_ctree1)
#accuracy.ctree.1
#summary(ctree1)


### 
ctree2 = ctree(cardio  ~ .,  train_dt) #, control=ctree_control(maxdepth=5))
#plot(ctree2, type = "simple")
y.hat.ctree2 = predict(ctree2, newdata=test_dt)
tab_ctree2 = table(test_dt$cardio, y.hat.ctree2)
accuracy.ctree.2 <- sum(diag(tab_ctree2)) / sum(tab_ctree2)
#accuracy.ctree.2


###
ctree3 = ctree(cardio  ~ .,  train_dt, control=ctree_control(maxdepth=4))
#plot(ctree3, type = "simple")
y.hat.ctree3 = predict(ctree3, newdata=test_dt)
tab_ctree3 = table(test_dt$cardio, y.hat.ctree3)
accuracy.ctree.3 <- sum(diag(tab_ctree3)) / sum(tab_ctree3)


ctree4 = ctree(cardio  ~ .,  train_dt, control=ctree_control(maxdepth=2))
#plot(ctree4, type = "simple")
y.hat.ctree4 = predict(ctree4, newdata=test_dt)
tab_ctree4 = table(test_dt$cardio, y.hat.ctree4)
accuracy.ctree.4 <- sum(diag(tab_ctree4)) / sum(tab_ctree4)

#accuracy.ctree.3

mat.accuracies = matrix(c(accuracy_Test, #accuracy_Test2,
                          accuracy.tree.1, 
                          accuracy.ctree.1, accuracy.ctree.2, accuracy.ctree.3, accuracy.ctree.4), ncol = 1, byrow = TRUE)
colnames(mat.accuracies) = c("Accuracy-Validation")
rownames(mat.accuracies) = c("rpar1* D  = 10", # "rpar2", 
                             "Dtree1 D = 1", 
                             "CTree1* D = 5", "CTree2 D = 10", "CTree3 D = 4", "CTree3 D = 2")
mat.accuracies

# mat.accuracies[order(mat.accuracies, decreasing = TRUE),]


# Checking Type II Errors
# 0 no heart disease, 1 heart disease
# Type 2 error, when you predict someone doesn't have cardiovascular disease but they do
# (1,0) reality is 1, predicted is 0

tab_rpart
tab_rpart[2,2]/sum(tab_rpart[2,])

tab_tree1
tab_tree1[2,2]/sum(tab_tree1[2,])

tab_ctree1  #DEPTH 5
tab_ctree1[2,2]/sum(tab_ctree1[2,])

tab_ctree2 #depth 10
tab_ctree2[2,2]/sum(tab_ctree2[2,])

tab_ctree3 #depth 4
tab_ctree3[2,2]/sum(tab_ctree3[2,])

tab_ctree4 # depth 2
tab_ctree4[2,2]/sum(tab_ctree4[2,])


#precision: TP/TP+FP
#bottom row
##################END OF DECISION TREE HERE###################################

##############Support Vector Machine###########################################

# Creating Dummy Variables for categorical variables
cholesterol <- as.factor(dat3$cholesterol)
gluc <- as.factor(dat3$gluc)
tmp_cho <- data.frame(model.matrix(~cholesterol-1))
tmp_gluc <- data.frame(model.matrix(~gluc-1))
dt_svm <- cbind(dat3[,c(13, 2:7, 10:12)], 
              tmp_cho[,1:2], tmp_gluc[,1:2])
rm(cholesterol, gluc)
rm(tmp_cho, tmp_gluc)
str(dt_svm)

set.seed(123456)

# Doing Train-Test Split
sample = sample.split(dt_svm$cardio, SplitRatio = 0.7)
train_svm = subset(dt_svm, sample == TRUE)
test_svm  = subset(dt_svm, sample == FALSE)
train_svm$cardio <- as.factor(train_svm$cardio)
test_svm$cardio <- as.factor(test_svm$cardio)

# Case 1: Linear Kernel
svmfit1 <- svm(cardio ~ ., 
               data = train_svm, kernel = "linear", 
               cost = 1)

traintable1 <- table(truth = train_svm$cardio, predict = svmfit1$fitted)
traintable1
testtable1 <- table(truth = test_svm$cardio, predict = predict(svmfit1, test_svm))
testtable1

confusionMatrix(traintable1)
confusionMatrix(testtable1)

# Train Accuracy: 0.7268 
# Test Accuracy: 0.7239

# Case 2: Radial Kernel
svmfit2 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 1)

traintable2 <- table(truth = train_svm$cardio, predict = svmfit2$fitted)
traintable2
testtable2 <- table(truth = test_svm$cardio, predict = predict(svmfit2, test_svm))
testtable2

confusionMatrix(traintable2)
confusionMatrix(testtable2)

# Train Accuracy: 0.7391
# Test Accuracy: 0.7324

# Case 3: Increasing Cost from 1 to 10
svmfit3 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 10)

traintable3 <- table(truth = train_svm$cardio, predict = svmfit3$fitted)
traintable3
testtable3 <- table(truth = test_svm$cardio, predict = predict(svmfit3, test))
testtable3

confusionMatrix(traintable3)
confusionMatrix(testtable3)

# Train Accuracy: 0.7487
# Test Accuracy: 0.7298

# Case 4: Decreasing Cost from 1 to 0.1
svmfit4 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 0.1)

traintable4 <- table(truth = train_svm$cardio, predict = svmfit4$fitted)
traintable4
testtable4 <- table(truth = test_svm$cardio, predict = predict(svmfit4, test_svm))
testtable4

confusionMatrix(traintable4)
confusionMatrix(testtable4)

# Train Accuracy: 0.7331
# Test Accuracy: 0.7325

# Case 5: Adding Gamma = 1
svmfit5 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 1, gamma = 1)

traintable5 <- table(truth = train_svm$cardio, predict = svmfit5$fitted)
traintable5
testtable5 <- table(truth = test_svm$cardio, predict = predict(svmfit5, test_svm))
testtable5

confusionMatrix(traintable5)
confusionMatrix(testtable5)

# Train Accuracy: 0.7928
# Test Accuracy: 0.7203

# Case 6: Decreasing Cost from 1 to 0.01
svmfit6 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 0.01)

traintable6 <- table(truth = train_svm$cardio, predict = svmfit6$fitted)
traintable6
testtable6 <- table(truth = test_svm$cardio, predict = predict(svmfit6, test_svm))
testtable6

confusionMatrix(traintable6)
confusionMatrix(testtable6)

# Train Accuracy: 0.7246
# Test Accuracy: 0.7254

# Case 7: Adding Gamma = 0.1
svmfit7 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 1, gamma = 0.1)

traintable7 <- table(truth = train_svm$cardio, predict = svmfit7$fitted)
traintable7
testtable7 <- table(truth = test_svm$cardio, predict = predict(svmfit7, test_svm))
testtable7

confusionMatrix(traintable7)
confusionMatrix(testtable7)

# Train Accuracy: 0.7413
# Test Accuracy: 0.7328

# Case 8: Adding Gamma = 5
svmfit8 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 1, gamma = 5)

traintable8 <- table(truth = train_svm$cardio, predict = svmfit8$fitted)
traintable8
testtable8 <- table(truth = test_svm$cardio, predict = predict(svmfit8, test_svm))
testtable8

confusionMatrix(traintable8)
confusionMatrix(testtable8)

# Train Accuracy: 0.8729
# Test Accuracy: 0.6841

# Case 9: Changing cost from 1 to 5
svmfit9 <- svm(cardio ~ ., 
               data = train_svm, kernel = "radial", 
               cost = 5)

traintable9 <- table(truth = train_svm$cardio, predict = svmfit9$fitted)
traintable9
testtable9 <- table(truth = test_svm$cardio, predict = predict(svmfit9, test_svm))
testtable9

confusionMatrix(traintable9)
confusionMatrix(testtable9)

# Train Accuracy: 0.7458  
# Test Accuracy: 0.7306

# Case 10: Cost = 0.1, Gamma = 0.1
svmfit10 <- svm(cardio ~ ., 
                data = train_svm, kernel = "radial", 
                cost = 0.1, gamma = 0.1)

traintable10 <- table(truth = train_svm$cardio, predict = svmfit10$fitted)
traintable10
testtable10 <- table(truth = test_svm$cardio, predict = predict(svmfit10, test_svm))
testtable10

confusionMatrix(traintable10)
confusionMatrix(testtable10)

# Train Accuracy: 0.7333 
# Test Accuracy: 0.73


# Case 11: Cost = 10, Gamma = 0.1
svmfit11 <- svm(cardio ~ ., 
                data = train_svm, kernel = "radial", 
                cost = 10, gamma = 0.1)

traintable11 <- table(truth = train_svm$cardio, predict = svmfit11$fitted)
traintable11
testtable11 <- table(truth = test_svm$cardio, predict = predict(svmfit11, test_svm))
testtable11

confusionMatrix(traintable11)
confusionMatrix(testtable11)

# Train Accuracy: 0.7534 
# Test Accuracy: 0.7286


# BEST MODEL: Case 7

svmBest <- svm(cardio ~ ., data = train_svm, kernel = "radial", 
               cost = 1, gamma = 0.1)

traintablebest <- table(truth = train_svm$cardio, predict = svmBest$fitted)
traintablebest 
testtablebest  <- table(truth = test_svm$cardio, predict = predict(svmBest, test_svm))
testtablebest 

confusionMatrix(traintablebest)
confusionMatrix(testtablebest)

# Train Accuracy: 0.7413
# Test Accuracy: 0.7328

summary(svmBest)

cv_sample = dt_svm[sample(1:nrow(dt_svm), 6870),]
cv_sample$cardio <- as.factor(cv_sample$cardio)
tune.out <- tune(svm, cardio ~ ., data = cv_sample, ranges = list(cost = c(0.1, 1, 10), gamma = c(0.1, 1, 10), 
                                                                  kernel = c('linear', 'radial')))
tune.out$best.parameters

# The best model by cross validation has the same parameters as what 
#we got with the variations we attempted

#########################END OF SVM HERE#######################################