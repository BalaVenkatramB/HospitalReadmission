install.packages("corrplot")
library(corrplot)
install.packages('nnet')
library(nnet)
install.packages("mlogit")
library(mlogit)
#Setting the path from where we read the file
setwd("/Users/balavenkatrambalantrapu/Desktop")

#Reading the file after imputing the missing values and removing non significant coloumns
dat1<-read.csv("final.csv", header=TRUE, strip.white = TRUE,na.strings = c(""," ",".","NA"))
summary(dat1)

# Vuew the data initially before we perform our analysis 
View(dat1)

#Combining all the numerical variables to perform the correlation analysis
dat2<-cbind.data.frame(dat1$time_in_hospital,dat1$num_lab_procedures,dat1$num_medications, dat1$number_diagnoses, dat1$number_emergency, dat1$number_inpatient,dat1$number_outpatient,dat1$readmitted)

#Plotting the corrplot
corrplot(cor(dat2),"circle","full")
d<-pairs(dat2)

#Performing Chi-Sq test on the categoroical variables
ch<-chisq.test(dat3)
summary(ch)

#Performing Logistic Regression on all the categorical variables 
dat3<-cbind.data.frame(dat1$race,dat1$gender,dat1$age, dat1$max_glu_serum,dat1$A1Cresult,dat1$insulin,dat1$diabetesMed,dat1$change,dat1$readmitted)
typeof(dat3$`dat1$max_glu_serum`)
x<-na.omit(dat3)
for(i in 1:9){
  x[,i]<-as.factor(x[,i])
}
dat4<-glm(dat1$readmitted~., data=dat3)
summary(dat4)


#Performing Linear Modeling on all the numerical variables
dat5<-lm(readmitted~time_in_hospital+num_lab_procedures+num_procedures+num_medications+number_emergency+number_inpatient , data=dat1)
summary(dat5)


#Performing Principal component Analysis
install.packages("stats")
library(stats)
pc1 <-  principal(dat1, nfactors = 3, rotate="none")
summary(pc1)
plot(pc1$values,type="b")



#Model Building
#Partitioning of data into training and Testing data 
testdata<-sample_frac(f, 0.2)
sid <-as.numeric(rownames(testdata))
traindata <- f[-sid,]
View(traindata)

#Decision Tree Model
library(ROCR)
library(rpart)
fit <- ctree(traindata$readmitted~., data =traindata, control= ctree_control(mincriterion = .99, minsplit = 1000))
varimp(w)
info.gain.ctree(fit)
varImpPlot(fit)
summary(fit)

#Predict Output 
predicted= predict(fit, traindata, type='prob')
roc<-performance(predicted, "tpr", "fpr")
plot(roc,)
w<-weights(fit)
tab<-table(predicted,testdata$readmitted)
tab

predicted
error = mean(predicted != testdata$readmitted)
error
dev.off()
#Plot the output
plot(fit)
plot(fit, type="simple",           # no terminal plots
     inner_panel=node_inner(fit,
                            abbreviate = F,            # short variable names
                            pval = T,                 # no p-values
                            id = T),                  # no id of node
     terminal_panel=node_terminal(fit, 
                                  abbreviate =T,
                                  digits = 1,                   # few digits on numbers
                                  fill = c("grey"),            # make box white not grey
                                  id = FALSE)
)






#RandomForest
install.packages("randomForest")
library(randomForest)
f$dia
which( colnames(f)==c("diag_3_desc","diag_2_desc" ))
f<-subset(f[,-c(37,38,39)])
fit <- randomForest(traindata$readmitted~., data =traindata, controls = ctree_control(mincriterion = .99, minsplit = 1500), proximity=TRUE)
cforest(traindata$readmitted~., data =traindata, controls = cforest_control(mincriterion = .99, minsplit = 500))
varImpPlot(fit)
summary(fit)

#Predict Output 
predicted= predict(fit,testdata)
tab<-table(predicted,testdata$readmitted)
tab
randomForest::getTree(fit)
randomForest::partialPlot(traindata$readmitted~., data =traindata, controls = ctree_control(mincriterion = .95, minsplit = 500))
predicted
error = mean(predicted != testdata$readmitted)
error
#Plot the output
MDSplot(fit, traindata$readmitted)


## Naive Bayesian

library(e1071)

classifier<-naiveBayes(traindata[,1:48], traindata[,49]) 

classifier<-naiveBayes(change ~.,data=traindata1,usekernel=T)  
plot(classifier)

table(predict(classifier, testdata[,-20]), testdata[,20])
test_predictions = predict(classifier, testdata, type = "class") 
test_error = sum(test_predictions != testdata$readmitted)/nrow(testdata) 
test_error

##Logistic

install.packages("mlogit")
log <- glm(readmitted~.,data=data,family = binomial(logit))
prob=predict(log,type=c("response"))
data$prob=prob
install.packages("pROC")
library(pROC)
g <- roc(readmitted ~ prob, data = data)
plot(g)   





##Additional Task 1 & 2
d<-read.csv("ram.csv", header=TRUE, strip.white = TRUE,na.strings = c(""," ",".","NA"))
d1<-d$num_medications
d1<-cbind(d$num_procedures,d$number_diagnoses, d$number_emergency, d$number_inpatient, d$number_outpatient)
d1[,-3]
d2<-d1[,1]
as.matrix(d1)
install.packages("mvoutlier")
library(mvoutlier)
uni.plot(d)
boxplot(d)
View(d)
install.packages("mlogit")
library(mlogit)
g<-lm(d$time_in_hospital~d$num_lab_procedures+d$num_medications+d$num_procedures+d$number_diagnoses+ d$number_emergenc+d$number_inpatient+d$number_outpatient, family="binomial")
summary(g)
dist=daisy(d, stand=TRUE,metric=c("gower"))
library(HSAUR)
library(cluster)
library(daisy)
out2=outliers.ranking(dist,method="sizeDiff",meth="ward.D")             
as.table(head(out2$rank.outliers,100))
as.table(head(out2$prob.outliers,100))


patternData <- subset(d[,c(3,4,5,19,33,36,37,40,38)])
install.packages("arulesViz")

library(arulesViz)

patternPatients <- as(patternData,"transactions")
rules<-apriori(patternPatients,
               parameter=list(minlen=2,support=0.1,confidence=0.6),
               appearance=list (rhs=c("readmitted=TRUE",
                                      "readmitted=FALSE"),
                                default="lhs"))






patternData <- subset(d[,c(3,4,5,19,33,36,37,40,38)])
install.packages("arulesViz")

library(arulesViz)

patternPatients <- as(patternData,"transactions")
rules<-apriori(patternPatients,
               parameter=list(minlen=2,support=0.1,confidence=0.6),
               appearance=list (rhs=c("readmitted=TRUE",
                                      "readmitted=FALSE"),
                                default="lhs"))
