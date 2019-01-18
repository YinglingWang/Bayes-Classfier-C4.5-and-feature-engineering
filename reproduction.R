---
title: "Project by Yingling Wang"
output:
  word_document: default
  html_document: default
---
```{r}
function_c4.5 <- function(data,train_percent){
  #Installing Caret Package
  library(caret)
  c4.5_list = c()
  for (i in 1:9){
    accuracy_list1 = c()
    for (j in 1:15){
      set.seed(j*10)
      #Data Partitioning
      trainIndices <- createDataPartition(y = data$class, p=train_percent[i], list = FALSE)
      #trainIndices = sample(1:nrow(ecoli),nrow(ecoli)*0.8)
      trainset <- data[trainIndices, ]
      testset <- data[-trainIndices, ]
      
    #C4.5
      # Installing Weka Package for C4.5 Algorithm
      library(RWeka)
      # Applying J48 from RWeka package
      C4.5 <- J48(class ~., data = trainset)
      # Making predictions
      prediction1 <- predict(C4.5, testset, type="class")
      # Creating Confusion Matrix
      cm1 <- table(testset$class, prediction1, dnn = c("Real Values", "Predictions"))
      #Calculating Accuracy
      n1 = diag(cm1)
      accuracy1 <- sum(n1)/sum(cm1)
      accuracy_list1[j] = accuracy1
    
    }
    a1 = sum(accuracy_list1)/15
    c4.5_list[i] = a1
  }
  return(c4.5_list)
}
```

```{r}
function_nbc <- function(data,train_percent){
  #Installing Caret Package
  library(caret)
  nbc_list = c()
  for (i in 1:9){
    accuracy_list2 = c()
    for (j in 1:15){
      set.seed(j*10)
      #Data Partitioning
      trainIndices <- createDataPartition(y = data$class, p=train_percent[i], list = FALSE)
      #trainIndices = sample(1:nrow(ecoli),nrow(ecoli)*0.8)
      trainset <- data[trainIndices, ]
      testset <- data[-trainIndices, ]
      
    #Naive Bayes Classifier
      # Installing e1071
      library(e1071)
      # Applying Naive Bayes
      nbc <- naiveBayes(class ~., data =trainset )
      # Making predictions
      prediction2 <- predict(nbc, testset, type="class")
      cm2 <- table(testset$class, prediction2, dnn = c("Real Values", "Predictions"))
      #Calculating Accuracy
      n2 = diag(cm2)
      accuracy2 <- sum(n2)/sum(cm2)
      accuracy_list2[j] = accuracy2
    }
    a2 = sum(accuracy_list2)/15
    nbc_list[i] = a2
  }
  return(nbc_list)
}
```

```{r}
function_cv_c4.5 <- function(yourData){
  require(caret)
  set.seed(100)
  folds<-createFolds(y=yourData$class,k=5)
  accuracy_list1=c()
#Perform 5 fold cross validation
for(i in 1:5){
    #Segement your data by fold using the which() function 
    train_cv<-yourData[-folds[[i]],]
    test_cv<-yourData[folds[[i]],]
     #C4.5
      # Installing Weka Package for C4.5 Algorithm
      library(RWeka)
      # Applying J48 from RWeka package 
      C4.5 <- J48(class ~., data = train_cv)
      # Making predictions
      prediction1 <- predict(C4.5, test_cv, type="class")
      # Creating Confusion Matrix
      cm1 <- table(test_cv$class, prediction1, dnn = c("Real Values", "Predictions"))
      #Calculating Accuracy
      n1 = diag(cm1)
      accuracy1 <- sum(n1)/sum(cm1)
      accuracy_list1[i] = accuracy1
}
a1 = sum(accuracy_list1)/5
return(a1)
}
```

```{r}
function_cv_nbc <- function(yourData){
  require(caret)
  set.seed(100)
  folds<-createFolds(y=yourData$class,k=5)
  accuracy_list2=c()
#Perform 5 fold cross validation
for(i in 1:5){
    #Segement your data by fold using the which() function 
    train_cv<-yourData[-folds[[i]],]
    test_cv<-yourData[folds[[i]],]
     #Naive Bayes Classifier
      # Installing e1071
      library(e1071)
      # Applying Naive Bayes
      nbc <- naiveBayes(class ~., data =train_cv )
      # Making predictions
      prediction2 <- predict(nbc, test_cv, type="class")
      cm2 <- table(test_cv$class, prediction2, dnn = c("Real Values", "Predictions"))
      #Calculating Accuracy
      n2 = diag(cm2)
      accuracy2 <- sum(n2)/sum(cm2)
      accuracy_list2[i] = accuracy2
}
a2 = sum(accuracy_list2)/5
return(a2)
}
```

```{r}
function_select <- function(data){
  #Installing Caret Package
  library(caret)
  #Data Partitioning
  for (i in 1:5){
    set.seed(i*20)
    trainIndices <- createDataPartition(y = data$class, p=0.1, list = FALSE)
    #trainIndices = sample(1:nrow(ecoli),nrow(ecoli)*0.7)
    trainset <- data[trainIndices, ]
    testset <- data[-trainIndices, ]
    # Installing Weka Package for C4.5 Algorithm
    library(RWeka)
    # Applying J48 from RWeka package
    C4.5 <- J48(class ~., data = trainset)
    library(partykit)
    plot(C4.5)
  }
}
```

```{r}
#define different train percentage
train_percent = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
```
#Ecoli
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/ecoli.data"
ecoli <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ""))
colnames(ecoli) <- c("Sequence_Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2","class")
ecoli
```

```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = ecoli,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final

```

```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = ecoli,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```

```{r}
start_time3 <- Sys.time()
function_select(data=ecoli)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```

```{r}
newdata <- ecoli[c( "alm1","alm2","lip","gvh","mcg","aac","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```

```{r}
  plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Ecoli",ylim=c(70,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(70,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(70,100))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)

```


```{r}
cv_c4.5 = function_cv_c4.5(yourData=ecoli)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=ecoli)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
#German Credit
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
german <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ""))
colnames(german)[21] <- "class"
german
```
```{r}
german[21] <- as.factor(x = german [[21]])
library(plyr)
german$class <- revalue(german$class, c("1" = "good", "2" = "bad" ))
```

```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = german,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = german,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```
```{r}
start_time3 <- Sys.time()
function_select(data =german)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```
```{r}
newdata <- german[c( "V1","V2","V3","V12","V14","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```
```{r}
  plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="German Credit",ylim=c(65,78))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(65,78))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(65,78))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)
```
```{r}
cv_c4.5 = function_cv_c4.5(yourData=german)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=german)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
#Kr vs kp
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/chess/king-rook-vs-king-pawn/kr-vs-kp.data"
krkp <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))
colnames(krkp)[37] <- "class"
krkp
```
```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = krkp,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = krkp,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```
```{r}
start_time3 <- Sys.time()
function_select(data = krkp)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```
```{r}
newdata <- krkp[c( "V10","V21","V33","V32","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```
```{r}
  plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Chess Endgames (kr-vs-kp)",ylim=c(75,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(75,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(75,100))
  legend("bottomleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)

```
```{r}
cv_c4.5 = function_cv_c4.5(yourData=krkp)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=krkp)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/monks-problems/monks-1.test"
monk <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ""))
monk = monk[,1:7]
colnames(monk)[1] <- "class"
monk
```

```{r}
for (i in 1:7){
monk[i] <- as.factor(x = monk[[i]])
}
monk
```

```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = monk,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = monk,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```

```{r}
start_time3 <- Sys.time()
function_select(data=monk)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```
```{r}
newdata <- monk[c( "V2","V3","V4","V6","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```
```{r}
plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Monk",ylim=c(65,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(65,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(65,100))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)
```

```{r}
cv_c4.5 = function_cv_c4.5(yourData=monk)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=monk)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroom <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))
#colnames(ecoli) <- c("Sequence Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2","class")
colnames(mushroom)[1] <- "class"
mushroom
```
```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = mushroom,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```

```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = mushroom,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```
```{r}
start_time3 <- Sys.time()
function_select(data=mushroom)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3

```

```{r}
newdata <- mushroom[c( "V6","V21","V9","V19","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```
```{r}
plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Mushroom",ylim=c(90,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(90,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(90,100))
  legend("bottomleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)
```

```{r}
cv_c4.5 = function_cv_c4.5(yourData=mushroom)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=mushroom)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/molecular-biology/promoter-gene-sequences/promoters.data"
gene <-  as.data.frame(read.table(file = url, header = FALSE, dec = '.', sep = ","))

#colnames(ecoli) <- c("Sequence Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2","class")
#colnames(german)[21] <- "class"
gene
```
```{r}
a = as.matrix(gene[3])
library(stringr)
b=str_sub(a, -57)
b = as.matrix(b)
c<-strsplit(b,'')
d=matrix(nrow=106,ncol=57)
for (i in 1:106){
  for (j in 1:57){
    d[i,j]= c[[i]][j]
  }
}
f= as.data.frame(d)
f
```
```{r}
gene = cbind(gene[1],f)
colnames(gene)[1] <- "class"
gene
dim(gene)
```
```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = gene,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = gene,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```
```{r}
start_time3 <- Sys.time()
function_select(data=gene)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```
```{r}
newdata <- gene[c( "V7","V17","V40","V15","V45","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```
```{r}
plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Promoter Gene Sequences",ylim=c(65,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(65,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(65,100))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)

```
```{r}
cv_c4.5 = function_cv_c4.5(yourData=gene)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=gene)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data"
soybean <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))
#colnames(ecoli) <- c("Sequence Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2","class")
colnames(soybean)[1] <- "class"
soybean

```
```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = soybean,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final

```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = soybean,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```
```{r}
start_time3 <- Sys.time()
function_select(data=soybean)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3

```
```{r}
newdata <- soybean[c( "V3","V14","V33","V27","V6","V16","V15","V27","V19","V4","V23","V30","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final

```
```{r}
plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Soybean",ylim=c(40,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(40,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(40,100))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)
```
```{r}
cv_c4.5 = function_cv_c4.5(yourData=soybean)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=soybean)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
cancer <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))
#colnames(ecoli) <- c("Sequence Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2","class")
colnames(cancer)[11] <- "class"
cancer[11] <- as.factor(x = cancer [[11]])
library(plyr)
cancer$class <- revalue(cancer$class, c("2" = "benign", "4" = "malignant" ))
cancer = cancer[-1]
cancer
```
```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = cancer,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = cancer,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final

```
```{r}
start_time3 <- Sys.time()
function_select(data=cancer)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```
```{r}
newdata <- cancer[c( "V3","V4","V5","V7","V8","V10","class")] 
newdata
```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final
```
```{r}
plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Wisconsin Breast Cancer",ylim=c(90,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(90,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(90,100))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)
```

```{r}
cv_c4.5 = function_cv_c4.5(yourData=cancer)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=cancer)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```
```{r}
#Getting Dataset
url= "https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
vote <-  as.data.frame(read.table(file = url, header = FALSE, dec = ".", sep = ","))
#colnames(ecoli) <- c("Sequence Name", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2","class")
colnames(vote)[1] <- "class"
vote
```
```{r}
library(caret)
start_time1 <- Sys.time()
c4.5_final=function_c4.5(data = vote,train_percent)
end_time1 <- Sys.time()
c4.5_time = end_time1 - start_time1
c4.5_final
```
```{r}
start_time2 <- Sys.time()
nbc_final=function_nbc(data = vote,train_percent)
end_time2 <- Sys.time()
nbc_time = end_time2 - start_time2
nbc_final
```
```{r}
start_time3 <- Sys.time()
function_select(data=vote)
end_time3 <- Sys.time()
select_time = end_time3 - start_time3
```
```{r}
newdata <- vote[c( "V5","V4","V8","class")] 
newdata

```
```{r}
start_time4 <- Sys.time()
sbc_final = function_nbc(data = newdata,train_percent)
end_time4 <- Sys.time()
sbc_time = end_time4 - start_time4 + select_time
nbc_final
sbc_final

```
```{r}
plot(train_percent*100, c4.5_final*100,xlab="Training Data(%)",ylab="Accuracy(%)",type="o",lty= 6, main ="Congressional Voting Records",ylim=c(85,100))
  par(new=TRUE)
  plot(train_percent*100, nbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 2,pch = 3, ylim=c(85,100))
  par(new=TRUE)
  plot(train_percent*100, sbc_final*100,axes = FALSE,xlab = "", ylab = "",type="o",lty= 1,pch = 4, ylim=c(85,100))
  legend("topleft",inset = .01,c("NBC","SBC","C4.5"),lty = c(2,1,6),pch=c(3,4,1),x.intersp=0.5, y.intersp=0.8)
```
```{r}
cv_c4.5 = function_cv_c4.5(yourData=vote)
cv_c4.5
```
```{r}
cv_nbc = function_cv_nbc(yourData=vote)
cv_nbc
```
```{r}
cv_sbc = function_cv_nbc(yourData=newdata)
cv_sbc
```
```{r}
c4.5_time
nbc_time
sbc_time
```










