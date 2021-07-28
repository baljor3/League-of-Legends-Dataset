install.packages("corrplot")
install.packages('caTools')
install.packages('caret')
install.packages('e1071')
install.packages('h2o')

library(h2o)
library(caret)
library(e1071)
library(corrplot)
library(caTools)
source("http://www.sthda.com/upload/rquery_cormat.r")

league<-read.csv("high_diamond_ranked_10min.csv")

# TO check if there is any NAs and compute the mean of the value.
for(j in 3:length(league)){
  for(i in 1:length(league$blueCSPerMin)){
    if(is.na(league[i,j])){
      league[i,j] = mean(league[[j]])
    }
  }
}

#to get the correlation matrix
rquery.cormat(league,type="full")

#the reduce correlation matrix
p<- league[,c('blueWins','blueWardsPlaced','blueWardsDestroyed','blueFirstBlood','blueKills','blueDeaths','blueAssists','blueDragons','blueHeralds','blueTowersDestroyed','blueTotalGold','blueTotalExperience','blueTotalMinionsKilled','blueGoldDiff','blueExperienceDiff')]
rquery.cormat(p)

#split the data
reduceleaguedataset = league[,2:40]
split = sample.split(reduceleaguedataset$blueWins, SplitRatio = .8)
trainingset = subset(reduceleaguedataset, split ==TRUE)
testset = subset(reduceleaguedataset, split == FALSE)

#scaling
trainingset[,2:39] =scale(trainingset[,2:39])
testset[,2:39]= scale(testset[,2:39])

#full model
fullmodel<-glm(formula = blueWins~.,
          data=trainingset,
          family = binomial)
summary(fullmodel)

#to predict the test set
probablity_of_winning = predict(fullmodel,type = 'response',newdata= testset[-1])
# check the accuracy for the logistic regression
pred = ifelse(probablity_of_winning>.5,1,0)

#confusion matrix
cm = table(testset[,1],pred)
cm
(cm[1,1]+cm[2,2])/length(testset$blueWins)

#reduced model
#Why do some have NA's? because some columns are prefectly correlated to each other.
#Blue Herald and blue Elite Monsters are perfectly correlated. Hence, the NA in the logistic formula.
Reducemodel<-glm(formula = blueWins ~blueDeaths+blueDragons+blueHeralds+blueGoldDiff+blueExperienceDiff+redTowersDestroyed+redEliteMonsters,
          data=trainingset,
          family = binomial)
summary(Reducemodel)
probablity_of_winning = predict(Reducemodel,type = 'response',newdata= testset[,-1])

# check the accuracy for the logistic regression
pred = ifelse(probablity_of_winning>.5,1,0)
cm = table(testset[,1],pred)

#73% accuracy
(cm[1,1]+cm[2,2])/length(testset$blueWins)

#using PCA to plot 
pca =preProcess(x = trainingset[,-1],method ='pca',pcaComp = 2)
trainingset = predict(pca,trainingset)
trainingset = trainingset[,c(2,3,1)]
testset = predict(pca,testset)
testset = testset[,c(2,3,1)]
newg<-glm(formula = blueWins ~.,
          data=trainingset,
          family = binomial)
set = testset
x1 =seq(min(set[,1])-1,max(set[,1])+1, by=.01)
x2 =seq(min(set[,2])-1,max(set[,2])+1, by=.01)
gridset =expand.grid(x1,x2)
colnames(gridset) = c('PC1', 'PC2')
prob_set = predict(newg, type = 'response', newdata = gridset)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (test set)',
     xlab = 'PCA1', ylab = 'PCA2',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(gridset, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#Neural Networks
#connecting 
h2o.init(nthreads = -1)
#the neural network as.h2o changes the data frame to a h2o frame because h2o frame can only be used
netural = h2o.deeplearning(y='blueWins',
                           training_frame = as.h2o(trainingset),
                           activation = 'Rectifier',
                           hidden = c(20,20),
                           epochs = 100,
                           train_samples_per_iteration = -2)
#predict
pre= h2o.predict(netural,newdata =as.h2o(testset[,-1]))
prediction = ifelse(pre>.5,1,0)
#changing h2o object to normal vector
prediction =as.vector(prediction)
#accuracy
cm = table(testset[,1],prediction)
#69.5% accuracy
(cm[1,1]+cm[2,2])/length(testset$blueWins)

#disconnecting
h2o.shutdown()

#data analysis

names(league)
i =3
for(i in 3:length(league)){
  d=names(league)[i]
  c=league[,c(2,i)]
  fit = glm(blueWins ~ ., data=c, family=binomial)
  newdat <- data.frame(d=seq(min(c[,2]), max(c[,2]),len=9879))
  names(newdat)[1]=names(league)[i]
  newdat$blueWins = predict(fit, newdata=newdat, type="response")
  plot(blueWins ~ ., data=c, col="red4")
  lines(blueWins ~ ., data=newdat, col="green4", lwd=2)
  abline(h=.5,col ='red',lwd = 4, lty = 1)
}

