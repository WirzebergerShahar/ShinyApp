#install.packages('rsconnect')
rsconnect::setAccountInfo(name='shaharw30',
                          token='88630C03BEE898D103A5CACCBF9CA32F',
                          secret='yhZ/PhvtG/hAxJFV8cvRbaQJphvoUPO9L1BmoXKD')
library(rsconnect)
#install.packages('glue')
library(glue)
rsconnect::deployApp('C:/Users/ξεθι/Documents/heatmap')



#install.packages('rsconnect')
rsconnect::setAccountInfo(name='shaharw30',
                          token='88630C03BEE898D103A5CACCBF9CA32F',
                          secret='yhZ/PhvtG/hAxJFV8cvRbaQJphvoUPO9L1BmoXKD')
library(rsconnect)
#install.packages('glue')
library(glue)
rsconnect::deployApp('C:/Users/ξεθι/Documents/rshiny')




#
dataset<-read.csv('C:/Users/ξεθι/Documents/rshiny/shiny data.csv')
dataset<-dataset[,c('Category','Country','Currency','Goal','Images','Videos','Succeeded')]
# dividing data to train and test sets
set.seed(1993)
test_len<-nrow(dataset)*0.2
as.integer(test_len)
test_set<- sample (1:nrow(dataset),test_len)
valid_set <- dataset[test_set,]
train_set <- dataset[-test_set,]

##################################     DT      ################################################################################################


tree   <- rpart(train_set$Succeeded ~., data = train_set , method = 'class', parm=list(split='information') )

# train set
preds.tree.train <- predict(tree, newdata = train_set[,1:7], type='class')
preds.tree.test <- predict(tree, newdata = valid_set[,1:7], type='class')

#accuracy
(sum(preds.tree.train==train_set$Succeeded))/nrow(train_set)
(sum(preds.tree.test==valid_set$Succeeded))/nrow(valid_set)

#Confusion Matrix
table(prediction = preds.tree.test, true_values = valid_set$Succeeded)