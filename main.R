train_data <- read.csv("/home/patsac/kuliah/smt6/daming-kom1338/prak/tugasakhir/train.csv")
test_data <- read.csv("/home/patsac/kuliah/smt6/daming-kom1338/prak/tugasakhir/test.csv")
testdata <- sample_n(test_data, 200)

set.seed(13337)
library(dplyr)
strata <- split(train_data, train_data$satisfaction)
summary(sample)

# Menentukan jumlah sampel yang diinginkan untuk setiap strata
samp_size <- c(1000, 1000)

# Mengambil sampel dari setiap strata secara acak
samp <- lapply(names(strata), function(x) {
  stratum <- strata[[x]]
  if(x == 'satisfied') { idx = 1 }
  else { idx = 2 }
  stratum[sample(nrow(stratum), size = samp_size[idx]), ]
  })

# Menggabungkan sampel dari setiap strata menjadi satu data frame
sampled_data <- do.call(rbind, samp)
sampled_data$satisfaction <- as.factor(sampled_data$satisfaction)

# Mencetak hasil sampling
summary(sampled_data)
print(sampled_data)
dataset <- select(sampled_data, id, Type.of.Travel, Class, Inflight.wifi.service, Online.boarding, Inflight.entertainment, Checkin.service, Arrival.Delay.in.Minutes, satisfaction)
dataset$Type.of.Travel <- as.factor(dataset$Type.of.Travel)
dataset$Class <- as.factor(dataset$Class)
summary(dataset)
View(dataset)
library(party)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(readxl)
testdata <- select(testdata, id, Type.of.Travel, Class, Inflight.wifi.service, Online.boarding, Inflight.entertainment, Checkin.service, Arrival.Delay.in.Minutes, satisfaction)
testdata$Type.of.Travel <- as.factor(testdata$Type.of.Travel)
testdata$Class <- as.factor(testdata$Class)
testdata$satisfaction <- as.factor(testdata$satisfaction)

formula <- satisfaction ~ Type.of.Travel + Class + Inflight.wifi.service + Online.boarding + Inflight.entertainment + Checkin.service + Arrival.Delay.in.Minutes

hasil_ctree <- ctree(formula, data=dataset)
plot(hasil_ctree)
ctree_pred <- predict(hasil_ctree, newdata = testdata)
confusionMatrix(ctree_pred, testdata$satisfaction)

hasil_rpart <- rpart(formula, data = dataset,control = rpart.control(minsplit = 100))
rpart.plot(hasil_rpart)
rpart_pred <- predict(hasil_rpart, newdata = testdata, type ="class")
confusionMatrix(rpart_pred, testdata$satisfaction)
