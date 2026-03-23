library(ISLR2)
head(Boston)
?Boston
class(Boston)

library(randomForest)

df = Boston

total_size = nrow(df)
train_size = 400

# sample training set index, replace = FALSE makes no duplicated training sample.
train_ind = sample(1:total_size, train_size, replace = FALSE)

# test set index
test_ind = setdiff(1:total_size, train_ind)

test_ind = setdiff(1:total_size, train_ind)

rf_boston1 = randomForest(medv ~., data = df, subset = train_ind, mtry = 12,
                          ntree = 100, importance = TRUE)

rf_boston1
importance(rf_boston1)

varImpPlot(rf_boston1)

# test set prediction
rf_boston1_pre = predict(rf_boston1, newdata = df[test_ind,])


# another way to write the Random Forest without setting up a data.frame
y_train = df[train_ind, "medv"]

x_train = df[train_ind, 1:12]

rf_boston2 = randomForest(x = x_train, y = y_train, mtry = 6,
                          ntree = 100, importance = TRUE)

rf_boston2
importance(rf_boston2)

x_test = df[test_ind, 1:12]
y_test = df[test_ind, "medv"]

rf_boston2_pre = predict(rf_boston2, newdata = x_test)


par(mfrow = c(1,1))
plot(y_test, type = "l")
lines(rf_boston2_pre, col = "red")
