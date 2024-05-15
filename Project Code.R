library(broom)
library(caret)
library(dplyr)
library(lmtest)
library(ggplot2)
library(DMwR2)
library(ROSE)  
library(randomForest)
library(rpart)
library(rpart.plot)
library(partykit)
library(shiny)
library(shinydashboard)
library(plotly)



baseyear <- read.csv('/Users/amandatran0727/Desktop/DATA Capstone/Updated Master Data.csv')

data1 <- read.csv("/Users/amandatran0727/Desktop/DATA Capstone/trial.csv") 
data1 <- data1 %>% dplyr::select(-c("X"))

data1[,] <- lapply(data1[,], as.factor)
names(data1) <- tolower(names(data1))

data2 <- data1 %>% dplyr::select(c("bys14", "bys20e", "bys20h", "bys20i", "bys20j", "bys20k",
                                   "bys20m", "bys21b", "bys21c", "bys21d", "bys21e", "bys22a", "bys23a", "bys23b", "bys23c", "bys23d", "bys23e", "bys23f",
                                   "bys24f", "bys24g", "bys27h", "bys28", "bys37",
                                   "bys33a", "bys33b", "bys33c", "bys33d", "bys33e", "bys33f", "bys33g", "bys33h", "bys33i", "bys33j", "bys33k", "bys33l", "bys71g", "bys35a", "bys36a", "bys39",
                                   "bys41", "bys42", "bys43", "bys44a", "bys44c", "bys44e",
                                   "bys44f", "bys44g", "bys45c", "bys46a", "bys46b", "bys48a", "bys48b",
                                   "bys49a", "bys49b", "bys56", "bys83b",
                                   "bys85a", "bys85b","f1doqflg"))

# baseyear <- read.csv('/Users/amandatran0727/Desktop/DATA Capstone/Updated Master Data.csv')

predictors <- c(
  "bys14",
  "bys20a", "bys20b", "bys20c", "bys20d", "bys20e", "bys20f", "bys20g", "bys20h", "bys20i", "bys20j", "bys20k", "bys20l", "bys20m", "bys20n",
  "bys21a", "bys21b", "bys21c", "bys21d", "bys21e",
  "bys22a", "bys22b", "bys22c", "bys22d", "bys22e", "bys22f", "bys22g", "bys22h",
  "bys27a", "bys27b", "bys27c", "bys27d", "bys27e", "bys27f", "bys27g", "bys27h", "bys27i", "bys28", "bys37",
  "bys33a", "bys33b", "bys33c", "bys33d", "bys33e", "bys33f", "bys33g", "bys33h", "bys33i", "bys33j", "bys33k", "bys33l",
  "bys71g"
)

# Define the list of titles
pred_titles <- c(
  "Sex of Student",
  "Students get along well with teachers",
  "There is real school spirit",
  "Students friendly with other racial groups",
  "Other students often disrupt class",
  "The teaching is good",
  "Teachers are interested in students",
  "Teachers praise effort",
  "In class often feels put down by teachers",
  "In class often feels put down by students",
  "Does not feel safe at this school",
  "Disruptions get in way of learning",
  "Misbehaving students often get away with it",
  "There are gangs in school",
  "Racial/ethnic groups often fight",
  "Everyone knows what school rules are",
  "School rules are fair",
  "Punishment same no matter who you are",
  "School rules are strictly enforced",
  "Students know punishment for broken rules",
  "Had something stolen at school",
  "Someone offered drugs at school",
  "Someone threatened to hurt 10th-grader at school",
  "Got into a physical fight at school",
  "Someone hit 10th-grader",
  "Someone forced money/things from 10th-grader",
  "Someone damaged belongings",
  "Someone bullied or picked on 10th-grader",
  "Classes are interesting and challenging",
  "Satisfied by doing what expected in class",
  "Has nothing better to do than school",
  "Education is important to get a job later",
  "School is a place to meet friends",
  "Plays on a team or belongs to a club",
  "Learns skills for job in school",
  "Teachers expect success in school",
  "Parents expect success in school",
  "How much likes school",
  "Importance of good grades to student",
  "Ever in Advanced Placement program",
  "Ever in International Baccalaureate program",
  "Ever in part-time program at regional vocational school",
  "Ever in a remedial English class",
  "Ever in a remedial math class",
  "Ever in bilingual/bicultural class",
  "Ever in English as Second Language program",
  "Ever in dropout prevention program",
  "Ever in special education program",
  "Ever in distance learning course",
  "Ever in career academy",
  "Ever in program to help prepare for college",
  "Did not participate in these work-based learning experiences"
)

# Create the data frame
predictors_df <- data.frame(Variable = predictors, Value = pred_titles)


for (var in predictors) {
  baseyear[[var]] <- ifelse(baseyear[[var]] < 0, NA, baseyear[[var]])
}

baseyear <- na.omit(baseyear)


# List of variables
vars <- c("bys54a", "bys54b", "bys54c", "bys54d", "bys54e", "bys54f", "bys54g", "bys54h", "bys54i", "bys54j", "bys54k", "bys54l", "bys54n", "bys54o")


for(var in vars) {
  baseyear[[var]] <- ifelse(baseyear[[var]] == 3, 1, 0)
}

baseyear[,] <- lapply(baseyear[,], factor)

#logit ----
# data1 <- baseyear %>%
#   dplyr::select(c("bys14", "bys20e", "bys20h", "bys20i", "bys20j", "bys20k",
#                   "bys20m", "bys21b", "bys21c", "bys21d", "bys21e", "bys22a", starts_with("bys23"),
#                   "bys24f", "bys24g", "bys27h", "bys28", "bys37",
#                   starts_with("bys33"), "bys71g", "bys35a", "bys35b", "bys36a", "bys36b", "bys39",
#                   "bys41", "bys42", "bys43", "bys44a", "bys44c", "bys44e",
#                   "bys44f", "bys44g", "bys45c", "bys46a", "bys46b", "bys48a", "bys48b",
#                   "bys49a", "bys49b", "bys56", "bys57", "bys58", "bys60", "bys83b",
#                   "bys85a", "bys85b", "f1doqflg"))
# 
# 
# data1[,] <- lapply(data1[,], as.numeric)
# 
# for (col in names(data1)) {
#   data1[[col]][data1[[col]] < 0] <- NA
# }
# 
# data1 <- na.omit(data1)
# 
# data1[,] <- lapply(data1[,], as.factor)

# # Combine categories for BYS24F and BYS24G
#data1$BYS24F <- ifelse(data1$BYS24F == 1, 1, 2)
#data1$BYS24F <- droplevels(as.factor(data1$BYS24F), exclude = c(3, 4, 5))

#data1$BYS24G <- ifelse(data1$BYS24G == 1, 1, 2)
#data1$BYS24G <- droplevels(as.factor(data1$BYS24G), exclude = c(3, 4, 5))

logit_full <- glm(f1doqflg ~ ., data = data2, family = "binomial")
summary(logit_full)


# ROSE for 54a ----

# Resampling and modeling for variable "bys54a"
new54a <- baseyear[, c(predictors, "bys54a")]

# Split data into train and test sets
train_index_54a <- createDataPartition(new54a$bys54a, p = 0.7, list = FALSE)
train_data_54a <- new54a[train_index_54a, ]
test_data_54a <- new54a[-train_index_54a, ]

X_train_54a <- train_data_54a[, !names(train_data_54a) %in% "bys54a"]
y_train_54a <- train_data_54a$bys54a
X_test_54a <- test_data_54a[, !names(test_data_54a) %in% "bys54a"]
y_test_54a <- test_data_54a$bys54a

# Perform ROSE on training data
train_rose_54a <- ROSE(bys54a ~ ., data = train_data_54a)
train_data_54a <- train_rose_54a$data

#model_54a <- rpart(bys54a~., data = train_data_54a, method = "class")
#rpart.plot(model_54a, extra = 101, fallen.leaves = TRUE, tweak = 1.5, main = "Decision Tree ROSE 54a", cex = 0.7)

rf_54a <- randomForest(bys54a ~ ., data = train_data_54a, ntree = 100)

# Predictions
test_pred_54a <- predict(rf_54a, newdata = test_data_54a, type = "class")

# Confusion matrix
conf_mat_54a <- table(Actual = test_data_54a$bys54a, Predicted = test_pred_54a)

# Extract TP and FN from the confusion matrix
#TP_54a <- conf_mat_54a["1", "1"]
#FN_54a <- conf_mat_54a["1", "0"]

# Calculate TPR
#TPR_54a <- #TP_54a / (#TP_54a + #FN_54a)

# Print the TPR
##print(#TPR_54a)

# Plot variable importance
#varImpPlot(rf_54a, n.var = 5) 

# ROSE for 54b ----
# Resampling and modeling for variable "bys54b"
new54b <- baseyear[, c(predictors, "bys54b")]

# Split data into train and test sets
train_index_54b <- createDataPartition(new54b$bys54b, p = 0.7, list = FALSE)
train_data_54b <- new54b[train_index_54b, ]
test_data_54b <- new54b[-train_index_54b, ]

X_train_54b <- train_data_54b[, !names(train_data_54b) %in% "bys54b"]
y_train_54b <- train_data_54b$bys54b
X_test_54b <- test_data_54b[, !names(test_data_54b) %in% "bys54b"]
y_test_54b <- test_data_54b$bys54b

# Perform ROSE on training data
train_rose_54b <- ROSE(bys54b ~ ., data = train_data_54b)
train_data_54b <- train_rose_54b$data

#model_54b <- rpart(bys54b~., data = train_data_54b, method = "class")
#rpart.plot(model_54b, extra = 101, fallen.leaves = TRUE, main = "Decision Tree ROSE 54b", cex = 0.7)

rf_54b <- randomForest(bys54b ~ ., data = train_data_54b, ntree = 100)

# Predictions
test_pred_54b <- predict(rf_54b, newdata = test_data_54b, type = "class")

# Confusion matrix
conf_mat_54b <- table(Actual = test_data_54b$bys54b, Predicted = test_pred_54b)

# Extract TP and FN from the confusion matrix
#TP_54b <- conf_mat_54b["1", "1"]
#FN_54b <- conf_mat_54b["1", "0"]

# Calculate TPR
#TPR_54b <- #TP_54b / (#TP_54b + #FN_54b)

# Print the TPR
#print(#TPR_54b)

# Plot variable importance
#varImpPlot(rf_54b, n.var=5) 

# RF for 54c ----
# Resampling and modeling for variable "bys54c"
new54c <- baseyear[, c(predictors, "bys54c")]

# Split data into train and test sets
train_index_54c <- createDataPartition(new54c$bys54c, p = 0.7, list = FALSE)
train_data_54c <- new54c[train_index_54c, ]
test_data_54c <- new54c[-train_index_54c, ]

X_train_54c <- train_data_54c[, !names(train_data_54c) %in% "bys54c"]
y_train_54c <- train_data_54c$bys54c
X_test_54c <- test_data_54c[, !names(test_data_54c) %in% "bys54c"]
y_test_54c <- test_data_54c$bys54c

#model_54c <- rpart(bys54c~., data = train_data_54c, method = "class")
#rpart.plot(model_54c, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54c", cex = 0.7)

rf_54c <- randomForest(bys54c ~ ., data = train_data_54c, ntree = 100)

# Predictions
test_pred_54c <- predict(rf_54c, newdata = test_data_54c, type = "class")

# Confusion matrix
conf_mat_54c <- table(Actual = test_data_54c$bys54c, Predicted = test_pred_54c)

# Extract TP and FN from the confusion matrix
#TP_54c <- conf_mat_54c["1", "1"]
#FN_54c <- conf_mat_54c["1", "0"]

# Calculate TPR
#TPR_54c <- #TP_54c / (#TP_54c + #FN_54c)

# Print the TPR
#print(#TPR_54c)

# Plot variable importance
#varImpPlot(rf_54c, n.var=5) 

# RF for 54d ----

# Resampling and modeling for variable "bys54d"
new54d <- baseyear[, c(predictors, "bys54d")]

# Split data into train and test sets
train_index_54d <- createDataPartition(new54d$bys54d, p = 0.7, list = FALSE)
train_data_54d <- new54d[train_index_54d, ]
test_data_54d <- new54d[-train_index_54d, ]

X_train_54d <- train_data_54d[, !names(train_data_54d) %in% "bys54d"]
y_train_54d <- train_data_54d$bys54d
X_test_54d <- test_data_54d[, !names(test_data_54d) %in% "bys54d"]
y_test_54d <- test_data_54d$bys54d

# Perform ROSE on training data
train_rose_54d <- ROSE(bys54d ~ ., data = train_data_54d)
train_data_54d <- train_rose_54d$data

#model_54d <- rpart(bys54d~., data = train_data_54d, method = "class")
#rpart.plot(model_54d, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54d", cex = 0.7)

rf_54d <- randomForest(bys54d ~ ., data = train_data_54d, ntree = 100)

# Predictions
test_pred_54d <- predict(rf_54d, newdata = test_data_54d, type = "class")

# Confusion matrix
conf_mat_54d <- table(Actual = test_data_54d$bys54d, Predicted = test_pred_54d)

# Extract TP and FN from the confusion matrix
#TP_54d <- conf_mat_54d["1", "1"]
#FN_54d <- conf_mat_54d["1", "0"]

# Calculate TPR
#TPR_54d <- #TP_54d / (#TP_54d + #FN_54d)

# Print the TPR
#print(#TPR_54d)

# Plot variable importance
#varImpPlot(rf_54d, n.var=5) 

# RF for 54e ----
# Resampling and modeling for variable "bys54e"
new54e <- baseyear[, c(predictors, "bys54e")]

# Split data into train and test sets
train_index_54e <- createDataPartition(new54e$bys54e, p = 0.7, list = FALSE)
train_data_54e <- new54e[train_index_54e, ]
test_data_54e <- new54e[-train_index_54e, ]

X_train_54e <- train_data_54e[, !names(train_data_54e) %in% "bys54e"]
y_train_54e <- train_data_54e$bys54e
X_test_54e <- test_data_54e[, !names(test_data_54e) %in% "bys54e"]
y_test_54e <- test_data_54e$bys54e

# Perform ROSE on training data
train_rose_54e <- ROSE(bys54e ~ ., data = train_data_54e)
train_data_54e <- train_rose_54e$data

#model_54e <- rpart(bys54e~., data = train_data_54e, method = "class")
#rpart.plot(model_54e, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54e", cex = 0.7)

rf_54e <- randomForest(bys54e ~ ., data = train_data_54e, ntree = 100)

# Predictions
test_pred_54e <- predict(rf_54e, newdata = test_data_54e, type = "class")

# Confusion matrix
conf_mat_54e <- table(Actual = test_data_54e$bys54e, Predicted = test_pred_54e)

# Extract TP and FN from the confusion matrix
#TP_54e <- conf_mat_54e["1", "1"]
#FN_54e <- conf_mat_54e["1", "0"]

# Calculate TPR
#TPR_54e <- #TP_54e / (#TP_54e + #FN_54e)

# Print the TPR
#print(#TPR_54e)

# Plot variable importance
#varImpPlot(rf_54e, n.var=5) 


# RF for 54f ----
# Resampling and modeling for variable "bys54f"
new54f <- baseyear[, c(predictors, "bys54f")]

# Split data into train and test sets
train_index_54f <- createDataPartition(new54f$bys54f, p = 0.7, list = FALSE)
train_data_54f <- new54f[train_index_54f, ]
test_data_54f <- new54f[-train_index_54f, ]

X_train_54f <- train_data_54f[, !names(train_data_54f) %in% "bys54f"]
y_train_54f <- train_data_54f$bys54f
X_test_54f <- test_data_54f[, !names(test_data_54f) %in% "bys54f"]
y_test_54f <- test_data_54f$bys54f

#model_54f <- rpart(bys54f~., data = train_data_54f, method = "class")
#rpart.plot(model_54f, extra = 101, fallen.leaves = TRUE, tweak = 1.5, main = "Decision Tree 54f", cex = 0.7)

rf_54f <- randomForest(bys54f ~ ., data = train_data_54f, ntree = 100)

# Predictions
test_pred_54f <- predict(rf_54f, newdata = test_data_54f, type = "class")

# Confusion matrix
conf_mat_54f <- table(Actual = test_data_54f$bys54f, Predicted = test_pred_54f)

# Extract TP and FN from the confusion matrix
#TP_54f <- conf_mat_54f["1", "1"]
#FN_54f <- conf_mat_54f["1", "0"]

# Calculate TPR
#TPR_54f <- #TP_54f / (#TP_54f + #FN_54f)

# Print the TPR
#print(#TPR_54f)

# Plot variable importance
#varImpPlot(rf_54f, n.var =5)

# ROSE for 54g ----
# Resampling and modeling for variable "bys54g"
new54g <- baseyear[, c(predictors, "bys54g")]

# Split data into train and test sets
train_index_54g <- createDataPartition(new54g$bys54g, p = 0.7, list = FALSE)
train_data_54g <- new54g[train_index_54g, ]
test_data_54g <- new54g[-train_index_54g, ]

X_train_54g <- train_data_54g[, !names(train_data_54g) %in% "bys54g"]
y_train_54g <- train_data_54g$bys54g
X_test_54g <- test_data_54g[, !names(test_data_54g) %in% "bys54g"]
y_test_54g <- test_data_54g$bys54g

# Perform ROSE on training data
train_rose_54g <- ROSE(bys54g ~ ., data = train_data_54g)
train_data_54g <- train_rose_54g$data

#model_54g <- rpart(bys54g~., data = train_data_54g, method = "class")
#rpart.plot(model_54g, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54g", cex = 0.7)

rf_54g <- randomForest(bys54g ~ ., data = train_data_54g, ntree = 100)

# Predictions
test_pred_54g <- predict(rf_54g, newdata = test_data_54g, type = "class")

# Confusion matrix
conf_mat_54g <- table(Actual = test_data_54g$bys54g, Predicted = test_pred_54g)

# Extract TP and FN from the confusion matrix
#TP_54g <- conf_mat_54g["1", "1"]
#FN_54g <- conf_mat_54g["1", "0"]

# Calculate TPR
#TPR_54g <- #TP_54g / (#TP_54g + #FN_54g)

# Print the TPR
#print(#TPR_54g)

# Plot variable importance
#varImpPlot(rf_54g, n.var=5)

# ROSE for 54h ----
# Resampling and modeling for variable "bys54h"
new54h <- baseyear[, c(predictors, "bys54h")]

# Split data into train and test sets
train_index_54h <- createDataPartition(new54h$bys54h, p = 0.7, list = FALSE)
train_data_54h <- new54h[train_index_54h, ]
test_data_54h <- new54h[-train_index_54h, ]

X_train_54h <- train_data_54h[, !names(train_data_54h) %in% "bys54h"]
y_train_54h <- train_data_54h$bys54h
X_test_54h <- test_data_54h[, !names(test_data_54h) %in% "bys54h"]
y_test_54h <- test_data_54h$bys54h

# Perform ROSE on training data
train_rose_54h <- ROSE(bys54h ~ ., data = train_data_54h)
train_data_54h <- train_rose_54h$data

#model_54h <- rpart(bys54h~., data = train_data_54h, method = "class")
#rpart.plot(model_54h, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54h", cex = 0.7)

rf_54h <- randomForest(bys54h ~ ., data = train_data_54h, ntree = 100)

# Predictions
test_pred_54h <- predict(rf_54h, newdata = test_data_54h, type = "class")

# Confusion matrix
conf_mat_54h <- table(Actual = test_data_54h$bys54h, Predicted = test_pred_54h)

# Extract TP and FN from the confusion matrix
#TP_54h <- conf_mat_54h["1", "1"]
#FN_54h <- conf_mat_54h["1", "0"]

# Calculate TPR
#TPR_54h <- #TP_54h / (#TP_54h + #FN_54h)

# Print the TPR
#print(#TPR_54h)

# Plot variable importance
#varImpPlot(rf_54h, n.var=5)

# ROSE for 54i ----
# Resampling and modeling for variable "bys54i"
new54i <- baseyear[, c(predictors, "bys54i")]

# Split data into train and test sets
train_index_54i <- createDataPartition(new54i$bys54i, p = 0.7, list = FALSE)
train_data_54i <- new54i[train_index_54i, ]
test_data_54i <- new54i[-train_index_54i, ]

X_train_54i <- train_data_54i[, !names(train_data_54i) %in% "bys54i"]
y_train_54i <- train_data_54i$bys54i
X_test_54i <- test_data_54i[, !names(test_data_54i) %in% "bys54i"]
y_test_54i <- test_data_54i$bys54i

# Perform ROSE on training data
train_rose_54i <- ROSE(bys54i ~ ., data = train_data_54i)
train_data_54i <- train_rose_54i$data

#model_54i <- rpart(bys54i~., data = train_data_54i, method = "class")
#rpart.plot(model_54i, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54i", cex = 0.7)

rf_54i <- randomForest(bys54i ~ ., data = train_data_54i, ntree = 100)

# Predictions
test_pred_54i <- predict(rf_54i, newdata = test_data_54i, type = "class")

# Confusion matrix
conf_mat_54i <- table(Actual = test_data_54i$bys54i, Predicted = test_pred_54i)

# Extract TP and FN from the confusion matrix
#TP_54i <- conf_mat_54i["1", "1"]
#FN_54i <- conf_mat_54i["1", "0"]

# Calculate TPR
#TPR_54i <- #TP_54i / (#TP_54i + #FN_54i)

# Print the TPR
#print(#TPR_54i)

# Plot variable importance
#varImpPlot(rf_54i, n.var=5)

# ROSE for 54j ----
# Resampling and modeling for variable "bys54j"
new54j <- baseyear[, c(predictors, "bys54j")]

# Split data into train and test sets
train_index_54j <- createDataPartition(new54j$bys54j, p = 0.7, list = FALSE)
train_data_54j <- new54j[train_index_54j, ]
test_data_54j <- new54j[-train_index_54j, ]

X_train_54j <- train_data_54j[, !names(train_data_54j) %in% "bys54j"]
y_train_54j <- train_data_54j$bys54j
X_test_54j <- test_data_54j[, !names(test_data_54j) %in% "bys54j"]
y_test_54j <- test_data_54j$bys54j

# Perform ROSE on training data
train_rose_54j <- ROSE(bys54j ~ ., data = train_data_54j)
train_data_54j <- train_rose_54j$data

#model_54j <- rpart(bys54j~., data = train_data_54j, method = "class")
#rpart.plot(model_54j, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54j", cex = 0.7)

rf_54j <- randomForest(bys54j ~ ., data = train_data_54j, ntree = 100)

# Predictions
test_pred_54j <- predict(rf_54j, newdata = test_data_54j, type = "class")

# Confusion matrix
conf_mat_54j <- table(Actual = test_data_54j$bys54j, Predicted = test_pred_54j)

# Extract TP and FN from the confusion matrix
#TP_54j <- conf_mat_54j["1", "1"]
#FN_54j <- conf_mat_54j["1", "0"]

# Calculate TPR
#TPR_54j <- #TP_54j / (#TP_54j + #FN_54j)

# Print the TPR
#print(#TPR_54j)

# Plot variable importance
#varImpPlot(rf_54j, n.var=5)


# RF for 54k ----
# Resampling and modeling for variable "bys54k"
new54k <- baseyear[, c(predictors, "bys54k")]

# Split data into train and test sets
train_index_54k <- createDataPartition(new54k$bys54k, p = 0.7, list = FALSE)
train_data_54k <- new54k[train_index_54k, ]
test_data_54k <- new54k[-train_index_54k, ]

X_train_54k <- train_data_54k[, !names(train_data_54k) %in% "bys54k"]
y_train_54k <- train_data_54k$bys54k
X_test_54k <- test_data_54k[, !names(test_data_54k) %in% "bys54k"]
y_test_54k <- test_data_54k$bys54k

#model_54k <- rpart(bys54k~., data = train_data_54k, method = "class")
#rpart.plot(model_54k, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54k", cex = 0.7)

rf_54k <- randomForest(bys54k ~ ., data = train_data_54k, ntree = 100)

# Predictions
test_pred_54k <- predict(rf_54k, newdata = test_data_54k, type = "class")

# Confusion matrix
conf_mat_54k <- table(Actual = test_data_54k$bys54k, Predicted = test_pred_54k)

# Extract TP and FN from the confusion matrix
#TP_54k <- conf_mat_54k["1", "1"]
#FN_54k <- conf_mat_54k["1", "0"]

# Calculate TPR
#TPR_54k <- #TP_54k / (#TP_54k + #FN_54k)

# Print the TPR
#print(#TPR_54k)

# Plot variable importance
#varImpPlot(rf_54k, n.var = 5)

# ROSE RF for 54l ----
# Resampling and modeling for variable "bys54l"
new54l <- baseyear[, c(predictors, "bys54l")]

# Split data into train and test sets
train_index_54l <- createDataPartition(new54l$bys54l, p = 0.7, list = FALSE)
train_data_54l <- new54l[train_index_54l, ]
test_data_54l <- new54l[-train_index_54l, ]

X_train_54l <- train_data_54l[, !names(train_data_54l) %in% "bys54l"]
y_train_54l <- train_data_54l$bys54l
X_test_54l <- test_data_54l[, !names(test_data_54l) %in% "bys54l"]
y_test_54l <- test_data_54l$bys54l

# Perform ROSE on training data
train_rose_54l <- ROSE(bys54l ~ ., data = train_data_54l)
train_data_54l <- train_rose_54l$data

#model_54l <- rpart(bys54l~., data = train_data_54l, method = "class")
#rpart.plot(model_54l, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54l", cex = 0.7)

rf_54l <- randomForest(bys54l ~ ., data = train_data_54l, ntree = 100)

# Predictions
test_pred_54l <- predict(rf_54l, newdata = test_data_54l, type = "class")

# Confusion matrix
conf_mat_54l <- table(Actual = test_data_54l$bys54l, Predicted = test_pred_54l)

# Extract TP and FN from the confusion matrix
#TP_54l <- conf_mat_54l["1", "1"]
#FN_54l <- conf_mat_54l["1", "0"]

# Calculate TPR
#TPR_54l <- #TP_54l / (#TP_54l + #FN_54l)

# Print the TPR
#print(#TPR_54l)

# Plot variable importance
#varImpPlot(rf_54l, n.var=5)

# ROSE RF for 54n ----
# Resampling and modeling for variable "bys54n"
new54n <- baseyear[, c(predictors, "bys54n")]

# Split data into train and test sets
train_index_54n <- createDataPartition(new54n$bys54n, p = 0.7, list = FALSE)
train_data_54n <- new54n[train_index_54n, ]
test_data_54n <- new54n[-train_index_54n, ]

X_train_54n <- train_data_54n[, !names(train_data_54n) %in% "bys54n"]
y_train_54n <- train_data_54n$bys54n
X_test_54n <- test_data_54n[, !names(test_data_54n) %in% "bys54n"]
y_test_54n <- test_data_54n$bys54n

# Perform ROSE on training data
train_rose_54n <- ROSE(bys54n ~ ., data = train_data_54n)
train_data_54n <- train_rose_54n$data

#model_54n <- rpart(bys54n~., data = train_data_54n, method = "class")
#rpart.plot(model_54n, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54n", cex = 0.7)

rf_54n <- randomForest(bys54n ~ ., data = train_data_54n, ntree = 100)

# Predictions
test_pred_54n <- predict(rf_54n, newdata = test_data_54n, type = "class")

# Confusion matrix
conf_mat_54n <- table(Actual = test_data_54n$bys54n, Predicted = test_pred_54n)

# Extract TP and FN from the confusion matrix
#TP_54n <- conf_mat_54n["1", "1"]
#FN_54n <- conf_mat_54n["1", "0"]

# Calculate TPR
#TPR_54n <- #TP_54n / (#TP_54n + #FN_54n)

# Print the TPR
#print(#TPR_54n)

# Plot variable importance
#varImpPlot(rf_54n, n.var=5)

# RF for 54o ----
# Resampling and modeling for variable "bys54o"
new54o <- baseyear[, c(predictors, "bys54o")]

# Split data into train and test sets
train_index_54o <- createDataPartition(new54o$bys54o, p = 0.7, list = FALSE)
train_data_54o <- new54o[train_index_54o, ]
test_data_54o <- new54o[-train_index_54o, ]

X_train_54o <- train_data_54o[, !names(train_data_54o) %in% "bys54o"]
y_train_54o <- train_data_54o$bys54o
X_test_54o <- test_data_54o[, !names(test_data_54o) %in% "bys54o"]
y_test_54o <- test_data_54o$bys54o

#model_54o <- rpart(bys54o~., data = train_data_54o, method = "class")
#rpart.plot(model_54o, extra = 101, fallen.leaves = TRUE, main = "Decision Tree 54o", cex = 0.7)

rf_54o <- randomForest(bys54o ~ ., data = train_data_54o, ntree = 100)

# Predictions
test_pred_54o <- predict(rf_54o, newdata = test_data_54o, type = "class")

# Confusion matrix
conf_mat_54o <- table(Actual = test_data_54o$bys54o, Predicted = test_pred_54o)

# Extract TP and FN from the confusion matrix
#TP_54o <- conf_mat_54o["1", "1"]
#FN_54o <- conf_mat_54o["1", "0"]

# Calculate TPR
#TPR_54o <- #TP_54o / (#TP_54o + #FN_54o)

# Print the TPR
#print(#TPR_54o)

# Plot variable importance
#varImpPlot(rf_54o, n.var = 5)

# Dashboard ----
library(plotly)
complete_rows <- complete.cases(baseyear)

# Subset the data to include only rows without NAs
baseyear_complete <- baseyear[complete_rows, c(vars, predictors, "bys23a", "bys23b", "bys23c", "bys23d", "bys23e", "bys23f", 
                                               "bys24f", "bys24g", "bys35a", "bys36a", 
                                               "bys39", "bys41", "bys42", "bys43", "bys44a", "bys44c", "bys44e", 
                                               "bys44f", "bys44g", "bys45c", "bys46a", "bys46b", "bys48a", "bys48b", 
                                               "bys49a", "bys49b", "bys56", 
                                               "bys83b", "bys85a", "bys85b", "f1doqflg")]

#baseyear_complete[-which(names(baseyear_complete) %in% vars)] <- lapply(baseyear_complete[-which(names(baseyear_complete) %in% vars)], as.character)

# baseyear_complete[,67:102] <- lapply(baseyear_complete[,67:102], as.character)
# baseyear_complete[,67:102] <- lapply(baseyear_complete[,67:102], as.numeric)
# 
# for (col in names(baseyear_complete)) {
#   baseyear_complete[[col]][baseyear_complete[[col]] < 0] <- NA
# }
# 
# baseyear_complete <- na.omit(baseyear_complete)
# 
# baseyear_complete[] <- lapply(baseyear_complete[], as.factor)

# baseyear_complete$bys24f <- ifelse(baseyear_complete$bys24f == 1, 1, 2)
# baseyear_complete$bys24f <- droplevels(as.factor(baseyear_complete$bys24f), exclude = c(3, 4, 5))
# 
# baseyear_complete$bys24g <- ifelse(baseyear_complete$bys24g == 1, 1, 2)
# baseyear_complete$bys24g <- droplevels(as.factor(baseyear_complete$bys24g), exclude = c(3, 4, 5))

rows_with_bys54 <- apply(baseyear_complete[, grepl("^bys54", names(baseyear_complete))], 1, function(row) any(row == 1))
baseyear_bys54 <- baseyear_complete[rows_with_bys54, ]

baseyear_bys54[,67:97] <- lapply(baseyear_bys54[,67:97], as.character)
baseyear_bys54[,67:97] <- lapply(baseyear_bys54[,67:97], as.numeric)

for (col in names(baseyear_bys54)) {
  baseyear_bys54[[col]][baseyear_bys54[[col]] < 0] <- NA
}

baseyear_bys54 <- na.omit(baseyear_bys54)

baseyear_bys54[] <- lapply(baseyear_bys54[], as.factor)

baseyear_bys54$bys24f <- ifelse(baseyear_bys54$bys24f == 1, 1, 2)
baseyear_bys54$bys24f <- droplevels(as.factor(baseyear_bys54$bys24f), exclude = c(3, 4, 5))

baseyear_bys54$bys24g <- ifelse(baseyear_bys54$bys24g == 1, 1, 2)
baseyear_bys54$bys24g <- droplevels(as.factor(baseyear_bys54$bys24g), exclude = c(3, 4, 5))

baseyear_bys54$bys56 <- droplevels(baseyear_bys54$bys56, exclude = c(1, 2))

# Sample 5 rows from the subset
by_random <- baseyear_bys54[sample(nrow(baseyear_bys54), 10), ]
rownames(by_random) <- 1:10
                                                              

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "HS Success Advising Visualization"),
  dashboardSidebar(
    # Move the "Select Student ID" tab to the sidebar
    sidebarMenu(
      id = "tabs",
      menuItem("Select Student ID", tabName = "select_row")
    )
  ),
  dashboardBody(
    # Use sidebarLayout for a sidebar and main content
    sidebarLayout(
      # Sidebar panel for ID selection
      sidebarPanel(
        selectInput("row_select", "Select Row:", choices = 1:10)
      ),
      # Main panel for the content
      mainPanel(
        # Split the main content into four boxes
        fluidRow(
          box(
            title = "Important Life Values",
            height = "600px", # Set a fixed height for the box
            tableOutput("relevant_vars_table"),
            # Add scrollbar if the content overflows
            style = "overflow-y: auto;"
          ),
          box(
            title = "Topics for Life Success Conversations",
            height = "600px", # Set a fixed height for the box
            tableOutput("top_predictors_table"),
            # Add scrollbar if the content overflows
            style = "overflow-y: auto;"
          ),
          box(
            title = "Drop-out Risk",
            height = "600px", # Set a fixed height for the box
            plotlyOutput("dropout_risk_plot"),
            # Add scrollbar if the content overflows
            style = "overflow-y: auto;"
          ),
          box(
            title = "Drop-out Probability Distribution",
            height = "600px", # Set a fixed height for the box
            plotlyOutput("dropout_prob_dist_plot"),
            # Add scrollbar if the content overflows
            style = "overflow-y: auto;"
          )
        )
      )
    )
  )
)




# Define server logic

server <- function(input, output, session) {
  
  vars <- c("bys54a", "bys54b", "bys54c", "bys54d", "bys54e", "bys54f", "bys54g", "bys54h", "bys54i", "bys54j", "bys54k", "bys54l", "bys54n", "bys54o")
  
  # Create a named list with variable names as names and titles as values
  bys54_titles <- list(
    a = "Being successful in line of work",
    b = "Marrying right person/having happy family",
    c = "Having lots of money",
    d = "Having strong friendships",
    e = "Being able to find steady work",
    f = "Helping others in community",
    g = "Giving children better opportunities",
    h = "Living close to parents/relatives",
    i = "Getting away from this area",
    j = "Working to correct inequalities",
    k = "Having children",
    l = "Having leisure time",
    n = "Being expert in field of work",
    o = "Getting good education"
  )
  
  # Create a data frame combining vars and bys54_titles
  vars_and_titles <- data.frame(
    Variable = vars,
    Value = unlist(bys54_titles)
  )
  
  # Function to calculate drop-out probability for selected ID
  calculate_dropout_prob <- function(selected_id) {
    dropout_prob <- predict(logit_full, newdata = by_random[rownames(by_random) == selected_id, c("bys14", "bys20e", "bys20h", "bys20i", "bys20j", "bys20k",
                                                                                                  "bys20m", "bys21b", "bys21c", "bys21d", "bys21e", "bys22a", "bys23a", "bys23b", "bys23c", "bys23d", "bys23e", "bys23f",
                                                                                                  "bys24f", "bys24g", "bys27h", "bys28", "bys37",
                                                                                                  "bys33a", "bys33b", "bys33c", "bys33d", "bys33e", "bys33f", "bys33g", "bys33h", "bys33i", "bys33j", "bys33k", "bys33l", "bys71g", "bys35a", "bys36a", "bys39",
                                                                                                  "bys41", "bys42", "bys43", "bys44a", "bys44c", "bys44e",
                                                                                                  "bys44f", "bys44g", "bys45c", "bys46a", "bys46b", "bys48a", "bys48b",
                                                                                                  "bys49a", "bys49b", "bys56", "bys83b",
                                                                                                  "bys85a", "bys85b","f1doqflg")], type = "response", na.rm=T)
    return(dropout_prob)
  }
  
  # Function to calculate drop-out probability distribution for all IDs
  calculate_dropout_prob_dist <- function() {
    dropout_prob_dist <- predict(logit_full, newdata = by_random, type = "response")
    return(dropout_prob_dist)
  }
  
  # Output for drop-out risk plot
  output$dropout_risk_plot <- renderPlotly({
    selected_id <- input$row_select
    dropout_prob <- calculate_dropout_prob(selected_id)
    
    # Define the data for the color bar
    color_data <- data.frame(value = seq(0, 0.5, length.out = 100))
  
    
    # Create the horizontal graduated color bar plot
    graduated_bar_horizontal <- ggplot(color_data, aes(x = value, y = 1, fill = value, text = paste("Risk (Probability): ", scales::percent(value, accuracy = 0.1)))) +
      geom_tile() +
      geom_point(data = data.frame(value = dropout_prob, y = 0.35), aes(x = value, y = y), shape = 17, size = 5, color = "black", fill = "transparent") +  # Add chevron indicator
      scale_fill_gradient(low = "green", high = "red") +  # Adjust colors as desired
      theme(plot.background = element_rect(fill = "white"),
            axis.line = element_blank(), 
            axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            legend.position = "none") + 
      coord_fixed(ratio = 0.05) +  # Adjust the height of the bar
      annotate("text", x = 0.025, y = 2.5, label = "Low", size = 5, color = "black") +  # Add text above the color scale
      annotate("text", x = 0.25, y = 2.5, label = "Middle", size = 5, color = "black") +
      annotate("text", x = 0.48, y = 2.5, label = "High", size = 5, color = "black")
    
    # Convert ggplot to plotly for interactivity
    graduated_bar_horizontal_plotly <- ggplotly(graduated_bar_horizontal, tooltip = "text") %>%
      layout(plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE)
    
    # Modify the tooltip text for the predicted value
    graduated_bar_horizontal_plotly$x$data[[1]]$text[paste("Risk (Probability): ", dropout_prob)] <- paste("Predicted Value: ", scales::percent(dropout_prob, accuracy = 0.1))
    
    
    # Return the plotly object
    graduated_bar_horizontal_plotly
  })
  
  
  # Output for drop-out probability distribution plot
  output$dropout_prob_dist_plot <- renderPlotly({
    dropout_prob_dist <- calculate_dropout_prob_dist()
    selected_id <- input$row_select
    #dropout_prob <- calculate_dropout_prob(selected_id)
    
    # Create a data frame for plotting
    plot_data <- data.frame(ID = rep("All", length(dropout_prob_dist)),
                            dropout_prob = scales::percent(dropout_prob_dist, accuracy = 0.1))
    plot_data$ID[by_random$ID == selected_id] <- "Selected"
    
    # Extract the Value where ID == "Selected"
    selected_value <- calculate_dropout_prob(selected_id)
    selected_value <- scales::percent(selected_value, accuracy = 0.1)
    
    print(selected_value)
    
    p <- ggplot(plot_data, aes(y = dropout_prob)) +
      geom_boxplot() +
      geom_point(data = data.frame(Value = selected_value), aes(x = 0, y = Value, 
                                                                     text = paste("Student's risk: ", Value)), 
                 color = "red", size = 3) +
      theme_minimal() + 
      labs(title = "Distribution of Drop-out Risk among the current Student Body")
    
    # Convert the ggplot to plotly
    ggplotly(p, tooltip = c("text")) 
  })
  
  # Random Forest
  observeEvent(input$row_select, {
    selected_row <- input$row_select
    selected_data <- by_random[selected_row, c(vars,predictors)]
    
    relevant_vars <- vars[selected_data[1, grepl("^bys54", names(selected_data))] == 1]
    
    relevant_vars_table <- data.frame(
      Value = vars_and_titles$Value[match(relevant_vars, vars_and_titles$Variable)]
    )
    
    output$relevant_vars_table <- renderTable({
      relevant_vars_table
    }, rownames = FALSE)
    
    all_top_predictor_titles <- c()
    
    for (var in relevant_vars) {
      model_name <- substring(var, 4)
      rf_model_name <- paste0("rf_", model_name)
      
      if (exists(rf_model_name)) {
        rf_model <- get(rf_model_name)
        top_predictors <- head(sort(rf_model$importance[,1], decreasing = TRUE), 3)
        top_predictor_names <- names(top_predictors)
        top_predictor_titles <- predictors_df$Value[match(top_predictor_names, predictors_df$Variable)]
        
        all_top_predictor_titles <- c(all_top_predictor_titles, top_predictor_titles)
      }
    }
    
    unique_top_predictor_titles <- unique(all_top_predictor_titles)
    
    top_predictors_table <- data.frame(Topic = unique_top_predictor_titles)
    
    output$top_predictors_table <- renderTable({
      top_predictors_table
    }, rownames = FALSE)
  })
}




# Run the application
shinyApp(ui, server)

