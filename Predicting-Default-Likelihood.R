# Part 1: Executive Summary for Regression Question

# Part 2: Data and Variable Description for Regression

library(ggplot2)
set.seed(111)
data <- as.data.frame(read.csv("/Users/chenzian/Desktop/UVA/STAT 4630/Project/bankloans.csv"))

# a) Data Cleaning and Processing

df <- na.omit(data)
df <- df[,-c(4,6)]
df$ed <- factor(df$ed)
df$default <- factor(df$default)
levels(df$ed) <- c("high school", "undergraduate", "master", "ph.d", "higher education")
contrasts(df$ed) <- contr.sum(5)  

sample.data <- sample.int(nrow(df), floor(.50 * nrow(df)), replace = F)
train <- df[sample.data, ]
test <- df[-sample.data, ]

# b) Relevant Summaries

summary_statistics <- train[,1:6]
summary(summary_statistics)

# Correlation Matrix

df2 <- data[,-c(4,6,9)]
round(cor(df2[,1:6]), 3)

moments::skewness(train$income)
moments::skewness(train$creddebt)
moments::skewness(train$othdebt)

log_income <- log(train$income)
hist(log_income, xlab = "Log(income) (in thousands $)", main = "Distribution of Yearly Incomes", breaks = 12, col = 'yellow')
mean_log_income <- mean(log_income, na.rm = TRUE)
abline(v = mean_log_income, col = 'limegreen', lwd = 4)
text(x = mean_log_income + 0.7, y = 35, labels = paste("Mean =", round(mean_log_income, 2)), col = 'limegreen')

par(mar = c(5, 5, 3, 1), cex=.4)
boxplot(log_income ~ train$ed,
        main = "Boxplot of Income by Education Level",
        xlab = "Education Level",
        ylab = "Log(income)",
        col = c("lightgray", "lightblue", "yellow2","#C1FFC1","thistle1"),
        border = "black",
        notch = FALSE)

boxplot(log_income ~ age, data = train,
        xlab = "Age",
        ylab = "Log(income)",
        col = "lightyellow",
        border = "black")

Log_creddebt <- log(train$creddebt)
ggplot(train, aes(x = log_income, y = Log_creddebt , color = log_income)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  scale_color_gradient(low = "yellow", high = "red") + 
  labs(x = "Log(income)", y = "Log(creddebt)", title = "Income vs. Credit to Debt Ratio")+
  theme(plot.title = element_text(hjust = 0.5))

Log_othdebt <- log(train$othdebt)

ggplot(train, aes(x = log_income, y = Log_othdebt, color = log_income)) +
  geom_point() +
  scale_color_gradient(low = "yellow", high = "brown") +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  labs(x = "Log(income)", y = "Log(othdebt)", title = "Income vs. Other Debt") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(train, aes(x = log_income, fill = default)) +
  geom_bar(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#F0E442", "lightgreen")) +  
  labs(x = "Log(income)", y = "Count", title = "Bar Chart: Income vs. Default")

ed_colors <- c("lightblue", "green", "orange", "yellow", "red")

ggplot(train, aes(x = employ, y = log_income, color = ed)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(method = lm, data = train, se=F, color="purple") +
  scale_color_manual(values = ed_colors) +  
  labs(x = "Work Experience", y = "Log Income")  

pairs(df2, lower.panel = NULL)

# Part 3: Regression Question

# 3.1 Exploratory Data Analysis for Regression Question

# 3.2 Shrinkage Methods

loan_data <- read.csv("/Users/chenzian/Desktop/UVA/STAT 4630/Project/bankloans.csv")
loan_data$ed <- as.factor(loan_data$ed)
loan_data$logincome <- log(loan_data$income)
loan_data$logcreddebt <- log(loan_data$creddebt)
loan_data$logothdebt <- log(loan_data$othdebt)

library(glmnet)
x <- model.matrix(logincome ~ employ + age + logcreddebt + logothdebt + ed, data = loan_data)[, -1]
y <- loan_data$logincome
ridge.r <- glmnet::glmnet(x, y, alpha = 0, lambda = 0)
result <- lm(logincome ~ employ + age + creddebt + othdebt + ed, data = loan_data)
cbind(coefficients(result), coefficients(ridge.r))

ridge.r <- glmnet::glmnet(x, y, alpha = 0, lambda = 0, thresh = 1e-23)
cbind(coefficients(result), coefficients(ridge.r))

ridge.r <- glmnet::glmnet(x, y, alpha = 0, lambda = 0, thresh = 1e-100)
cbind(coefficients(result), coefficients(ridge.r))

# 3.3 Regression Trees

library(dplyr)
library(tree)
library(glmnet)
loan_data <- read.csv("/Users/chenzian/Desktop/UVA/STAT 4630/Project/bankloans.csv")
loan_data$ed <- as.factor(loan_data$ed)
loans <- loan_data %>%
  select('income','age', 'ed', 'employ','creddebt','othdebt')
loans <- na.omit(loans)
loans$logincome <- log(loans$income)
loans$logcreddebt <- log(loans$creddebt)
loans$logothdebt <- log(loans$othdebt)

set.seed(4630)
sample.data <- sample.int(nrow(loans), floor(.50 * nrow(loans)), replace = F)
train <- loans[sample.data, ]
test <- loans[-sample.data, ]
loans.test <- test[,"logincome"]
tree.result <- tree(logincome ~ age + ed + employ + logcreddebt + logothdebt, data = train)
cv.loans <- cv.tree(tree.result, K = 10)
trees.num <- cv.loans$size[which.min(cv.loans$dev)]
tree.full <- tree::tree(logincome ~ age + ed + employ + logcreddebt + logothdebt, data = loans)
prune.full <- tree::prune.tree(tree.full, best = trees.num)
summary(prune.full)

plot(prune.full)
text(prune.full, cex = 0.45)
yhat.prune <- predict(prune.full, newdata = test)
pruned_mse <- mean((loans.test - yhat.prune)^2)

library(randomForest)
set.seed(4630)
rf.loans <- randomForest(logincome ~ age + ed + employ + logcreddebt + logothdebt, data = train, mtry = 4, importance = TRUE)
round(importance(rf.loans), 2)
varImpPlot(rf.loans)
yhat.rf <- predict(rf.loans, newdata = test)
mse.rf <- mean((loans.test - yhat.rf)^2)

# 3.4 Summary of Findings

library(glmnet)
lr <- lm(logincome ~ age + ed + employ + logcreddebt + logothdebt, data = train)
predictions <- predict(lr, data = test)
residuals <- loans.test - predictions
mse_linear <- mean(residuals^2)
comp <- data.frame(Model = c("Linear Regression", "Ridge Regression", "Lasso Regression", "Pruned Regression Tree", "Random Forests"), Test_MSE = c(mse_linear, 0.1072249, 0.1066406, pruned_mse, mse.rf))
comp

# Part 4: Executive Summary for Classification Question

# Part 5: Data and Variable Description for Classification

# Part 6: Classification Question

# 6.1 Exploratory Data Analysis for Classification

df3 <- na.omit(data)
df3 <- df3[,-c(4,6)]
df3$default <- factor(df3$default)
df3$ed <- as.factor(df3$ed)
levels(df3$default) <- c("no", "yes")

contrasts(df3$default)
prop.table(table(df3$default))

sample.data <- sample.int(nrow(df3), floor(.50 * nrow(df3)), replace = F)
train <- df3[sample.data, ]
test <- df3[-sample.data, ]

log.income <- log(train$income)
log.othdebt <- log(train$othdebt)
log.creddebt <- log(train$creddebt)

plot = ggplot(train, aes(x = log.creddebt, y = log.income, color = default)) +
  geom_point(shape = 19) +
  scale_color_manual("default", values = c("no" = "yellow", "yes" = "limegreen"))
plot + labs(title = "Plot of Credit to Debt Ratio and Income")

boxplot(log.creddebt ~ default, data = train,
        main = "Boxplot of Credit to Debt Ratio and Default",
        xlab = "Default",
        ylab = "Log(creddebt)",
        col = c("yellow2", "lightblue", border = "black", 
                notch = FALSE))

boxplot(log.income ~ default, data = train,
        main = "Boxplot of Income and Default",
        xlab = "Default",
        ylab = "Log(income)",
        col = c("#9999CC", "gold", border = "black", 
                notch = FALSE))

boxplot(employ ~ default, data = train,
        main = "Boxplot of employ and Default",
        xlab = "Default",
        ylab = "Employ",
        col = c("lightyellow", "thistle1", border = "black", 
                notch = FALSE))

boxplot(log.othdebt ~ default, data = train,
        main = "Boxplot of othdebt and Default",
        xlab = "Default",
        ylab = "Log(othdebt)",
        col = c("yellow", "#6B8E23", border = "black", 
                notch = FALSE))

boxplot(age ~ default, data = train,
        main = "Boxplot of Age and Default",
        xlab = "Default",
        ylab = "Age",
        col = c("darkgreen", "orange", border = "black", 
                notch = FALSE))

# 6.2 Logistic Regression Model

logistic_regression <- glm(default ~ age + employ + income + creddebt + othdebt + ed, family = binomial, data = train)
summary(logistic_regression)

# 6.3 Classification Trees

library(readr)
library(tree)
library(randomForest)
Data <- read.csv("/Users/chenzian/Desktop/UVA/STAT 4630/Project/bankloans.csv", header = T)
Data <- na.omit(Data[,-c(2,4,6)])
Data$default <- factor(Data$default)
set.seed(4630)
sample.data <- sample.int(nrow(Data), floor(.50 * nrow(Data)), replace = F)
train <- Data[sample.data, ]
test <- Data[-sample.data, ]
y.test <- test$default
tree.class.train <- tree(default ~ ., data = train)
summary(tree.class.train)

plot(tree.class.train)
text(tree.class.train, cex=0.4, pretty=0)

set.seed(4630)
rf.class <- randomForest(default ~ ., data = train, mtry = 2, importance = TRUE)
varImpPlot(rf.class)

# 6.4 Summary of Findings

logistic_regression <- glm(default ~ age + employ + income + creddebt + othdebt, family = binomial, data = train)
logistic_preds <- predict(logistic_regression, newdata = test, type = "response")
compare.test <- data.frame(actual = test$default, predicted_probability = logistic_preds, predicted = ifelse(logistic_preds > 0.5, 1, 0))
pred.test.error <- mean(compare.test$predicted != test$default) 
log.pred.test <- predict(tree.class.train, newdata = test, type = "class")
table(y.test, log.pred.test)
fpr_log <- 27 / (27 + 244)
fnr_log <- 45 / (45 + 34)

probabilities <- predict(logistic_regression, type = "response")
threshold <- 0.5
predicted_classes <- ifelse(probabilities > threshold, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = y.test)
confusion_matrix

set.seed(4630)
tree.pred.test <- predict(tree.class.train, newdata = test, type = "class")
table(y.test, tree.pred.test)
error_rate <- (29 + 46) / (242 + 46 + 29 + 33)
fpr <- (29) / (29 + 242)
fnr <- (46) / (46 + 33)

set.seed(4630)
pred.rf <- predict(rf.class, newdata = test)
table(y.test, pred.rf)
error_rf <- (28 + 45) / (243 + 28 + 45 + 34)
fpr_rf <- 28 / (243 + 24)
fnr_rf <- 45 / (45 + 34)

metrics <- data.frame(
  Test_Error = c(error_rate, pred.test.error, error_rf),
  FPR = c(fpr, fpr_log, fpr_rf),
  FNR = c(fnr, fnr_log, fnr_rf)
)
colnames(metrics) <- c("Error Rate", "FPR", "FNR")
rownames(metrics) <- c("0.5 Threshold RBS", "0.5 Threshold Logistic Regression", "0.5 Threshold RF")
metrics

error_rf <- (17 + 71) / (200 + 71 + 17 + 62)
fpr_rf <- (71) / (71 + 200)
fnr_rf <- (17) / (17 + 62)
error_rate <- (24 + 70) / (195 + 76 + 23 + 56)
fpr <- (76) / (76 + 195)
fnr <- (23) / (23 + 56)
metrics <- data.frame(
  Test_Error = c(error_rate, 0.2184897, error_rf),
  FPR = c(fpr, 0.2436279, fpr_rf),
  FNR = c(fnr, 0.2875843, fnr_rf)
)
colnames(metrics) <- c("Error Rate", "FPR", "FNR")
rownames(metrics) <- c("0.4 Threshold RBS", "0.4 Threshold Logistic Regression", "0.3 Threshold RF")
metrics
