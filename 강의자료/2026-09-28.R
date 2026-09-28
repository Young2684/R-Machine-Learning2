train_data <- read.csv("C:/Users/USER/Desktop/실습/01.simple_train_data.csv")
test_data <- read.csv("C:/Users/USER/Desktop/실습/01.simple_test_data.csv")

model <- lm(upper_arm ~ thigh, data = train_data)
summary(model)

saveRDS(model, "regression_model.rds")
loaded_model <- readRDS("regression_model.rds")

predicted <- predict(loaded_model, newdata = test_data)
head(predicted)

results <- data.frame(Actual = test_data$upper_arm, Predicted = predicted)

rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", round(rmse, 3)))

model <- lm(upper_arm ~ 0 + thigh, data = train_data)
summary(model)

install.packages("rstatix")
install.packages("skimr")

library(rstatix)
library(skimr)

df <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes.csv", header = TRUE, stringsAsFactors = TRUE)
skim(df)

df <- na.omit(df)

df <- select(df, -c(Outcome)) #연속형이 아닌 변수 Outcome 제거거

skim(df)

#다중회귀분석 시작
#아노바 분석시 차이가 있어야함!
result <- lm(df$Diabetes ~ ., data = df)
result1 <- lm(df$Diabetes ~ 1, data = df)

anova_result <- anova(result1, result)
print(anova_result)

summary(result)

#회귀분석하고 p-value가 0.05보다 큰 것들은 전부 제거한 다음 회귀분석 돌리기기
df <- select(df, -c(Pregnancies, BloodPressure, BMI, Age))

#다시 회귀분석을 돌리고 R-Squared 값이 0.05601이므로 좋은 값은 아님
saveRDS(result, "regression_model.rds") # 이름자리에 회귀분석한 값이 들어가야함
loaded_model <- readRDS("regression_model.rds")

new_data <- read.csv("C:/Users/USER/Desktop/02.다중회귀분석_실습/02.diabetes_test.csv")

predicted <- predict(loaded_model, newdata = new_data)
head(predicted)

results <- data.frame(new_data, predicted = predicted)

rmse <- sqrt(mean((results$Diabetes - results$predicted)^2))
print(paste("RMSE:", round(rmse, 3)))
#=> 설명력이 낮다!! (0.291)

library(lmtest)
bptest(result)

library(car)
durbinWatsonTest(result)

shapiro_test(result$residuals)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(result)
par(opar)

cand <- influencePlot(result, id=list(n=3))
mf <- model.frame(result)
z <- round(scale(mf[sapply(mf, is.numeric)]),1)
rownames(z) <- rownames(mf)
cbind(round(cand,2), z[rownames(cand),])

#CookD값이 가장 높은 2개를 제거해봅시다
out_rows <- c("5", "14", "229", "371", "446", "580")
df2 <- df[!rownames(df) %in% out_rows, ]
result2 <- lm(Diabetes ~ ., data = df2)
cand <- influencePlot(result2, id=list(n=3))

max(cooks.distance(result2))
