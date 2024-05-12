
install.packages("ISLR2")       # An Introduction to Statistical Learning 2 
install.packages("tidyverse")   # for data science
install.packages("caret")       # Classification And REgression Training
install.packages("modelr")      # for modeling
install.packages("corrplot") 

#_____

library(ISLR2)
library(tidyverse)
library(caret)
library(modelr)
library(corrplot)

lda <- MASS::lda
qda <- MASS::qda

#_____

plot(Weekly)

#_____

Weekly %>%                              # Assignment operator
  select_if(is.numeric) %>%
  cor() %>% 
  corrplot::corrplot()

#_____

log_reg_weekly <- 
  glm(Direction ~ . - Today, data = Weekly, family = "binomial")

summary(log_reg_weekly)

#_____

actual_direction <- Weekly[["Direction"]]
predicted_direction <- 
  ifelse(predict(log_reg_weekly, type = "response")  > 0.5, 
         "Up", "Down") %>% 
  factor(levels = c("Down", "Up"))

caret::confusionMatrix(data = predicted_direction, 
                       reference = actual_direction)

#_____

train_weekly <- Weekly %>% 
  filter(between(Year, 1990, 2008))

test_weekly <- Weekly %>% 
  filter(between(Year, 2009, 2010))

reg_upto2008 <- 
  glm(Direction ~ Lag2, data = train_weekly, family = "binomial")

add_pred_direction <- function(df, model) {
  df %>% 
  add_predictions(model, type = "response") %>% 
  mutate(pred_direction = ifelse(
    pred > 0.5,
    "Up",
    "Down"
  ),
  pred_direction = factor(pred_direction, levels = c("Down", "Up")))
}

test_weekly_reg <- 
  test_weekly %>% 
  add_pred_direction(reg_upto2008)

caret::confusionMatrix(data = test_weekly_reg[["pred_direction"]],
                       reference = test_weekly_reg[["Direction"]])

#_____

lda_upto2008 <- 
  MASS::lda(Direction ~ Lag2, data = train_weekly)

test_weekly_lda <- 
  test_weekly %>% 
  mutate(pred_direction = 
           predict(lda_upto2008, 
                   newdata = test_weekly, 
                   type = "response")[["class"]])

caret::confusionMatrix(data = test_weekly_lda[["pred_direction"]],
                       reference = test_weekly_lda[["Direction"]])

#_____

qda_upto2008 <- 
  MASS::qda(Direction ~ Lag2, data = train_weekly)

test_weekly_qda <- 
  test_weekly %>% 
  mutate(pred_direction = 
           predict(qda_upto2008, 
                   newdata = test_weekly, 
                   type = "response")[["class"]])

caret::confusionMatrix(data = test_weekly_qda[["pred_direction"]],
                       reference = test_weekly_qda[["Direction"]])

#_____

train_x_weekly <- 
  train_weekly %>%
  select(Lag2)

train_y_weekly <- 
  train_weekly[["Direction"]]

test_x_weekly <- 
  test_weekly %>% 
  select(Lag2)

knn_upto2008 <- class::knn(
  train = train_x_weekly,
  test = test_x_weekly,
  cl = train_y_weekly,
  k = 1
)

caret::confusionMatrix(
  data = knn_upto2008,
  reference = test_weekly[["Direction"]]
)

#_____

library (e1071)

nb_upto2008 <- 
  naiveBayes(Direction ~ Lag2, data = train_weekly)

test_weekly_nb <- 
  test_weekly %>% 
  mutate(pred_direction = 
           predict(nb_upto2008, newdata = test_weekly, type = "class"))

caret::confusionMatrix(data = test_weekly_nb[["pred_direction"]],
                       reference = test_weekly_nb[["Direction"]])

#_____

reg2_upto2008 <- 
  glm(Direction ~ Lag1 + Lag2 + Volume, 
      data = train_weekly, family = "binomial")

reg3_upto2008 <- 
  glm(Direction ~ Lag1 + Lag2, 
      data = train_weekly, family = "binomial")

reg4_upto2008 <- 
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Volume, 
      data = train_weekly, family = "binomial")

reg5_upto2008 <- 
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Volume, 
      data = train_weekly, family = "binomial")

reg6_upto2008 <- 
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Volume + I(Volume)^2, 
      data = train_weekly, family = "binomial")

reg7_upto2008 <- 
  glm(Direction ~ Lag1 + Lag2*Volume + Lag3, 
      data = train_weekly, family = "binomial")

test_weekly_reg <- 
  test_weekly_reg %>%
  add_predictions(reg2_upto2008, var = "pred_reg2", type = "response") %>%
  add_predictions(reg3_upto2008, var = "pred_reg3", type = "response") %>%
  add_predictions(reg4_upto2008, var = "pred_reg4", type = "response") %>%
  add_predictions(reg4_upto2008, var = "pred_reg5", type = "response") %>%
  add_predictions(reg4_upto2008, var = "pred_reg6", type = "response") %>%
  add_predictions(reg4_upto2008, var = "pred_reg7", type = "response") %>%
  mutate_at(vars(starts_with("pred_reg")),
            ~ factor(ifelse(. > 0.5,
                            "Up",
                            "Down"), levels = c("Down", "Up")))


caret::confusionMatrix(data = test_weekly_reg[["pred_reg2"]],
                       reference = test_weekly_reg[["Direction"]])

caret::confusionMatrix(data = test_weekly_reg[["pred_reg3"]],
                       reference = test_weekly_reg[["Direction"]])

caret::confusionMatrix(data = test_weekly_reg[["pred_reg4"]],
                       reference = test_weekly_reg[["Direction"]])

caret::confusionMatrix(data = test_weekly_reg[["pred_reg5"]],
                       reference = test_weekly_reg[["Direction"]])

caret::confusionMatrix(data = test_weekly_reg[["pred_reg6"]],
                       reference = test_weekly_reg[["Direction"]])
                       
caret::confusionMatrix(data = test_weekly_reg[["pred_reg7"]],
                       reference = test_weekly_reg[["Direction"]])
 