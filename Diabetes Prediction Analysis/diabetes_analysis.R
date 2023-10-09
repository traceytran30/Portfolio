knitr::opts_chunk$set(echo = TRUE)

df = read.csv("diabetes.csv", header=TRUE, stringsAsFactors=FALSE)
n = nrow(df)

base_model = lm(df$Outcome ~ df$Age)
base_e = df$Outcome - (base_model$coefficients[1] + base_model$coefficients[2] * df$Age)
base_sse = sum(base_e^2)

mean(df$Age)
sd(df$Age)
cor(df$Age, df$Outcome)
range(df$Age)[2] - range(df$Age)[1]

mean(df$Pregnancies)
sd(df$Pregnancies)
cor(df$Pregnancies, df$Outcome)
range(df$Pregnancies)[2] - range(df$Pregnancies)[1]

mean(df$Glucose)
sd(df$Glucose)
cor(df$Glucose, df$Outcome)
range(df$Glucose)[2] - range(df$Glucose)[1]

mean(df$BloodPressure)
sd(df$BloodPressure)
cor(df$BloodPressure, df$Outcome)
range(df$BloodPressure)[2] - range(df$BloodPressure)[1]

mean(df$SkinThickness)
sd(df$SkinThickness)
cor(df$SkinThickness, df$Outcome)
range(df$SkinThickness)[2] - range(df$SkinThickness)[1]

mean(df$Insulin)
sd(df$Insulin)
cor(df$Insulin, df$Outcome)
range(df$Insulin)[2] - range(df$Insulin)[1]

mean(df$BMI)
sd(df$BMI)
cor(df$BMI, df$Outcome)
range(df$BMI)[2] - range(df$BMI)[1]

mean(df$DiabetesPedigreeFunction)
sd(df$DiabetesPedigreeFunction)
cor(df$DiabetesPedigreeFunction, df$Outcome)
range(df$DiabetesPedigreeFunction)[2] - range(df$DiabetesPedigreeFunction)[1]

pregnancies_model = lm(df$Outcome ~ df$Age + df$Pregnancies)
pregnancies_e = df$Outcome - (pregnancies_model$coefficients[1] + pregnancies_model$coefficients[2] * df$Age + pregnancies_model$coefficients[3] * df$Pregnancies)
pregnancies_sse = sum(pregnancies_e^2)
pregnancies_partial_r = (base_sse - pregnancies_sse) / base_sse

glucose_model = lm(df$Outcome ~ df$Age + df$Glucose)
glucose_e = df$Outcome - (glucose_model$coefficients[1] + glucose_model$coefficients[2] * df$Age + glucose_model$coefficients[3] * df$Glucose)
glucose_sse = sum(glucose_e^2)
glucose_partial_r = (base_sse - glucose_sse) / base_sse

bloodpressure_model = lm(df$Outcome ~ df$Age + df$BloodPressure)
bloodpressure_e = df$Outcome - (bloodpressure_model$coefficients[1] + bloodpressure_model$coefficients[2] * df$Age + bloodpressure_model$coefficients[3] * df$BloodPressure)
bloodpressure_sse = sum(bloodpressure_e^2)
bloodpressure_partial_r = (base_sse - bloodpressure_sse) / base_sse

skinthickness_model = lm(df$Outcome ~ df$Age + df$SkinThickness)
skinthickness_e = df$Outcome - (skinthickness_model$coefficients[1] + skinthickness_model$coefficients[2] * df$Age + skinthickness_model$coefficients[3] * df$SkinThickness)
skinthickness_sse = sum(skinthickness_e^2)
skinthickness_partial_r = (base_sse - skinthickness_sse) / base_sse

insulin_model = lm(df$Outcome ~ df$Age + df$Insulin)
insulin_e = df$Outcome - (insulin_model$coefficients[1] + insulin_model$coefficients[2] * df$Age + insulin_model$coefficients[3] * df$Insulin)
insulin_sse = sum(insulin_e^2)
insulin_partial_r = (base_sse - insulin_sse) / base_sse

bmi_model = lm(df$Outcome ~ df$Age + df$BMI)
bmi_e = df$Outcome - (bmi_model$coefficients[1] + bmi_model$coefficients[2] * df$Age + bmi_model$coefficients[3] * df$BMI)
bmi_sse = sum(bmi_e^2)
bmi_partial_r = (base_sse - bmi_sse) / base_sse

diabetespedigree_model = lm(df$Outcome ~ df$Age + df$DiabetesPedigreeFunction)
diabetespedigree_e = df$Outcome - (diabetespedigree_model$coefficients[1] + diabetespedigree_model$coefficients[2] * df$Age + diabetespedigree_model$coefficients[3] * df$DiabetesPedigreeFunction)
diabetespedigree_sse = sum(diabetespedigree_e^2)
diabetespedigree_partial_r = (base_sse - diabetespedigree_sse) / base_sse

F = ((base_sse - glucose_sse) / (glucose_sse / (n-2)))

plot(df$Age)
plot(df$Glucose)
plot(df$Age, df$Glucose)
plot(df$Age, df$Outcome)
plot(df$Glucose, df$Outcome)

library(ggplot2)
ggplot(df, aes(x = df$Age)) + geom_histogram(binwidth = 2, color = "black", fill = "blue") + facet_grid(df$Outcome ~.) + ggtitle("Presence of Diabetes by Age")
ggplot(df, aes(x = df$Glucose)) + geom_histogram(binwidth = 2, color = "black", fill = "blue") + facet_grid(df$Outcome ~.) + ggtitle("Presence of Diabetes by Glucose Levels")

