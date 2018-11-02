> library(rpart)
> library(rpart.plot)
> library(randomForest)
> library(DescTools)
> crime_data = read.table("C:\\Users\\SSamtani\\Downloads\\uscrime.txt", header = TRUE, check.names = FALSE)
> model = rpart(Crime ~ ., data = crime_data, method = "anova")
> rpart.plot(model)
> model2 = randomForest(Crime ~ ., data = crime_data)
> model2$predicted
> newdata = data.frame(M = 14, So = 0, Ed = 10, Po1 = 12.0, Po2 = 15.5, LF = .64, M.F = 94, 
+ Pop = 150, NW = 1.1, U1 = .12, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = .04, Time = 39)
> "predict"(model2, newdata)
> credit_data = read.table("C:\\Users\\SSamtani\\Downloads\\germancredit.txt", header = FALSE, check.names = FALSE)
> credit_data$V21 = factor(credit_data$V21)
> model3 = glm(V21 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20,
+ data = credit_data, family = binomial(link = "logit"))
> PseudoR2(model3)
> sum(credit_data[, 21] == 1)
> sum(credit_data[, 21] == 2)
> predictions = predict(model3, data = credit_data, type = "response")
> min_cost = 1000000
> threshold = .5
> threshold_inc = 0
> optimal_threshold = function(predictions) {
+ for (i in c(0, .05, .1, .15, .2, .25, .3, .35, .4, .45)) {
+ false_values = as.numeric(credit_data[,21]) - 1 - round(predictions + i)
+ false_positive = sum(false_values == 1)
+ false_negative = sum(false_values == -1)
+ if (false_positive*5 + false_negative < min_cost) {
+ min_cost = false_positive*5 + false_negative
+ threshold_inc = i}}
+ return(threshold - threshold_inc)}
> optimal_threshold(predictions)