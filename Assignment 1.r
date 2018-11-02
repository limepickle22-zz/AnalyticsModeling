##############Problem 2.2 Part 1###################
> library(kernlab)
> data = read.table("C:\\Users\\SSamtani\\Downloads\\credit_card_data.txt")
> full_svm = function(lambda, data) {
+ model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type = "C-svc",
+ kernel = "vanilladot", C = lambda, scaled = TRUE)
+ a = colSums(model@xmatrix[[1]] * model@coef[[1]])
+ a0 = model@b
+ pred = predict(model, as.matrix(data[,1:10]))
+ result = sum(pred == as.factor(data[,11]))/nrow(as.matrix(data))
+ return(result)}
> model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type = "C-svc",
> kernel = "vanilladot", C = 294, scaled = TRUE)
> a = colSums(model@xmatrix[[1]] * model@coef[[1]])
> a
> a0 = model@b
> a0
> pred = predict(model, as.matrix(data[,1:10]))
> pred
> result = sum(pred == as.factor(data[,11]))/nrow(as.matrix(data))
> result
############Problem 2.2 Part 1 Continued (Reducing Model to Two Coefficients#######
> model2 = ksvm(as.matrix(data[,c(5,10)]), as.factor(data[,11]), type = "C-svc",
+ kernel = "vanilladot", C = 294, scaled = TRUE)
> a2 = colSums(model2@xmatrix[[1]] * model2@coef[[1]])
> a2
> a02 = model2@b
> a02
> pred2 = predict(model2, as.matrix(data[,c(5, 10)]))
> result2 = sum(pred2 == as.factor(data[,11]))/nrow(as.matrix(data))
> result2
###########Problem 2.2 Part 2#######################
> model3 = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]), type = "C-svc",
> kernel = "rbfdot", C = 294, scaled = TRUE)
> a3 = colSums(model3@xmatrix[[1]] * model3@coef[[1]])
> a3
> a03 = model3@b
> a03
> pred3 = predict(model3, as.matrix(data[,1:10]))
> pred3
> result3 = sum(pred3 == as.factor(data[,11]))/nrow(as.matrix(data))
> result3
#################Problem 2.2 Part 3######################
> library(kknn)
> data = read.table("C:\\Users\\SSamtani\\Downloads\\credit_card_data.txt")
> knn_model = kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data[-2,], data[2,], k = 1, scale = TRUE)
> knn_model$fitted.values
> optimal_k = function(i) {
+ actual = data[i, 11] 
+ best_k = 10
+ for (j in 1:652) {
+ knn_model = kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data[-i,], data[i,], k = j, scale = TRUE)
+ if (abs(knn_model$fitted.values - actual) < .1) {
+ best_k = j}}
+ return (best_k)}
> optimal_k_list = sapply(c(1:50), optimal_k)
> median(optimal_k_list)
> prediction_kknn = function(i) {
+ knn_model = kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data[-i,], data[i,], k = 12, scale = TRUE)
+ return(round(knn_model$fitted.values, digits = 0))}
> prediction_vector = sapply(c(1:654), prediction_kknn)
> sum(prediction_vector == data[, 11])/nrow(data)

