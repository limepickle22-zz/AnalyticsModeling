################## Question 3.1 Part A SVM ################################
> library(e1071)
> data = read.table("C:\\Users\\SSamtani\\Downloads\\credit_card_data.txt")
> set.seed(101)
> sample = sample.int(n = nrow(data), size = floor(.8*nrow(data)), replace = F)
> trainval = data[sample, ]
> test = data[-sample, ]
> param = tune.svm(as.matrix(trainval[,1:10]), as.factor(trainval[,11]), 
+ cost = c(.001, .01, .1, 1, 10, 100, 1000, 10000), kernel = "linear")
> param

	Parameter tuning of ‘svm’:

	- sampling method: 10-fold cross validation 

	- best parameters:
 	cost
 	0.01

	- best performance: 0.1395864 

> model = param$best.model
> a = colSums(model@xmatrix[[1]] * model@coef[[1]])
> a
> a0 = model@b
> a0
> pred = predict(model, as.matrix(test[,1:10]))
> pred
> result = sum(pred == as.factor(test[,11]))/nrow(as.matrix(test))
> result

################## Question 3.1 Part A KNN ##################################
> library(caret)
> knncv = train(factor(V11, levels = c(0, 1), labels = c("No", "Yes"))~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, method = "knn", tuneGrid = 
+ expand.grid(k = 1:20), trControl = trainControl(method = "cv", number = 10),
+ metric = "Accuracy", data = trainval, preProcess = c("scale"))
> knncv
	k-Nearest Neighbors 

	523 samples
 	10 predictor
  	2 classes: 'No', 'Yes' 

	Pre-processing: scaled (10) 
	Resampling: Cross-Validated (10 fold) 
	Summary of sample sizes: 471, 472, 471, 470, 471, 470, ... 
	Resampling results across tuning parameters:

  	k   Accuracy   Kappa    
   	1  0.8030379  0.6002475
   	2  0.8012600  0.5986764
   	3  0.8431714  0.6838964
   	4  0.8318492  0.6604941
   	5  0.8508623  0.7015950
   	6  0.8585560  0.7169666
   	7  0.8604065  0.7209910
   	8  0.8507883  0.7017520
   	9  0.8547462  0.7091998
  	10  0.8470524  0.6926381
  	11  0.8412832  0.6827560
  	12  0.8356591  0.6706968
  	13  0.8374370  0.6740453
  	14  0.8470901  0.6927698
  	15  0.8431323  0.6850570
  	16  0.8452033  0.6885054
  	17  0.8338100  0.6659232
  	18  0.8376562  0.6739983
  	19  0.8338840  0.6647782
  	20  0.8242309  0.6452003

	Accuracy was used to select the optimal model using the largest value.
	The final value used for the model was k = 7.

> prediction_vector = predict(knncv, newdata = test)
> sum(prediction_vector == factor(test[,11], levels = c(0, 1), labels = c("No", "Yes")))/nrow(test)

####################### Question 3.1 Part B KNN ############################################
> library(kknn)
> set.seed(101)
> sample_train = sample.int(n = nrow(data), size = floor(.6*nrow(data)), replace = F)
> train_knn = data[sample_train, ]
> leftover = data[-sample_train, ]
> sample_valid = sample.int(n = nrow(leftover), size = floor(.5*nrow(leftover)), replace = F)
> valid_knn = leftover[sample_valid, ]
> test_knn = leftover[-sample_valid, ]
> pick_model = function(i) {
+ knn_model = kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, train_knn, valid_knn, k = i, scale = TRUE)
+ pred = as.integer(fitted(knn_model)+0.5)
+ return(sum(pred == valid_knn[, 11])/nrow(valid_knn))}
> for (i in 1:25) {
+ print(pick_model(i))}
> knn_model_test = kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, rbind(train_knn, valid_knn), test_knn, k = 9, scale = TRUE)
> pred_test = as.integer(fitted(knn_model_test)+0.5)
> sum(pred_test == test_knn[, 11])/nrow(test_knn)

################## Question 4.2 ###########################
> library(datasets)
> cluster_model_1 = kmeans(iris[, 1:4], centers = 1)
> cluster_model_2 = kmeans(iris[, 1:4], centers = 2)
> cluster_model_3 = kmeans(iris[, 1:4], centers = 3)
> cluster_model_4 = kmeans(iris[, 1:4], centers = 4)
> cluster_model_5 = kmeans(iris[, 1:4], centers = 5)
> cluster_model_1$tot.withinss
	[1] 681.3706
> cluster_model_2$tot.withinss
	[1] 152.348
> cluster_model_3$tot.withinss
	[1] 78.85144
> cluster_model_4$tot.withinss
	[1] 57.26562
> cluster_model_5$tot.withinss
	[1] 46.46117
> flower_accuracy = function(model) {
+ relabeled = factor(model$cluster, levels = c(1, 2, 3), 
+ labels = factor(model$size, levels = c(max(model$size),
+ median(model$size), min(model$size)), labels = c("versicolor",
+ "setosa", "virginica")))
+ return(sum(relabeled == iris[, 5])/nrow(iris))}
> cluster_model_11 = kmeans(iris[, 1], centers = 3)
> cluster_model_22 = kmeans(iris[, 2], centers = 3)
> cluster_model_33 = kmeans(iris[, 3], centers = 3)
> cluster_model_44 = kmeans(iris[, 4], centers = 3)
> cluster_model_12 = kmeans(iris[, 1:2], centers = 3)
> cluster_model_13 = kmeans(iris[, c(1, 3)], centers = 3)
> cluster_model_14 = kmeans(iris[, c(1, 4)], centers = 3)
> cluster_model_23 = kmeans(iris[, 2:3], centers = 3)
> cluster_model_24 = kmeans(iris[, c(2, 4)], centers = 3)
> cluster_model_34 = kmeans(iris[, 3:4], centers = 3)
> cluster_model_123 = kmeans(iris[, 1:3], centers = 3)
> cluster_model_124 = kmeans(iris[, c(1, 2, 4)], centers = 3)
> cluster_model_134 = kmeans(iris[, c(1, 3, 4)], centers = 3)
> cluster_model_234 = kmeans(iris[, 2:4], centers = 3)
> cluster_model_all = kmeans(iris[1:4], centers = 3)
> flower_accuracy(cluster_model_11)
	[1] 0.2133333
> flower_accuracy(cluster_model_22)
	[1] 0.1866667
> flower_accuracy(cluster_model_33)
	[1] 0.38
> flower_accuracy(cluster_model_44)
	[1] 0.96
> flower_accuracy(cluster_model_12)
	[1] 0.82
> flower_accuracy(cluster_model_13)
	[1] 0.88
> flower_accuracy(cluster_model_14)
	[1] 0.2733333
> flower_accuracy(cluster_model_23)
	[1] 0.9133333
> flower_accuracy(cluster_model_24)
	[1] 0.9266667
> flower_accuracy(cluster_model_34)
	[1] 0.96
> flower_accuracy(cluster_model_123)
	[1] 0.88
> flower_accuracy(cluster_model_124)
	[1] 0.8266667
> flower_accuracy(cluster_model_134)
	[1] 0.8933333
> flower_accuracy(cluster_model_234)
	[1] 0.94
> flower_accuracy(cluster_model_all)
	[1] 0.8933333