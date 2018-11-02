> library(MASS)
> library(glmnet)
> crime_data = read.table("C:\\Users\\SSamtani\\Downloads\\uscrime.txt", header = TRUE, check.names = FALSE)
> sampleSizeTraining = floor(.6*nrow(crime_data))
> sampleSizeValidation = floor(.4*nrow(crime_data))
> indicesTraining = sort(sample(seq_len(nrow(crime_data)), size=sampleSizeTraining))
> indicesNotTraining = setdiff(seq_len(nrow(crime_data)), indicesTraining)
> indicesValidation = sort(sample(indicesNotTraining, size=sampleSizeValidation))
> train = crime_data[indicesTraining, ]
> val = crime_data[indicesValidation, ]
> fit = lm(Crime ~ ., data = train)
> step_bidir = stepAIC(fit, direction = "both")
> step_for = stepAIC(fit, direction = "forward")
> step_back = stepAIC(fit, direction = "backward")
> fit_bidir = lm(Crime ~ M + Ed + Po1 + Po2 + U1 + U2 + Ineq + Prob + Time, data = val) 
> fit_for = lm(Crime ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob + Time, data = val)
> fit_back = lm(Crime ~ M + Ed + Po1 + Po2 + U1 + U2 + Ineq + Prob + Time, data = val)
> summary(fit_bidir)
> summary(fit_for)
> summary(fit_back)
> c(14, 10, 12, 15.5, .12, 3.6, 20.1, .04, 39)%*%c(39.336, 87.04824, 138, 27.4, -5556.5, 104.8, 68.6, -8056.8, -12.9) -2374
> x = scale(as.matrix(crime_data[1:15]))
> y = as.matrix(crime_data[16])
> set.seed(101)
> lasso = cv.glmnet(x, y, alpha = 1, nfolds = 5, type.measure = 'mse', family = 'gaussian')
> coef(lasso, s=lasso$lambda.min)
> plot(lasso)
> elastic = cv.glmnet(x, y, alpha = .5, nfolds = 5, type.measure = 'mse', family = 'gaussian')
> coef(elastic, s=elastic$lambda.min)
	        		1
	(Intercept) 905.08510638
	M            91.96372970
	So           22.64067510
	Ed          143.78265810
	Po1         246.25293575
	Po2          38.57732124
	LF            0.81136213
	M.F          63.04456652
	Pop          -0.08049603
	NW           16.44317612
	U1          -54.78094946
	U2           91.04598541
	Wealth       29.33322887
	Ineq        198.25493057
	Prob        -86.92790851

> plot(elastic)