> data = read.table("C:\\Users\\SSamtani\\Downloads\\uscrime.txt", header = TRUE, check.names = FALSE)
> model = prcomp(data[-16], scale = TRUE)
> T = model$rotation
> Z = model$x
> summary(model)
	Importance of components:
                          	PC1    PC2    PC3     PC4     PC5     PC6     PC7
	Standard deviation     2.4534 1.6739 1.4160 1.07806 0.97893 0.74377 0.56729
	Proportion of Variance 0.4013 0.1868 0.1337 0.07748 0.06389 0.03688 0.02145
	Cumulative Proportion  0.4013 0.5880 0.7217 0.79920 0.86308 0.89996 0.92142
                           	PC8     PC9    PC10    PC11    PC12    PC13   PC14
	Standard deviation     0.55444 0.48493 0.44708 0.41915 0.35804 0.26333 0.2418
	Proportion of Variance 0.02049 0.01568 0.01333 0.01171 0.00855 0.00462 0.0039
	Cumulative Proportion  0.94191 0.95759 0.97091 0.98263 0.99117 0.99579 0.9997
                          	PC15
	Standard deviation     0.06793
	Proportion of Variance 0.00031
	Cumulative Proportion  1.00000
> cum_prop = c(.4013, .5880, .7217, .79920, .86308, .89996, .92142, 
+ .94191, .95759, .97091, .98263, .99117, .99579, .9997, 1)
> components = sequence(15)
> plot(components, cum_prop)
> lo = loess(cum_prop ~ components)
> lines(predict(lo), col = 'blue')
> pcr = lm(Crime ~ model$x[,1] + model$x[,2] + model$x[,3] + model$x[,4] + model$x[,5], data = data)
> summary(pcr)
	Call:
	lm(formula = Crime ~ model$x[, 1] + model$x[, 2] + model$x[, 
   		 3] + model$x[, 4] + model$x[, 5], data = data)

	Residuals:
    	Min      1Q  Median      3Q     Max 
	-420.79 -185.01   12.21  146.24  447.86 

	Coefficients:
             		Estimate Std. Error t value Pr(>|t|)    
	(Intercept)    905.09      35.59  25.428  < 2e-16 ***
	model$x[, 1]    65.22      14.67   4.447 6.51e-05 ***
	model$x[, 2]   -70.08      21.49  -3.261  0.00224 ** 
	model$x[, 3]    25.19      25.41   0.992  0.32725    
	model$x[, 4]    69.45      33.37   2.081  0.04374 *  
	model$x[, 5]  -229.04      36.75  -6.232 2.02e-07 ***
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	Residual standard error: 244 on 41 degrees of freedom
	Multiple R-squared:  0.6452,    Adjusted R-squared:  0.6019 
	F-statistic: 14.91 on 5 and 41 DF,  p-value: 2.446e-08
> betas = pcr$coefficients[2:6]
> beta0 = pcr$coefficients[1]
> alphas = model$rotation[,1:5] %*% betas
> sigma = model$scale
> mu = model$center
> alphas_original = t(alphas)*(1/sigma)
> alpha0_original = -t(alphas)%*%(mu/sigma) + beta0
> newdata = c(14, 0, 10, 12, 15.5, .64, 94, 150, 1.1, .12, 3.6, 3200, 20.1, .04, 39)
> alphas_original%*%newdata + alpha0_original
         	[,1]
	[1,] 1388.926