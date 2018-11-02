> library(car)
> data = read.table("C:\\Users\\SSamtani\\Downloads\\uscrime.txt", header = TRUE, check.names = FALSE)
> model = lm(Crime~., data = data)
> summary(model)

	Call:
	lm(formula = Crime ~ ., data = data)

	Residuals:
    		Min      1Q  Median      3Q     Max 
		-395.74  -98.09   -6.69  112.99  512.67 

	Coefficients:
              	     Estimate Std. Error t value Pr(>|t|)    
	(Intercept) -5.984e+03  1.628e+03  -3.675 0.000893 ***
	M            8.783e+01  4.171e+01   2.106 0.043443 *  
	So          -3.803e+00  1.488e+02  -0.026 0.979765    
	Ed           1.883e+02  6.209e+01   3.033 0.004861 ** 
	Po1          1.928e+02  1.061e+02   1.817 0.078892 .  
	Po2         -1.094e+02  1.175e+02  -0.931 0.358830    
	LF          -6.638e+02  1.470e+03  -0.452 0.654654    
	M.F          1.741e+01  2.035e+01   0.855 0.398995    
	Pop         -7.330e-01  1.290e+00  -0.568 0.573845    
	NW           4.204e+00  6.481e+00   0.649 0.521279    
	U1          -5.827e+03  4.210e+03  -1.384 0.176238    
	U2           1.678e+02  8.234e+01   2.038 0.050161 .  
	Wealth       9.617e-02  1.037e-01   0.928 0.360754    
	Ineq         7.067e+01  2.272e+01   3.111 0.003983 ** 
	Prob        -4.855e+03  2.272e+03  -2.137 0.040627 *  
	Time        -3.479e+00  7.165e+00  -0.486 0.630708    
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	Residual standard error: 209.1 on 31 degrees of freedom
	Multiple R-squared:  0.8031,    Adjusted R-squared:  0.7078 
	F-statistic: 8.429 on 15 and 31 DF,  p-value: 3.539e-07
> cor(data['Po1'], data['Po2'])
> cor(data['U1'], data['U2'])
> model2 = lm(Crime~M+So+Ed+Po2+LF+M.F+Pop+NW+U1+Wealth+Ineq+Prob+Time, data = data)
> summary(model2)
	
	Call:
	lm(formula = Crime ~ M + So + Ed + Po2 + LF + M.F + Pop + NW + 
    	U1 + Wealth + Ineq + Prob + Time, data = data)

	Residuals:
    		Min      1Q  Median      3Q     Max 
		-404.23 -143.24    9.81  118.65  443.25 

	Coefficients:
              	      Estimate Std. Error t value Pr(>|t|)    
	(Intercept) -6137.1525  1651.9734  -3.715 0.000750 ***
	M              74.3897    44.7166   1.664 0.105666    
	So             77.0383   157.7771   0.488 0.628585    
	Ed            126.2788    63.0938   2.001 0.053623 .  
	Po2           114.3172    27.8182   4.109 0.000246 ***
	LF            129.3218  1559.3577   0.083 0.934406    
	M.F            14.0729    21.7033   0.648 0.521199    
	Pop            -0.6999     1.4014  -0.499 0.620814    
	NW              1.4407     6.9273   0.208 0.836524    
	U1           1446.4739  2895.2195   0.500 0.620666    
	Wealth          0.1401     0.1113   1.259 0.217035    
	Ineq           78.9164    24.5351   3.216 0.002904 ** 
	Prob        -3873.8517  2409.6387  -1.608 0.117439    
	Time            2.4966     7.3693   0.339 0.736920    
	---
	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

	Residual standard error: 227.7 on 33 degrees of freedom
	Multiple R-squared:  0.7514,    Adjusted R-squared:  0.6535 
	F-statistic: 7.674 on 13 and 33 DF,  p-value: 1.115e-06

> newdata = data.frame(M = 14, So = 0, Ed = 10, Po2 = 15.5, LF = .64, M.F = 94, 
+ Pop = 150, NW = 1.1, U1 = .12, Wealth = 3200, Ineq = 20.1, Prob = .04, Time = 39)
> predict(model2, newdata)