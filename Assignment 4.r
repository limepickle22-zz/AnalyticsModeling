############# Question 7.2 ####################
> data = read.table("C:\\Users\\SSamtani\\Downloads\\temps.txt", header = TRUE, check.names = FALSE)
> data = as.vector(unlist(data[,2:21]))
> myts = ts(data,start = c(1996,1), end = c(2015,123), frequency = 123)
> hw = HoltWinters(myts, alpha = .05, beta = FALSE, gamma = FALSE)
> plot(hw, col = "yellow", col.predicted = "green")
> predicted_values = as.numeric(hw$fitted[123:2459])
> get_summer_highs = function(data) {
+ start = 1
+ end = 123
+ max_days = vector('numeric')
+ for (i in 1:19) {
+ max_days[i] = which.max(data[start:end])
+ start = end + 1
+ end = end + 123
+ }
+ return(max_days)}
> summer_highs = get_summer_highs(predicted_values)
> years = seq(from = 1997, to = 2015, by = 1)
> plot(years, summer_highs, col = "blue", main = "Peak Summer From 1997 to 2015", xlab = "Year", ylab = "Peak in Days From July 1st")
> abline(lm(summer_highs ~ years))
> lm(summer_highs ~ years)
	Call:
	lm(formula = summer_highs ~ years)

	Coefficients:
	(Intercept)        years  
  	1090.7789      -0.5158

> cor(years, summer_highs)
	-0.2154844
