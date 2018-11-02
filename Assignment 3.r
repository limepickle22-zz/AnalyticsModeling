##################### Question 5.1 #######################
> library(outliers)
> data = read.table("C:\\Users\\SSamtani\\Documents\\uscrime.txt")
> converted_crime = as.numeric(as.matrix(data[-1, 16]))
> grubbs.test(converted_crime, type = 10)

        	Grubbs test for one outlier

	data:  converted_crime
	G = 2.81290, U = 0.82426, p-value = 0.07887
	alternative hypothesis: highest value 1993 is an outlier

##################### Question 6.2
> library(ggplot2)
> data2 = read.table("C:\\Users\\SSamtani\\Downloads\\temps.txt", header = TRUE, check.names = FALSE)
> cusum = function(data, C, T, year) {
+ count = 0
+ for (i in 1:nrow(data)) {
+ s = 88 - as.numeric(as.matrix(data[year][i,])) - C
+ if (count + s > 0) {
+ count = count + s}
+ else {
+ count = 0}
+ if (count > T) {
+ return(data[i, 1])}}
+ return(count)}
> for (i in 1:20) {
+ print(cusum(data2, 5, 50, colnames(data2)[i+1]))}
> dates_of_change = c(30, 27, 38, 24, 10, 27, 28, 28, 19, 40, 28, 41, 27, 17,
+ 33, 31, 37, 27, 29, 25)
> x = seq(1, 20)
> qplot(x, dates_of_change, xlab = "Years Since 1995", ylab = "Days Since Aug. 31",
+ geom = c("point", "smooth"))
> for (i in 1:20) {
+ avg_sum_temp[i] = mean(data2[colnames(data2)[i+1]][1:62+dates_of_change[i],])}
> avg_sum_temp
> cusum2 = function(data, C, T) {
+ count = 0
+ for (i in 1:20) {
+ s = data[i] - 85.06452 - C
+ if (count + s > 0) {
+ count = count + s}
+ else {
+ count = 0}
+ if (count > T) {
+ return(data[i])}}
+ return(count)}
> cusum2(avg_sum_temp, 1, 20)
> cusum2(avg_sum_temp, 1, 10)
> cusum2(avg_sum_temp, 1, 6)
> cusum2(avg_sum_temp, 1, 5)