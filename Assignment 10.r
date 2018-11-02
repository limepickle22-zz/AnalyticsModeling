> library(DAAG)
> library(perturb)
> breast_cancer_data = read.table("C:\\Users\\SSamtani\\Downloads\\breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, 
+ header = FALSE, sep = ",")
> breast_cancer_data[which(breast_cancer_data$V7 == "?"),]
> nrow(breast_cancer_data[which(breast_cancer_data$V7 == "?"),])/nrow(breast_cancer_data)
> mean(na.omit(as.numeric(breast_cancer_data$V7)))
> median(na.omit(as.numeric(breast_cancer_data$V7)))
> no_missing_values = na.omit(as.data.frame(sapply(breast_cancer_data, as.numeric)))
> attach(no_missing_values)
> reg = lm(V7 ~ ., data = no_missing_values)
> for (i in 1:16) {
+ print(predict(reg, newdata = breast_cancer_data[which(breast_cancer_data$V7 == "?"),][i,]))
+ }
> pert = perturb(reg, pvars = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"), prange = 
+ c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
> for (i in 1:16) {
+ print(as.numeric(pert$coeff.table[2,][1]) + as.numeric(pert$coeff.table[2,][-1])%*%as.numeric(
+ breast_cancer_data[which(breast_cancer_data$V7 == "?"),][i,])[-7])
+ }
