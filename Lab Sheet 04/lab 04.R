setwd("C:\\Users\\it24100477\\Desktop\\it24100477")
branch_data<-read.table("Exercise.txt",header = TRUE, sep = ",")
fix(branch_data)

str(branch_data)
summary(branch_data)

boxplot(branch_data$Sales_X1, main = "boxplot of sales", ylab = "sales")

five_num_summary <- fivenum(branch_data$Advertising_X2)
iqr_advertising <- IQR(branch_data$Advertising_X2)
print(iqr_advertising)

find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}
outliers_years <- find_outliers(branch_data$Years_X3)
print(outliers_years)