# Program used to analyse income of earners.
# Authur: Haoran Hong

income<- read.csv('income_data.csv', sep=',', header=FALSE)

x_i = income$V2

# Summary of data with histogram and numerical summaries: mean, std dev, etc.
hist(x_i, main='Histogram of frequency of income', xlab ='Income (£)', breaks = 100, col = 'green')
boxplot(x_i, ylab = 'Income (£)', col = 'green')
summary(x_i)

# Derivation of maximum likelihood estimator for alpha.
n = length(x_i)
x_m = 80000
alpha_hat = n/sum(log(x_i/x_m)) 
print(alpha_hat)

# Derivation of the Fisher information of aphal and approx distribution of alpha hat.
fisher_info = n/(alpha_hat^2)
print(fisher_info)

# Calculating a 95% equal-tailed confidence interval.
CI_lower = alpha_hat - 1.96*sqrt(fisher_info^-1)
CI_upper = alpha_hat + 1.96*sqrt(fisher_info^-1)
print(CI_lower)
print(CI_upper)

# Histogram and numerical summaries of the esitmation of the distribution of the predicted mean income.
library('Pareto')

# X_pareto = rPareto(n, x_m, alpha_hat)
Y = c()
for(i in 1:10000){
  X = rPareto(1000,x_m, alpha_hat)
  Y[i] = mean(X)
}
hist(Y, main='Pareto', xlab ='Income (£)', breaks = 100, col = 'green')
summary(Y)

# Probabilty that the income of the next year will be lower than the current year.
P = mean(Y<mean(x_i))
print(P)
