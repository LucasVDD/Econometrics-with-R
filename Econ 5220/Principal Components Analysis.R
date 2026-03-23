# Papers:
  # A tutorial on principal component analysis
  # WIRE's Computational Stats - Component Analysis

# install.packages("ISLR2")
library(ISLR2)
head(Boston)
?Boston
class(Boston)

df <- as.matrix(Boston)

x_demean = scale(df, center = TRUE, scale = TRUE)

x_mean = colMeans(df)

# eigen decomposition of covariance/correlation matrix

c = t(x_demean) %*% x_demean * (1/(nrow(x_demean)-1))

c_eig = eigen(c)

c_eig$values

c_eig$vectors

# PCA for raw data

pca1 = prcomp(df, center = TRUE, scale = TRUE, retx = TRUE)
eig_value = pca1$sdev^2
eig_vector = pca1$rotation
eig_score = x_demean %*% eig_vector

plot(eig_vector[,1], tpe = "l", xaxt = "n")
axis(1, at = 1:ncol(df), labels = colnames(df))

var_exp = eig_value/sum(eig_value)*100
plot(var_exp, type = "l", xlab = "pc", ylab = "Variance Explained")
cul_variance = cumsum(eig_value)/sum(eig_value)*100

plot(cul_variance, type = "l", xlab = "First k PC", ylab = "Cumulative Variance Explained")

plot(eig_score[,1], eig_score[,2])

dim(eig_score)

x_rep = eig_score %*% t(eig_vector)

plot(x_demean[1,], type = "l")
lines(x_rep[1,], col = "red")

j = 6

x_hat = eig_score[,1:j] %*% t(eig_vector[,1:j])

lines(x_hat[1,], col = "blue")
cul_variance


# SVD of raw data

x_svd = svd(x_demean)

u = x_svd$u
sig = x_svd$d
v = x_svd$v

sig

# Example 2
# ===================
nci.labs = NCI60$labs
nci.data = NCI60$data

dim(nci.data)

pca2 = prcomp(nci.data, scale = TRUE)
cols = function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1,2))
plot(pca2$x [,1:2], col = cols(nci.labs), pch = 19, xlab = "21", ylab = "Z2")
plot(pca2$x [,1:3], col = cols(nci.labs), pch = 19, xlab = "21", ylab = "Z3")


eig_value2 = pca2$sdev^2
exp_var = eig_value2/sum(eig_value2)
round(exp_var * 100, 2)

cum_var = cumsum(eig_value2)/sum(eig_value2)
round(cum_var * 100,2)




