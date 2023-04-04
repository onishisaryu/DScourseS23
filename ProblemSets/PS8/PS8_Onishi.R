library(modelsummary)
library(nloptr)

# question 4
set.seed(100)
N        <- 100000
K        <- 10
sigma    <- 0.5
# setup X
X        <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1]    <- 1 
# set epsilon
eps      <- rnorm(N,mean=0,sd=0.5)
# set beta
beta     <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
print(beta)
# set Y vector
Y        <- X%*%beta+eps

# question 5: Closed-form solution
betahat1 <- solve(crossprod(X)) %*% crossprod(X,Y)
print(betahat1)
as_tibble(betahat1)
print(paste("OLS estimate from closed-form solution:", betahat1))
mod[['OLS estimate from closed-form solution']] <- betahat1
  
# question 6: Gradient descent
alpha    <- 0.0000003
iter     <- 500
gradient <- function(beta) -2*t(X) %*% (Y - X %*% beta)
set.seed(100)
betahat2 <- floor(runif(ncol(X)))
beta.All  <- matrix(0, ncol = length(betahat2), nrow = iter)
for (i in 1:iter) {
  betahat2      <- beta - alpha * gradient(beta)
  beta.All[i, ] <- betahat2
}
print(paste("OLS estimate from gradient descent:", betahat2))
mod[['OLS estimate from gradient descent']] <- betahat2

# question 7a: L-BFGS algorithm
obj_fun  <- function(beta) {
  return(sum((Y - X %*% beta)^2))
}
obj_grad <- function(beta) {
  return(-2*t(X) %*% (Y - X %*% beta))
}
xstart <-runif(ncol(X))
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8)
res1 <- nloptr(x0=xstart,eval_f=obj_fun,eval_grad_f=obj_grad,opts=options)
betahat_r1 <- as.matrix(res1$solution)
print(paste("OLS estimate using L-BFGS:", res1$solution))

# question 7b: Nelder-Mead algorithm
options_b <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)
res2      <- nloptr( x0=xstart,eval_f=obj_fun,opts=options_b) #change x0
betahat_res2 <- as.matrix(res2$solution)
print(paste("OLS estimate using Nelder-Mead:", res2$solution))

# question 8: beta hat_MLE with L-BFGS
objfun  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}
gradient <- function(theta,Y,X){
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig  <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[length(theta)]       <- dim (X)[1] /sig - crossprod(Y-X%*%beta)/(sig^3)
  return ( grad )
}
# initial values
theta0   <- c(rep(5, ncol(X)), 1) 
theta0   <- c(coef(lm(Y ~ X - 1)), runif(1))
# Algorithm parameters
options  <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)
# Optimize:
result   <- nloptr(x0=theta0,eval_f=objfun, eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)
betahat3 <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]
print(paste("MLE estimate using L-BFGS:", betahat3))
betahat3_mat <- as.matrix(betahat3)

# question 9: lm()
lm <- lm(Y~X -1)
modelsummary(lm,"latex")
betahat4 <- as.matrix(lm$coefficients)
beta_true <- as.matrix(beta)
combined_beta <- cbind(beta_true, betahat1, betahat2, 
                      betahat_res1, betahat_res2, 
                      betahat3_mat, betahat4)
colnames(combined_beta) <- c("True Beta", "closed-form solution", "gradient descent", 
                             "L-BFGS", "Nelder-Mead",
                             "MLE estimate", "lm()") 
comb_beta <- as.tibble(combined_beta)
