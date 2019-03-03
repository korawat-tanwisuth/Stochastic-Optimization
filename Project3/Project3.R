load("data.Rdata")
library(glmnet)
library(gurobi)

df <- data.frame(cbind(X,y))
fit_lasso_cv <- cv.glmnet(X, y, alpha = 1)
lasso_min <- glmnet(X,y, alpha = 1, lambda = fit_lasso_cv$lambda.min)
min_lambda_coef<- coef(lasso_min)[-1]
length(which(min_lambda_coef!=0))

lasso_1se<- glmnet(X,y, alpha = 1, lambda = fit_lasso_cv$lambda.1se)
onese_lambda_coef<- coef(lasso_1se)[-1]
length(which(onese_lambda_coef!=0))


MIQP <- function(X,y,num_beta,k,M){
    model <- list()
    sum_z_constr <- matrix(c(rep(0,num_beta),rep(1,num_beta)),nrow=1)
    upper_m_constr <- cbind(diag(rep(1,num_beta)),-diag(rep(M,num_beta)))
    lower_m_constr <- cbind(diag(rep(-1,num_beta)),-diag(rep(M,num_beta)))
    model$A <- rbind(sum_z_constr,upper_m_constr,lower_m_constr)
    
    model$obj <- c((-t(X)%*%y),rep(0,num_beta))
    model$objcon <- t(y)%*%y
    Q <- matrix(0,2*num_beta,2*num_beta)
    Q[(1:num_beta),(1:num_beta)] <- t(X)%*%X
    model$Q <- 0.5*Q
    model$sense <- c(rep("<=",nrow(model$A)))
    model$vtype <- c(rep("C",num_beta),rep("B",num_beta))
    model$rhs <- c(k,rep(0,nrow(model$A)-1))
    model$modelsense <- "min"
    ans <- gurobi(model)
    return(ans)
}
done <- FALSE
num_beta <- 64
k <- 8
M <- 0.01
while(!done){
    print("hi")
    ans <- MIQP(X,y,num_beta,k,M)
    if(abs(max(ans$x[1:num_beta])) >= M){
        M <- 2*M
        next
    }
    else{
        break
    }
}

norm_2 <- function(v){
    return(t(v)%*%v)
}
beta_hat_MIQP <- ans$x[1:num_beta]
beta_hat_min_lasso <- min_lambda_coef
beta_hat_1se_lasso <- onese_lambda_coef
beta_0 <- beta_real
pred_error <- function(X,beta_hat,beta_0){
    estimate <- X%*%beta_hat
    truth <- X%*%beta_0
    error <- estimate-truth
    numerator <- norm_2(error)
    denominator <- norm_2(truth)
    return(numerator/denominator)
}
MIQP_error <- pred_error(X,beta_hat_MIQP,beta_0)
min_lasso_error <- pred_error(X,beta_hat_min_lasso,beta_0)
onese_lasso_error <- pred_error(X,beta_hat_1se_lasso,beta_0)
