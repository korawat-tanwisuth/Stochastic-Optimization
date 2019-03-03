##1
f <- function(x){
    return(-0.05*(x[1]^(2/3))*(x[2]^(1/3)))
}
gradf <- function(x){
    v1 <- (-0.05)*(2/3)*(x[1]^(-1/3))*(x[2]^(1/3))
    v2 <- (-0.05)*(1/3)*(x[2]^(-2/3))*(x[1]^(2/3))
    return(c(v1,v2))
}

ui <- matrix(c(-12,-15),ncol = 2)
ci <- matrix(c(-100000),ncol = 1)
ans1 <- constrOptim(c(5500,2200),f, grad=gradf, ui, ci)


##2
library(quadprog)
stocks <- read.csv("homework4stocks.csv")
dates <- stocks[,1]
stock_ret <- stocks[,2:ncol(stocks)]
mean_ret <- apply(stock_ret,2,mean)
var_ret <- apply(stock_ret,2,var)
cor_ret <- cor(stock_ret)
n <- ncol(stock_ret)
A <- rbind(rep(1,n),mean_ret,diag(n))
Amat <- t(A)
Dmat <- 2*cov(stock_ret)
dvec <- rep(0,n)
bvec <- c(1,0.01,rep(0,n))
ans2 <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
sol <- ans2$solution
exp_mean <- sum(sol*mean_ret)
exp_var <- sum(sol*var_ret)
exp_sd <- sqrt(exp_var)

##3 
library(knitr)
df<- read.csv("variable_selection.csv")
lm2_1 <- lm(y~x1+x2,df)
lm2_2 <- lm(y~x1+x3,df)
lm2_3 <- lm(y~x2+x3,df)
lm1_1 <- lm(y~x1,df)
lm1_2 <- lm(y~x2,df)
lm1_3 <- lm(y~x3,df)
sse <- function(lm_ob){
    return(sum(lm_ob$residuals^2))
}
sse_vec <- c(sse(lm2_1),sse(lm2_2),sse(lm2_3),sse(lm1_1),sse(lm1_2),sse(lm1_3))
sse_names <- c("x1_x2","x1_x3","x2_x3","x1","x2","x3")
table_sse <- data.frame(sse_vec)
rownames(table_sse) <- sse_names
table_sse

##4
n <- 5
c1<- c(1,1,rep(0,3))
c2 <- c(1,0,-1,-1,0)
c3 <- c(0,1,1,0,-1)
c4 <- c(rep(0,3),1,1)
A <- rbind(c1,c2,c3,c4)
Amat <- t(A)
bvec <- c(710,0,0,710)
dvec <- rep(0,n)
Dmat <- matrix(0,n,n)
diag(Dmat) <- 2*c(1,4,6,12,3)
solve.QP(Dmat, dvec, Amat, bvec, meq=length(bvec))

##5 
nfl <- read.csv("nflratings.csv",header = FALSE)
colnames(nfl) <- c("Week","HT_index","VT_index","HT_score","VT_score")
avg_rate <- 85
APS <- nfl["HT_score"]-nfl["VT_score"]
HT_index <- as.vector(nfl[["HT_index"]])
VT_index <- as.vector(nfl[["VT_index"]])

pred_spread <- function(HTR,VTR,HTA){
    return(HTR-VTR+HTA)
}
n <- max(HT_index)
pred_err <- function(x){
    x <- (x-mean(x))+avg_rate
    HTR <- x[HT_index]
    VTR <- x[VT_index]
    HTA <- x[length(x)]
    prediction <- pred_spread(HTR,VTR,HTA)
    return(sum((APS-prediction)^2))
}


guess <- runif(n+1,70,100)
ans5 <-optim(guess,pred_err,method = "CG")
ans5
n_iter <- 10

lowest <- 100^n
best_sol <- rep(0,n+1)
for(i in 1:n_iter){
    guess <- runif(n+1,70,100)
    res <-optim(guess,pred_err,method = "CG")
    if(res$val < lowest){
        best_sol <- res$par
        lowest <- res$val
    }
}

lowest
best_sol
# pred_err_2 <- function(x){
#     HTR <- x[HT_index]
#     VTR <- x[VT_index]
#     HTA <- x[HT_index]
#     return(sum((APS-pred_spread(HTR,VTR,HTA))^2))
# }
# guess_2 <- c(runif(n,50,100),runif(n,0,10))
# ans5_2 <-optim(guess_2,pred_err_2,method = "CG")
