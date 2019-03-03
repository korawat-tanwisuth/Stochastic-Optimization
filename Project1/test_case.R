C <- c(5,3.5,5,3.5,4,9,6,8,9,7)
M <- c(1,2,2,3,4,5,5,6,7,8)
P <- c(102,99,101,98,98,104,100,101,102,94)
L <- c(12000,18000,20000,20000,16000,15000,12000,10000)
optimization(P, C, M, L)
optimization <- function(P, C, M, L){
    # Function takes P:price of the bond, C: coupon of the bond,
    #M: maturity date of the bond, L: the liabilities
    library('lpSolve')
    # set right hand side variables to the liabilities
    RHS<-L
    # the cash outflow must equal the liabilities
    dire<-c(rep('=',length(L)))
    # the objective function minimizes the price
    ob<-P
    1
    Maturity<-M
    Price.Coupon.Index<-length(P)
    A<-matrix(data=NA,nrow=length(L),ncol=Price.Coupon.Index)
    # columns must equal the number of bonds
    for (idx in 1:length(P)){
        # rows must equal the number of liabilities
        for (i in 1:length(L)){
            # fill in rows with the coupon amount if the maturity date is less
            #than or at the liability due date
            if (Maturity[idx]>=i) {
                # if the maturity date is at the liability date then
                #cell bonds for face value
                if (Maturity[idx]==i){
                    A[i,idx]=100+C[idx]
                }
                # fill cell with coupon value if its not maturity date
                else{
                    A[i,idx]=C[idx]
                }
            }
            # if maturity date has passed fill cell with 0
            else{
                A[i,idx]=0
            }
        }
    }
    # solve with lpsolve
    sol=lp(dir='min',objective.in = ob,const.mat = A,const.dir =dire,const.rhs = RHS )
    return(sol)
}
get_answer_z <- function(P,C,M,L){
    
    ###Input: M,C, Output:A
    get_A <- function(M, C){
        num_row <- length(1:max(M))
        face_val_vec = rep(100, length(P))
        A<- matrix(0,nrow= num_row,ncol= length(P))
        
        for(i in 1:num_row){
            coupon <- (M >= i)*C
            face_val <- (M == i)*face_val_vec
            temp <- coupon + face_val
            A[i,] <- temp
        }
        return(A)
    }
    get_Z <- function(M){
        num_row <- length(1:max(M))
        Z_1 <- -diag(num_row)
        Z_2 <- ifelse((row(Z_1)-col(Z_1))==1 & (row(Z_1)>col(Z_1)),1,0)
        Z <- Z_1+Z_2
        Z <- Z[,1:num_row]
        return(Z)
    }
    A <- cbind(get_A(M,C),get_Z(M))
    num_row <- length(1:max(M))
    signs <- rep("=", num_row)
    ans <- lp("min",c(P,rep(0,num_row)),A,signs,L)###Add all coefficients of decision variables
    return(ans)
}
get_answer <- function(P,C,M,L){
    
    ###Input: M,C, Output:A
    get_A <- function(M, C){
        num_row <- length(1:max(M))
        face_val_vec = rep(100, length(P))
        A<- matrix(0,nrow= num_row,ncol= length(P))
        
        for(i in 1:num_row){
            coupon <- (M >= i)*C
            face_val <- (M == i)*face_val_vec
            temp <- coupon + face_val
            A[i,] <- temp
        }
        return(A)
    }
    # get_Z <- function(M){
    #     num_row <- length(1:max(M))
    #     Z_1 <- -diag(num_row)
    #     Z_2 <- ifelse((row(Z_1)-col(Z_1))==1 & (row(Z_1)>col(Z_1)),1,0)
    #     Z <- Z_1+Z_2
    #     Z <- Z[,1:num_row]
    #     return(Z)
    # }
    A <- get_A(M,C)
    num_row <- length(1:max(M))
    signs <- rep("=", num_row)
    ans <- lp("min",P,A,signs,L)###Add all coefficients of decision variables
    return(ans)
}

library(lpSolve)
temp <- rep(0,100)
temp_2 <- rep(0,100)
for(i in 1:1000){
M <- c(1,2,2,3,4,5,5,6,7,8)
C <- round(runif(10,2,10))
L <- round(runif(8, 8000,20000))
P <- round(runif(10,95,110))
k<- get_answer(P,C,M,L)
k_2<-get_answer_z(P,C,M,L)
r<-optimization(P,C,M,L)
diff <- sum(k$solution-r$solution)
diff2 <- sum(k_2$objval-r$objval)
temp[i] <- diff
temp_2[i]<-diff2
print(k_2$solution)
}
temp
sget_answer_z(P,C,M,L)$solution
optimization(P,C,M,L)$solution
get_answer(P,C,M,L)$solution
