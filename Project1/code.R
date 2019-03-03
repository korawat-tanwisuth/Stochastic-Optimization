library(lpSolve)
C <- c(5,3.5,5,3.5,4,9,6,8,9,7)
M <- c(1,2,2,3,4,5,5,6,7,8)
P <- c(102,99,101,98,98,104,100,101,102,94)
L <- c(12000,18000,20000,20000,16000,15000,12000,10000)

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
    ans <- lp("min",P,A,signs,L,compute.sens = T)###Add all coefficients of decision variables
    return(ans)
}

ans <- get_answer(P,C,M,L)
df_pos <- cbind(1:length(P),M,ans$duals[-(1:length(L))],ans$duals.from[-(1:length(L))],ans$duals.to[-(1:length(L))])
df_pos <- data.frame(df_pos)
colnames(df_pos) <-  c("Bonds","Maturity","Duals","From", "To")
plot(df_pos$Maturity, df_pos$Duals, main = "Duals by Maturity Periods (Non-Negativity Constraints)", xlab = "Maturity Periods", ylab = "Duals", col = "darkorange", pch = 19, cex = 1.5)


df_liability <- cbind(1:length(L),ans$duals[1:length(L)],ans$duals.from[1:length(L)],ans$duals.to[1:length(L)])
df_liability <- data.frame(df_liability)
colnames(df_liability) <-  c("Maturity","Duals","From", "To")
plot(df_liability$Maturity, df_liability$Duals, main = "Duals by Maturity Periods (Liability Constraints)", xlab = "Maturity Periods", ylab = "Duals", col = "darkorange", pch = 19, cex = 1.5)
# lpmodel<-make.lp(num_row,(length(P)+num_row))
# for(i in 1:ncol(A)){
#     set.column(lpmodel,i,A[,i])
# }
# set.rhs(lpmodel,L)
# set.constr.type(lpmodel, rep("=", num_row))
# set.objfn(lpmodel,c(P,rep(0,num_row)))
# solve(lpmodel)
