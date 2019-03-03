A = matrix(c(1,0.25,-0.45,0.14,1,-0.75,0.55,0.2,1,0.25,0,0.2,1,0.25,0,0.1),nrow=4)
b = matrix(c(250,0,0,37.5),nrow=4)
A_inv = solve(A)
x = A_inv%*%b


z = c(1.5,2.5,3,4.5)
A_temp = matrix(c(2,3,2,4,2,3,3,3,2,7,4,5),nrow = 3)
I = -diag(4)
A = rbind(A_temp,I)
b = c(100000,50000,60000,0,0,0,0)
signs = rep("<=",7)
ans = lp("max", z, A, signs, b)

A= matrix(rep(0,50),nrow=10)
i = 1
k=4
l=k
j= 1
p = 1
for (r in 1:10){
    A[i,j] = 1
    A[i,j+p] = -1
    l=l-1
    i=i+1
    p = p+1
    if(l==0){
        j=j+1
        l=k-1
        k=k-1
        p = 1
    }
    
}
A = rbind(A,rep(1,5))
b = matrix(c(7-52,21-24,7-38,0-45,34-16,25-17,27-7,7-5,3-30,14-52,0),nrow=11)
A.T = t(A)
A.T_A = A.T%*%A
x = (solve(A.T_A)%*%A.T)%*%b
