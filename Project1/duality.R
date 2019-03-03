ans <- get_answer(P,C,M,L)
df_pos <- cbind(1:length(P),M,ans$duals[-(1:length(L))],ans$duals.from[-(1:length(L))],ans$duals.to[-(1:length(L))])
df_pos <- data.frame(df_pos)
colnames(df_pos) <-  c("Bonds","Maturity","Duals","From", "To")
plot(df_pos$Maturity, df_pos$Duals, main = "Duals by Maturity Periods (Non-Negativity Constraints)", xlab = "Maturity Periods", ylab = "Duals", col = "darkorange", pch = 19, cex = 1.5)


df_liability <- cbind(1:length(L),ans$duals[1:length(L)],ans$duals.from[1:length(L)],ans$duals.to[1:length(L)])
df_liability <- data.frame(df_liability)
colnames(df_liability) <-  c("Maturity","Duals","From", "To")
plot(df_liability$Maturity, df_liability$Duals, main = "Duals by Maturity Periods (Liability Constraints)", xlab = "Maturity Periods", ylab = "Duals", col = "darkorange", pch = 19, cex = 1.5)