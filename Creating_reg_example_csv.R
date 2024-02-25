x1 <- round(runif(20, min = -300, max = 300), 2)
x2 <- round(runif(20, min = -500, max = 500), 2)
df1 <- data.frame( x1, x2)

write.csv(df1, file = "G:/My Drive/SPRING 2024/RF_Reg_example.csv", row.names = F)
