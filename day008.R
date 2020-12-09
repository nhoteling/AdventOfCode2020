
df <- read.table("data/day008-test.txt")
df$n <- 0


check_acc <- function(df) {
  acc <- 0
  i <- 1
  N <- 0
  while(N==0 & i<=nrow(df)) {
    df$n[i] <- df$n[i]+1          # increment n by 1
    N <- ifelse(df$n[i] > 1,1,0)  # check if N > 1
    if (N==1) {break}
    v <- switch(df$V1[i],
            "nop" = c(acc,i+1),
            "acc" = c(acc+df$V2[i],i+1),
            "jmp" = c(acc, i+df$V2[i]),
            stop("Unknown code!"))
    #print(paste(i,df$V1[i],df$V2[i],df$n[i],"acc =",acc))
    acc <- v[1]
    i <- v[2]
  }
  return(c(acc,i>nrow(df)))
}

acc <- check_acc(df)
print(paste("acc =",acc[1],acc[2]))


## Part 2 ##
# Try changing jmp to nop and then nop to jmp
dnop <- which(df$V1=="nop")
djmp <- which(df$V1=="jmp")

print("---nop to jmp---")
for (i in 1:length(dnop)) {
  df1 <- df
  df1$V1[dnop[i]] <- "jmp"
  acc <- check_acc(df1)
  print(paste("acc =",acc[1],acc[2]))
}

print("---jmp to nop---")
for (i in 1:length(djmp)) {
  df1 <- df
  df1$V1[djmp[i]] <- "nop"
  acc <- check_acc(df1)
  print(paste("acc =",acc[1],acc[2]))
}
#df1 <- df[df$V1=="nop",] <- "jmp"
