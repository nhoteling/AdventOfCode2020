

df <- read.table(file="data/day009.txt")
min <- 25
len <- nrow(df)

d <- lapply(seq.int(from=min+1,to=len,by=1), function(i) {
  z <- df$V1[i]
  dd <- lapply(seq.int(from=(i-min),to=(i-2),by=1), function(j) {
    y <- df$V1[j]
    ddd <- lapply(seq.int(from=(j+1),to=(i-1),by=1), function(k) {
      x <- df$V1[k]
      m <- ifelse(x+y==z,TRUE,FALSE)
      df2 <- data.frame(i,x,y,z,m)
      #print(paste(i,j,k,"   ",x,y,z,x+y,m,sep=" "))
      return(df2)
    })
    return(do.call(rbind,ddd))
  })
  return(do.call(rbind,dd))
})

dfx <- do.call(rbind,d)
dfx2 <- dfx %>% filter(m==FALSE) %>% group_by(i) %>% count() 
mx <- max(dfx2$n)
dfx3 <- dfx2 %>% filter(n==mx)
val <- df$V1[ min(dfx3$i) ]
print(paste("Val = ",val))


d <- lapply(seq.int(from=1,to=len-1,by=1), function(i) {
  dd <- lapply(seq.int(from=i+1,to=len,by=1), function(j) {
    v <- df$V1[i:j]
    num <- sum(v)
    dfn <- data.frame(mn=min(v),mx=max(v),m=(val==num))
    return(dfn)
  })
  return(do.call(rbind,dd))
})


df.n <- do.call(rbind,d)
df.nn <- df.n %>% filter(m==TRUE)
print(paste("Sum = ",df.nn$mn + df.nn$mx))
