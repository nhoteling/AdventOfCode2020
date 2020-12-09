library(readtext)
library(stringr)


# https://adventofcode.com/2020/day/3/input
f <- readtext(file="data/day003.txt")
v <- unlist(str_split(f$text,pattern="\n"))
len <- length(v)
#d <- lapply(seq_len(len), function(i) {
#  s <- paste(rep(v[i],len),collapse="")
#})
#v <- unlist(d)

#
#  Calcs
#
m <- str_length(v[1])
d <- lapply(seq.int(from=1,to=len,by=1), function(i) {
  idx <- (((i-1)*3) %% m)+1
  x <- str_sub(v[i], idx,idx)
  nstr <- paste(str_sub(v[i],1,idx-1),ifelse(x=="#","X","O"),str_sub(v[i],idx+1),sep="")
  print(paste(v[i],nstr,sep="     "))
  #print(paste("i =",i,"idx =",idx,"char =", x))
  #return(nstr)
  vec <- ifelse(x=="#","X","O")
  return(vec)
})
#nstr <- do.call(rbind,d)
fvec <- unlist(d)
print(paste("ntrees =",length(fvec[fvec=="X"])))



# PART 2

m <- str_length(v[1])
d <- lapply(seq.int(from=1,to=len,by=1), function(i) {
  z <- c(1,3,5,7)
  idx <- (((i-1)*z) %% m)+1
  x <- str_sub(v[i], idx,idx)
  #nstr <- paste(str_sub(v[i],1,idx-1),ifelse(x=="#","X","O"),str_sub(v[i],idx+1),sep="")
  #print(paste(v[i],nstr,sep="     "))
  #print(paste("i =",i,"idx =",idx,"char =", x))
  #return(nstr)
  vec <- ifelse(x=="#","X","O")
  return(vec)
})
df <- data.frame(do.call(rbind,d))


d <- lapply(seq.int(from=1,to=len,by=2), function(i) {
  z <- 1
  j <- (i-1)/2
  idx <- ((j*z) %% m)+1
  x <- str_sub(v[i], idx,idx)
  print(paste("i =",i,"idx =",idx,"char =", x))
  vec <- ifelse(x=="#","X","O")
  return(vec)
})
X5 <- unlist(d)
print(paste("ntrees-1 =",nrow(df[df$X1=="X",])))
print(paste("ntrees-2 =",nrow(df[df$X2=="X",])))
print(paste("ntrees-3 =",nrow(df[df$X3=="X",])))
print(paste("ntrees-4 =",nrow(df[df$X4=="X",])))
print(paste("ntrees-5 =",length(X5[X5=="X"])))

val1 <- as.numeric(nrow(df[df$X1=="X",]))
val2 <- as.numeric(nrow(df[df$X2=="X",]))
val3 <- as.numeric(nrow(df[df$X3=="X",]))
val4 <- as.numeric(nrow(df[df$X4=="X",]))
val5 <- as.numeric(length(X5[X5=="X"]))
val <- val1*val2*val3*val4*val5
                    
print(paste("Product =",val))
