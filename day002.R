library(tidyverse)
library(stringr)

df <- read.table("data/day002.txt",sep=" ",header=FALSE,fill=TRUE,stringsAsFactors=FALSE)
names(df) <- c("V1","V2","V3")
# parse min & max values
ss <- str_split(df$V1, pattern="-")
df.tmp <- data.frame(do.call(rbind,ss))
names(df.tmp) <- c("min","max")
df.tmp$min <- as.integer(df.tmp$min)
df.tmp$max <- as.integer(df.tmp$max)
# remove colon
df$V2 <- str_replace(df$V2,":","")             # remove colon
df.new <- cbind(df,df.tmp)                   
df.new$n <- str_count(df.new$V3,df.new$V2)     # count letters
d <- lapply(seq_len(nrow(df.new)), function(i) {between(df.new$n[i],df.new$min[i],df.new$max[i])})
df.new$tf <- do.call(rbind,d)

print(paste("Total =", nrow(df.new),
            "Valid =",nrow(df.new[df.new$tf==TRUE,]),
            "Not valid =", nrow(df.new[df.new$tf==FALSE,])))

d <- lapply(seq_len(nrow(df.new)), function(i) {
  d <- data.frame(do.call(rbind,str_locate_all(df.new$V3[i],df.new$V2[i])))
  idx1 <- df.new$min[i] %in% d$start
  idx2 <- df.new$max[i] %in% d$start
  val1 <- idx1 | idx2
  val2 <- idx1 & idx2
  val <- val1 & !val2
  return(val)
})
df.new$tf2 <- unlist(d)

print(paste("Total =", nrow(df.new),
            "Valid =",nrow(df.new[df.new$tf2==TRUE,]),
            "Not valid =", nrow(df.new[df.new$tf2==FALSE,])))

