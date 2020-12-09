library(stringr)
library(readtext)

# Test data
#fstr <- c("FBFBBFFRLR","BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL")
txt <- readtext("data/day005.txt",dvsep = "\n")
fstr <- unlist(str_split(txt$text,pattern="\n"))

d <- lapply(seq_len(length(fstr)), function(i) {
  

str1 <- str_sub(fstr[i],1,7)
str2 <- str_sub(fstr[i],8,10)
# F --> lower half
# B --> upper half

vec1 <- seq.int(from=0,to=127,by=1)
for (i in 1:7)
{
  s <- str_sub(str1,i,i)
  len <- length(vec1)
  x <- len/2
  sb <- if (str_count(s,"F")) {c(vec1[1],vec1[x])} else if (str_count(s,"B")) {c(vec1[x+1],vec1[len])} else {c(-1,-2)}
  vec1 <- if (str_count(s,"F")) {vec1[1]:vec1[x]} else if (str_count(s,"B")) {vec1[x+1]:vec1[len]} else {c(-1,-2)}
  #print(paste("Stuff: ",i,s,sb[1],"to",sb[2],len,x,sep=" "))
}


vec2 <- seq.int(from=0,to=7,by=1)
for (i in 1:3)
{
  s <- str_sub(str2,i,i)
  len <- length(vec2)
  x <- len/2
  sb <- if (str_count(s,"L")) {c(vec2[1],vec2[x])} else if (str_count(s,"R")) {c(vec2[x+1],vec2[len])} else {c(-1,-2)}
  vec2 <- if (str_count(s,"L")) {vec2[1]:vec2[x]} else if (str_count(s,"R")) {vec2[x+1]:vec2[len]} else {c(-1,-2)}
  #print(paste("Stuff: ",i,s,sb[1],"to",sb[2],len,x,sep=" "))
}
#print(paste("Final row =",vec1[1]))
#print(paste("Final column =",vec2[1]))
id <- vec1[1]*8 + vec2[1]
#print(paste("ID = ", id))
return(id)
})

ids <- unlist(d)
print(paste("max id =",max(ids)))

#  Part 2

seats <- seq.int(from=min(ids),to=max(ids),by=1)
missing <- seats[ !(seats %in% ids) ]
