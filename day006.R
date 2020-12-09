library(readtext)
library(stringr)

txt <- readtext("data/day006.txt")
fstr <- unlist(str_split(txt$text,pattern="\n\n"))
fstr2 <- str_replace_all(fstr, "\n","")

d <- lapply(seq_len(length(fstr2)), function(i) 
  {
    v <- str_detect(fstr2[i],letters)
    len <- length(v[v==TRUE])
  })
val <- sum(unlist(d))
print(paste("Total1 = ",val))

# PART 2


d <- lapply(seq_len(length(fstr2)), function(i) 
{
  n <- str_count(fstr[i],"\n")+1        # number of lines
  m <- str_count(fstr[i],letters)
  v <- (m==n)
  len <- length(v[v==TRUE])
})
val <- sum(unlist(d))
print(paste("Total2 = ",val))