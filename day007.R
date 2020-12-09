library(stringr)
library(readtext)
library(rlist)
library(readr)

sstr <- "shiny gold"

# Read and parse the data
fstr <- readtext("data/day007.txt")
nstr <- unlist(str_split(fstr$text,pattern=".\n"))
dstr <- lapply(seq_len(length(nstr)), function(i) 
  {
    n <- str_replace_all(nstr[i], "bags","")
    n <- str_replace_all(n, "bag","")
    d <- str_split(n,pattern=" contain ")
    df <- do.call(rbind,d)
  })
dfstr <- data.frame(do.call(rbind,dstr))

## Part 1 ##

x <- 1
dbags <- list()
ss <- sstr
while (x == 1)
{
  d <- lapply(seq_len(length(ss)), function(i) {
    dfstr$X1[ str_count(dfstr$X2,ss[i]) > 0 ]
  })
  s <- unlist(d)
  
  if(length(s) > 0)
  {
    #print(s)
    dbags <- list.append(dbags,s)
    ss <- s
  } else {x = 0}
}

total <- unique(unlist(dbags))
print(paste("Total1 =",length(total)))


## Part 2

x <- 1
dbags2 <- list()
vbags2 <- list()
ss <- sstr
while (x == 1)
{
  d <- lapply(seq_len(length(ss)), function(i) {
    dfstr$X2[ str_count(dfstr$X1,ss[i]) > 0 ]
  })
  s <- unlist(d)
  
  
  if(length(s) > 0)
  {
    print(s)
    s1 <- unlist(str_split(s," , "))
    d <- lapply(seq_len(length(s1)), function(i) {
      s2 <- unlist(str_split(str_trim(s1[i])," "))
      df <- data.frame(n=suppressWarnings(as.integer(s2[1])),bag=as.character(paste(s2[2],s2[3],sep=" ")))
      df %>% filter(!is.na(n))
    })
    dfx <- do.call(rbind,d)
    
    #vbags2 <- list.append(vbags2, parse_number(unlist(str_split(s,","))))
    #print(vbags2)
    dbags2 <- list.append(dbags2, dfx)
    ss <- rep(dfx$bag,dfx$n)
    vbags2 <- list.append(vbags2,ss)
  } else {x = 0}
}

df.bags <- do.call(rbind,dbags2)
df.bags <- df.bags[ complete.cases(df.bags), ]
#total <- unlist(dbags2)
print(paste("Total2 =",sum(df.bags$n)))
