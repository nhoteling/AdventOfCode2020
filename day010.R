
df <- read.table("data/day010.txt") %>% arrange(V1)
df$diff <- c(NA,diff(df$V1,1))
df2 <- na.omit(df %>% group_by(diff) %>% count())
v1 <- df2$n[df2$diff==1]+1
v2 <- df2$n[df2$diff==3]+1
print(paste("Diff-1 =", v1))
print(paste("Diff-3 =", v2))
print(paste(v1,"*",v2,"=",v1*v2))
      
## Part 2 ##


v <- df$V1

# Solution from @brentcrossman on Twitter
trib_seq <- tibble(count=c(1,2,3,4,5,7),
                   trib = c(2,4,7,13,24,44))
dfm <- tibble(jolts=c(0,v,max(v)+3))

perms <- dfm %>% 
  arrange(jolts) %>%
  mutate(dif=jolts-lag(jolts,1, default=0)) %>%
  mutate(running_1 = (dif == lag(dif,1))&(dif==1)) %>%
  mutate(run_id = cumsum(running_1==TRUE & lag(running_1,1)==FALSE)) %>%
  filter(running_1) %>%
  group_by(run_id) %>%
  summarise(count=sum(running_1)) %>%
  left_join(trib_seq,by="count") %>%
  pull(trib)

print(paste("Prod =",format(prod(perms), scientific=FALSE) ))


# Another solution I found online: @TeaStats;  didn't work, don't understand
#dfn <- as.data.frame(unclass(rle(diff(v))))
#dfn <- subset(dfn, values==1)
#count_combos <- function(n) {sum( choose(n-1,0:3))}
#dfn <- transform(dfn, combos=sapply(lengths, count_combos))
#print(paste("prod =", format(prod(dfn$combos), scientific=FALSE)))


#testn <- function(v,d) {
#  dd <- lapply(seq_len(length(d)), function(i) {
#    vv <- v[ -d[[i]] ]
#    success <- !any(diff(vv,1) > 3)
#  })
#  x <- unlist(dd)
#  n <- length(x[x==TRUE])
#  return(n)
#}

#v <- df$V1
#len <- length(v)
#dd <- lapply(seq.int(from=1,to=len,by=1), function(i) {
#  d <- combn(length(v),i,function(x) list(x))
#  n <- testn(v,d)
#})
#vv <- unlist(dd)
#print(paste("Total =",sum(vv)))
