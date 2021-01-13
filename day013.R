library(stringr)

fdata <- readLines("data/day013.txt")
tm <- as.integer(fdata[1])
ids0 <- unlist(str_split(fdata[2],pattern=","))
ids1 <- as.integer(ids0[ ids0 != "x" ])
len <- length(ids1)

d <- lapply(seq_len(len), function(i) {
  v <- seq.int(from=0,to=tm+ids1[i],by=ids1[i])
  df <- data.frame(id=ids1[i], tm= v[ v>tm ])
})

df <- do.call(rbind,d)
bus_id <- df$id[ df$tm == min(df$tm) ]
tm_wait <- (min(df$tm)-tm)
print(paste(bus_id,"*",tm_wait,"=",bus_id*tm_wait))


#
#  Part 2  (from SelbyDavid.com)
#
buses <- as.integer(ids0)
sieve <- function(a1, a2, n1, n2, maxit=1e5) {
  x <- a1 + n1*(0:maxit)
  x[which.max(x %% n2 == a2 %% n2)]
}

find_timetable <- function(buses) {
  offsets <- -(seq_along(buses) - 1)[!is.na(buses)]  #a
  buses <- buses[!is.na(buses)]                      #n
  x <- offsets[1]
  for (i in 2:length(buses))
    x <- sieve(x, offsets[i], prod(head(buses, i-1)), buses[i])
  x
}

print(paste(format(find_timetable(buses), sci=FALSE)))