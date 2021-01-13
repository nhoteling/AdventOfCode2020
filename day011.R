library(OpenImageR)  # matrix convolution
library(stringr)

df <- read.table("data/day011.txt")
len <- str_length(df$V1[1])
mm <- t(matrix(unlist(str_split(df$V1,"")),nrow=len))
#m <- mm
mm[mm=="."] <- -1
mm[mm=="L"] <- 0
mm <- matrix(as.numeric(mm),ncol=len)  # ncol here because transpose above
m <- mm
m[ mm==-1 ] <- 0
k <- matrix(c(rep(-1,4),3,rep(-1,4)),3,3)


#
# Part 1 solve with convolution,
# with hints from Twitter via @TeaStats (David Selby)
#

n <- 0
N <- 1
while(N==1) {
  w <- convolution(m,k)
  w[ mm==-1 ] <- NA       # spaces w/out seats
  w[ w>=0 ] <- 1          # seats with people
  w[ w<0 ] <- 0           # seats w/out people
  w[ is.na(w) ] <- 0      # spaces have no people
  
  if (all(w==m)) {break}  # if no change from last iteration
  #dif <- abs(sum(w-m))
  #if (dif == 0) {break}
  n <- n+1
  m <- w
  if (n > 300) {
    print("Too many iterations!")
    break}
}

print(paste("n =",n,"filled =",sum(w)))




## part 2 ##
m <- mm
m[ mm==-1 ] <- NA
seat_ids <- which(!is.na(m), arr.ind = TRUE)  # coordinates of valid seats
dirs <- subset(expand.grid(down = -1:1, right = -1:1), down | right)  # 8 directions

radial_search <- function(seat, directions, radius = 1) {
  if (!nrow(directions))
    return(NULL)
  
  i <- seat[1] + radius * directions[['down']]
  j <- seat[2] + radius * directions[['right']]
  
  in_bounds <- i > 0 & i <= nrow(m) & j > 0 & j <= ncol(m)
  ij <- cbind(i, j)[in_bounds, , drop = FALSE]  # ?
  
  seat_exists <- !is.na( m[ij] )
  remaining_dirs <- directions[in_bounds, ][!seat_exists, ]
  visible <- unname(ij[seat_exists, , drop = FALSE])
  
  rbind(visible, radial_search(seat, remaining_dirs, radius + 1))
}
#
# apply function will run function 'radial_search' over column '1' of 'seat_ids'
# the last option is passed to the function call for 'radial_search'
#
line_of_sight <- apply(seat_ids, 1, radial_search, directions = dirs)




change_places <- function(visible, input) {
  seating_plan <- input             # seating plan is the input matrix
  floor <- is.na(seating_plan)      # NAs in the seating plan are floor space
  
  for (iter in 1:100) {
    new_seating_plan <- seating_plan    # iterate the seating plan
    for (seat in seq_along(visible)) {  # iterate along line-of-sight
      current <- seating_plan[!floor][seat]
      neighbours <- sum(seating_plan[visible[[seat]]])
      if (current & neighbours >= 5) {
        new_seating_plan[!floor][seat] <- 0
      } else if (!current & !neighbours) {
        new_seating_plan[!floor][seat] <- 1
      }
    }
    if (all(seating_plan == new_seating_plan, na.rm = TRUE)) {
      return(c(iter,sum(seating_plan, na.rm = TRUE)))
    }
    seating_plan <- new_seating_plan
  }
  
  stop('Failed to converge after 100 iterations')
}

vv <- change_places(line_of_sight, m)

print(paste("n2 =",vv[1],"filled =",vv[2]))
