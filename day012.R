library(readr)
df <- read.table("data/day012.txt",header=FALSE) %>% 
  mutate(move=str_sub(V1,1,1), val=parse_number(V1))



turns <- c("R","L")
moves <- c("E","S","W","N","F")
mdirs <- c(0,90,180,270)

# Update direction
update_dir <- function(trn,val,dir) {
  angle <- mdirs[ which(moves==dir) ]
  angle_new <- ifelse(trn=="R",angle+val,angle-val)
  angle_new <-  ifelse(angle_new>=360,angle_new-360,ifelse(angle_new<0,360+angle_new,angle_new))
  dir_new <- moves[ which(mdirs==angle_new) ]
  #print(paste(dir,angle,trn,val,angle_new,dir_new))
  return(dir_new)
}

# Update coordinates
update_crd <- function(dir,val,xy) {
  xy_new <- switch(dir,
                    "N" = c(xy[1],xy[2]+val),
                    "E" = c(xy[1]+val,xy[2]),
                    "W" = c(xy[1]-val,xy[2]),
                    "S" = c(xy[1],xy[2]-val))
  return(xy_new)
}

test <- function(xy) {
  return(c(xy[1]+1,xy[2]-1))
}


#
#
#
xy <- c(0,0)
direction <- "E"
len <- nrow(df)
for ( i in 1:len) {
  direction <- ifelse(df$move[i] %in% turns, 
                      update_dir(df$move[i],df$val[i],direction),direction)
  
  xy <- if(df$move[i] %in% moves) {
      h <- ifelse(df$move[i]=="F",direction,df$move[i])
      update_crd(h,df$val[i],xy)
    } else {xy}
 #print(paste(i,"cmd =",df$V1[i],"direction =",direction,"xy =",xy[1],xy[2]))
}

#print(paste("Distance =", dist(xy,method="manhattan")))
print(paste("Distance =", sum(abs(xy[1]),abs(xy[2]))))


#
# Part 2
#

rotateVec <- function(xy,angle) {
  x1 <- xy[1]
  y1 <- xy[2]
  b <- angle*pi/180
  x2 <- x1*cos(b) - y1*sin(b)
  y2 <- x1*sin(b) + y1*cos(b)
  return(c(x2,y2))
}

rotate_wypt <- function(cmd,val,wypt) {
  angle <- ifelse(cmd=="L",val,-val)
  wypt_new <- round(rotateVec(wypt,angle))
  return(wypt_new)
}

translate_wypt <- function(cmd,val,xy) {
  xy_new <- switch(cmd,
                   "N" = c(xy[1],xy[2]+val),
                   "E" = c(xy[1]+val,xy[2]),
                   "W" = c(xy[1]-val,xy[2]),
                   "S" = c(xy[1],xy[2]-val))
  return(xy_new)
}

update_wypt <- function(cmd,val,wypt) {
  wypt_new <- if (cmd %in% turns) {
    rotate_wypt(cmd,val,wypt)
  } else {translate_wypt(cmd,val,wypt)}
  return(wypt_new)
}

update_xy <- function(val,wypt,xy) {
  xy_new <- val*wypt + xy
}

wypt_start <- c(10,1)
wypt <- wypt_start
xy <- c(0,0)
for (i in 1:len) {
  cmd <- df$move[i]
  val <- df$val[i]
  
  wypt <- if(cmd != "F") {update_wypt(cmd,val,wypt)} else {wypt}  # update waypoint
  xy <- if (cmd=="F") {val*wypt+xy} else {xy}  # update xy
  
  print(paste(i,cmd,val,"wypts =", wypt[1],wypt[2],"xy =",xy[1],xy[2]))
}

print(paste("Distance2 =", sum(abs(xy[1]),abs(xy[2]))))


