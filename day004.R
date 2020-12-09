library(readtext)
library(stringr)

pspt <- c("byr","iyr","eyr","hgt","hcl","ecl","pid")
len <- length(pspt)
# cid field is not required


txt <- readtext(file="data/day004.txt")
d <- str_split(txt$text, pattern="\n\n")            # Each record is separated by '\n\n'

dd <- lapply(seq_len(length(d)), function(i) {
  fstr <- str_replace_all(d[[i]],"\n"," ")          # Convert '\n' to space
  d.str1 <- str_split(fstr, pattern=" ")                # Split by spaces
  ddd <- lapply(seq_len(length(d.str1)), function(j) {
    d.str2 <- str_split(d.str1[[j]], pattern=":")        # 
    df.str <- data.frame(do.call(rbind,d.str2))
    names(df.str) <- c("X1","X2")
    val <- length(pspt[pspt %in% df.str$X1])
    return(val)
  })
  return(unlist(ddd))
})

vals <- unlist(dd)
tfvals <- ifelse(vals==len,"valid","invalid")
print(paste("nvalid =",length(tfvals[tfvals=="valid"])))



# -- Part 2 --

check_vals <- function(df) {
  # byr
  fbyr <- as.integer(df$X2[df$X1=="byr"])
  tfbyr <- (fbyr >= 1920) & (fbyr <= 2002)
  # iyr
  fiyr <- as.integer(df$X2[df$X1=="iyr"])
  tfiyr <- (fiyr >= 2010) & (fiyr <= 2020)
  # eyr
  feyr <- as.integer(df$X2[df$X1=="eyr"])
  tfeyr <- (feyr >= 2020) & (feyr <= 2030)
  # hgt
  fhgt <- as.character(df$X2[df$X1=="hgt"])
  tfhgt <- if (str_count(fhgt,"cm")) {
    x <- as.integer(str_replace(fhgt,"cm",""))
    (x >= 150) & (x <= 193)
  } else if (str_count(fhgt,"in")) {
    x <- as.integer(str_replace(fhgt,"in",""))
    (x >= 59) & (x <= 76)
  } else {FALSE}
  # hcl
  fhcl <- as.character(df$X2[df$X1=="hcl"])
  tfhcl <- (str_sub(fhcl,1,1) == "#") & 
              (sum(str_count(fhcl,as.character(0:9))) +
              sum(str_count(fhcl,letters[1:6])) == 6)
  # ecl
  fecl <- as.character(df$X2[df$X1=="ecl"])
  tfecl <- fecl %in% c("amb","blu","brn","gry","grn","hzl","oth")
  #pid
  fpid <- as.character(df$X2[df$X1=="pid"])
  tfpid <- (sum(str_count(fpid,as.character(0:9))) == 9)
  
  return(tfbyr & tfiyr & tfeyr & tfhgt & tfhcl & tfecl & tfpid)
}

dd <- lapply(seq_len(length(d)), function(i) {
  fstr <- str_replace_all(d[[i]],"\n"," ")          # Convert '\n' to space
  d.str1 <- str_split(fstr, pattern=" ")                # Split by spaces
  ddd <- lapply(seq_len(length(d.str1)), function(j) {
    d.str2 <- str_split(d.str1[[j]], pattern=":")        # 
    df.str <- data.frame(do.call(rbind,d.str2))
    names(df.str) <- c("X1","X2")
    val <- length(pspt[pspt %in% df.str$X1])
    tfvals <- ifelse(val==len,check_vals(df.str),FALSE)
    return(tfvals)
  })
  return(unlist(ddd))
})

tfvals2 <- unlist(dd)
print(paste("nvalid2 =",length(tfvals2[tfvals2==TRUE])))
