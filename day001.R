
#--- Day 1: Report Repair ---
#  After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.
#
#The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.
#
#To save your vacation, you need to get all fifty stars by December 25th.
#
#Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
#  
#  Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
#
#Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

df <- read.table("data/day001.csv")
len <- nrow(df)

d <- lapply(seq_len(len-1), function(i) {
  dd <- lapply(seq.int(from=i+1,to=len,by=1), function(j) {
    sum <- df$V1[i] + df$V1[j]
    if(sum==2020) {print(paste(df$V1[i]," * ",df$V1[j], " = ", df$V1[i]*df$V1[j],sep=""))}
  })
})

#
#  Part 2
#


d <- lapply(seq_len(len-2), function(i) {
  dd <- lapply(seq.int(from=i+1,to=len-1,by=1), function(j) {
    ddd <- lapply(seq.int(from=j+1,to=len,by=1), function(k) {
      sum <- df$V1[i] + df$V1[j] + df$V1[k]
      if(sum==2020) 
      {
        print(paste(df$V1[i]," * ",df$V1[j], " * ",df$V1[k], " = ", 
                    df$V1[i]*df$V1[j]*df$V1[k],sep=""))
      }
    })
  })
})

