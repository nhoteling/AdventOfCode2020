library(stringr)
library(dplyr)
library(tidyr)  # for extract function
library(tidyverse)

#
# Part 1 (from David Robinson)
#
input <- data.frame(x=readLines("data/day014-test.txt"))
powers <- 2^seq(35,0)

as_binary <- function(x) {
  c(rep(0L,4), rev(as.integer(intToBits(x))))
}

# These functions not used for part 1, from TeaStats
intTo36Bits <- function(n) {
  bit32 <- rev(as.character(intToBits(n)))
  c(rep(0, 4), as.integer(bit32))
}
binaryToInt <- function(b) {
  b <- as.integer(strsplit(b, '')[[1]])
  sum(b * 2^rev(seq_along(b) - 1))
}
mask <- function(mask, x) {
  mask <- suppressWarnings(as.integer(strsplit(mask, '')[[1]]))
  x <- as.integer(strsplit(x, '')[[1]])
  x[!is.na(mask)] <- mask[!is.na(mask)]
  paste(x, collapse = '')
}
#

instructions <- input %>% 
  extract(x, "mask", "mask = (.*)", remove=FALSE, convert=TRUE) %>%
  extract(x, c("address","value"), c("mem\\[(.*)\\] = (.*)"), remove=FALSE, convert=TRUE) %>%
  fill(mask) %>%
  filter(!is.na(address)) %>%
  mutate(binary_value = map(value, as_binary),
         mask = map(str_split(mask,""), as.integer),
         masked = map2(mask, binary_value, coalesce),
         masked_value = map_dbl(masked, ~ sum(powers * .)))
          
addresses <- numeric(max(instructions$address))
for (i in seq_len(nrow(instructions))) {
  addresses[ instructions$address[i] ] <- instructions$masked_value[i]
}

print(paste("sum =",sum(addresses)))


#
#  Part 2
#

#
#  David Robinson part 2 didn't work for me...
#  Error: Problem with `mutate()` input `addresses`.
#  x vector memory exhausted (limit reached?)
#
#  https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
#  in Terminal:
#   > cd ~
#   > touch .Renviron
#   > open .Renviron
#  Then, add this as the first line:
#  R_MAX_VSIZE=100Gb 
#

floating <- function(mask) {
  n <- sum(powers * mask, na.rm=TRUE) # Get num based on the 1s
  w <- which(is.na(mask)) # get a vector of 2 ^ floating possibilities
  m <- sapply(seq_len(2^length(w))-1, intToBits) %>%
    head(length(w))
  n+c(powers[w] %*% (m==1))
}

p2 <- instructions %>%
  select(-binary_value, -masked) %>%
  mutate(binary_address = map(address, as_binary)) %>%
  mutate(masked_address = map2(mask, binary_address, ~ ifelse(is.na(.x), NA, .x | .y))) %>%
  mutate(addresses = map(masked_address, floating))

addresses <- list()
for (i in seq_len(nrow(p2))) {
  addresses[as.character(p2$addresses[[i]])] <- instructions$value[i]
}

print(paste("sum2 =",sum(unlist(addresses))))

#
# Solution from TeaStats...
#
#program <- read.table('data/day014-test.txt', sep = '=', strip.white = TRUE,
#                      col.names = c('key', 'value')) %>%
#  extract(key, c('dest', 'address'), '(mem|mask)\\[?(\\d*)\\]?',
#          convert = TRUE) %>%
#  mutate(mask = value[which(dest == 'mask')[cumsum(dest == 'mask')]]) %>%
#  filter(dest == 'mem') %>%
#  select(-dest) %>%
#  mutate(value = as.integer(value),
#         value_binary = lapply(value, intTo36Bits),
#         value_binary = sapply(value_binary, paste, collapse = ''),
#         value_masked = mapply(mask, mask, value_binary))


#decode <- function(mask, x) {
#  mask <- suppressWarnings(as.integer(strsplit(mask, '')[[1]]))
#  x <- as.integer(strsplit(x, '')[[1]])
#  x[!is.na(mask) & mask] <- mask[!is.na(mask) & mask] # no change if mask is 0!
#  n_floating <- sum(is.na(mask))
#  decoded <- c()
#  for (i in seq_len(2^n_floating) - 1) {
#    x[is.na(mask)] <- tail(intTo36Bits(i), n_floating)
#    decoded <- c(decoded, paste(x, collapse = ''))
#  }
#  decoded
#}
#
#val <- program %>%
#  select(-value_masked) %>%
#  mutate(address_binary = sapply(lapply(address, intTo36Bits), paste, collapse = ''),
#         address_decoded = mapply(decode, mask, address_binary)) %>%
#  select(address_decoded, value_binary) %>%
#  tidyr::unnest_longer(address_decoded) %>%
#  group_by(address_decoded) %>%
#  summarise(last_integer = binaryToInt(last(value_binary))) %>%
#  pull(last_integer) %>% sum %>% format(scientific = FALSE)
#
#print(paste(val))



