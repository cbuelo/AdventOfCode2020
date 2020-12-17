# in a list of number, find the two that sum to 2020 and return their product
library(tidyverse)

addTo2020_returnSum <- function(x, nWant = 2){
  combos = expand_grid(x1 = x, x2 = x, x3 = x)
  combos = combos %>% 
    mutate(Sums2 = x1 + x2, Products2 = x1*x2, Sums3 = x1 + x2 + x3, Products3 = x1*x2*x3)
  sumsCol = paste0("Sums", nWant)
  ProdsCol = paste0("Products", nWant)
  answer = combos %>% 
    filter( !!as.symbol(sumsCol) == 2020) %>% 
    select(!!as.symbol(ProdsCol)) %>% 
    pull() %>% 
    unique()
  
  return(answer)
}

numbers = as.numeric(read_lines("d1_p1_input.txt"))

addTo2020_returnSum(numbers)
addTo2020_returnSum(numbers, nWant=3)
