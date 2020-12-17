# in a list of number, find the two that sum to 2020 and return their product
library(tidyverse)

validate_pw <- function(x){
  df = as.data.frame(x) %>% 
    separate(x, sep=" ", into=c("NumberAllowed", "Character", "Password"))
  
  df = df %>% 
    mutate(Character = str_remove(Character, pattern=":")) %>% 
    separate(NumberAllowed, sep="-", into=c("MinAllowed", "MaxAllowed"), convert = TRUE) %>% 
    mutate(Noccurances = str_count(Password, pattern=Character))
    
  out = df %>% 
    filter(Noccurances >= MinAllowed & Noccurances <= MaxAllowed) %>% 
    nrow()
  return(out)
}

pwds = read_lines("d2_p1_input.txt")

validate_pw(pwds)

validate_pw_v2 <- function(x){
  df = as.data.frame(x) %>% 
    separate(x, sep=" ", into=c("Indices", "Character", "Password"))
  
  df = df %>% 
    mutate(Character = str_remove(Character, pattern=":")) %>% 
    separate(Indices, sep="-", into=c("Ind1", "Ind2"), convert = TRUE) %>% 
    mutate(Ind1_match = str_sub(Password, start = Ind1, end = Ind1) == Character, Ind2_match = str_sub(Password, start = Ind2, end = Ind2) == Character) %>% 
    mutate(Valid = (Ind1_match + Ind2_match) == 1)
  
  out = sum(df$Valid)
  return(out)
}

validate_pw_v2(pwds)
