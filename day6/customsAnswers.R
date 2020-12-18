# answers to customs forms
library(tidyverse)
answers = read_lines("d6_input.txt")
groups = cumsum(answers == "") + 1

answers_tibble = tibble(answer = answers, group = groups) %>% 
  filter(answer != "")

# part 1: sum of # of questions that anyone in group answered yes to
answers_grouped = answers_tibble %>% 
  group_by(group) %>% 
  summarise(combd = paste0(answer, collapse="")) %>% 
  rowwise() %>% 
  mutate(nUnLet = n_distinct(first(str_split(combd, pattern = ""))))

sum(answers_grouped$nUnLet)
# part 2: all people in group must have answered yes
answers_grouped2 = answers_tibble %>% 
  group_by(group) %>% 
  summarise(gsize = length(group), combd = paste0(answer, collapse="")) %>% 
  rowwise() %>% 
  mutate(letter_table = list(table(first(str_split(combd, pattern = ""))))) %>% 
  rowwise() %>% 
  mutate(all_yes = list(letter_table == gsize)) %>% 
  mutate(n_all_yes = all_yes %>% simplify() %>% sum())

sum(answers_grouped2$n_all_yes)

# alternative answer from: https://twitter.com/drob/status/1335453278558167040

input <- tibble(x = read_lines("d6_input.txt"))
answers <- input %>% 
  mutate(group = cumsum(x == "")) %>% 
  filter(x != "") %>% 
  add_count(group, name="group_total") %>% 
  separate_rows(x, sep="") %>% 
  filter(x != "")

# part 1
answers %>% 
  distinct(group, x) %>% 
  nrow()

# part 2
answers %>% 
  count(group, x, group_total) %>% 
  filter(n == group_total) %>% 
  nrow()


