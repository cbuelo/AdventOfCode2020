# find out what seat you're in
library(tidyverse)

getCode <- function(seatNumber, nOptions, lowChar, highChar){
  seatNum_1indexed = seatNumber + 1
  nChars = log2(nOptions)
  seatCode = ""
  curInterval = nOptions
  minPossible = 1
  for(i in 1:nChars){
    curTheshold = minPossible + (curInterval / 2)
    seatCode = paste0(seatCode, ifelse(seatNum_1indexed < curTheshold, lowChar, highChar))
    minPossible = ifelse(seatNum_1indexed < curTheshold, minPossible, curTheshold)
    curInterval = curInterval / 2
  }
  return(seatCode)
}


code_lut = expand_grid(Row = 0:127, Col = 0:7)
code_lut = code_lut %>% 
  mutate(RowCode = getCode(.$Row, nOptions = 128, lowChar = "F", highChar = "B"),
         ColCode = getCode(.$Col, nOptions=8, lowChar = "L", highChar="R"))

code_lut = code_lut %>% 
  mutate(FullCode = paste0(.$RowCode, .$ColCode),
         SeatID = Row * 8 + Col)

# Part 1
codes = read_lines("d5_input.txt")
code_lut %>% 
  filter(FullCode %in% codes) %>% 
  summarise(max(SeatID))

# Part 2
code_lut %>% 
  filter(!FullCode %in% codes) %>% 
  arrange(SeatID) %>% 
  mutate(SeatID_diffPrev = SeatID - lag(SeatID, default=0), SeatID_diffNext = lead(SeatID, default=0) - SeatID ) %>% 
  filter(SeatID_diffPrev > 1, SeatID_diffNext > 1)
