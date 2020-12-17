# validate passport batch files

# bFile = read_lines("example_input.txt")
bFile = read_lines("d4_input.txt")

bFile[bFile == ""] = "--"
bFile_comb = paste0(bFile, collapse = " ")
bFile_sep = str_split(bFile_comb, pattern = " -- ")[[1]]

bFile_sep2 = str_split(bFile_sep, pattern = " ")

return_named_vector <- function(x){
  hold_split = str_split(x, pattern=":", simplify = TRUE)
  values = hold_split[, 2]
  names(values) = hold_split[, 1]
  return(values)
}

neededCols = c("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
bFile_sep3 = lapply(bFile_sep2, return_named_vector)
bFile_df = bind_rows(bFile_sep3)
bFile_df = bFile_df %>% 
  mutate(Valid = rowSums(!is.na(.[, all_of(neededCols)])) == 7)

sum(bFile_df$Valid)

# part 2: validate the values
bFile_df_validated = bFile_df
bFile_df_validated = bFile_df_validated %>% 
  mutate(byr = as.numeric(byr),
         iyr = as.numeric(iyr),
         eyr = as.numeric(eyr),
         hgt_unit = "NA")

bFile_df_validated = bFile_df_validated %>% 
  mutate(byr_valid = byr >= 1920 & byr <= 2002,
         iyr_valid = iyr >= 2010 & iyr <= 2020,
         eyr_valid = eyr >= 2020 & eyr <= 2030,
         hgt_unit = ifelse(grepl(.$hgt, pattern = "^[[:digit:]]+cm"), "cm", hgt_unit),
         hgt_unit = ifelse(grepl(.$hgt, pattern = "^[[:digit:]]+in"), "in", hgt_unit),
         hgt_unit = ifelse(hgt_unit == "NA", NA, hgt_unit)
  )

bFile_df_validated = bFile_df_validated %>% 
  mutate(hgt = ifelse(!is.na(hgt_unit), gsub(.$hgt, pattern = c("cm"), replacement = ""), hgt)) %>% 
  mutate(hgt = ifelse(!is.na(hgt_unit), gsub(.$hgt, pattern = c("in"), replacement = ""), hgt)) %>% 
  mutate(hgt = ifelse(is.na(hgt_unit), NA, hgt)) %>% 
  mutate(hgt = as.numeric(hgt))

bFile_df_validated = bFile_df_validated %>% 
  mutate(hgt_valid = ifelse(!is.na(hgt) & !is.na(hgt_unit) & ((hgt_unit == "in" & hgt >= 59 & hgt <= 76) | (hgt_unit == "cm" & hgt >= 150 & hgt <= 193)), TRUE, FALSE))

bFile_df_validated = bFile_df_validated %>% 
  mutate(hcl_valid = grepl(.$hcl, pattern="^#(\\d|[a-f]){6}"),
         ecl_valid = ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
         pid_valid = grepl(.$pid, pattern="^\\d{9}$"))

neededCols_valid = paste0(neededCols, "_valid")

bFile_df_validated = bFile_df_validated %>% 
  mutate(FullyValid = rowSums(!is.na(.[, all_of(neededCols_valid)])) == 7)

sum(bFile_df_validated$FullyValid)


