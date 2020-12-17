# navigate a "map", and see how many "trees" you hit

treeMap = read_lines("d3_p1_input.txt")
# treeMap = read_lines("test_input.txt")
# line_length = unique(str_length(treeMap))

countTrees <- function(treeMap, nOver, nDown, line_length = nchar(treeMap[1])){
  lines_hit = seq(from = 1, to = length(treeMap), by=nDown)
  xpositions = 1 + ((nOver*(seq_along(lines_hit)-1)) %% line_length)
  chars = str_sub(treeMap[lines_hit], start=xpositions, end=xpositions)
  
  return(as.numeric(sum(chars == "#")))
}

a = countTrees(treeMap, nOver=1, nDown=1) * countTrees(treeMap, nOver=3, nDown=1) 
b = countTrees(treeMap, nOver=5, nDown=1) * countTrees(treeMap, nOver=7, nDown=1) * countTrees(treeMap, nOver=1, nDown=2)
a*b

countTrees(treeMap, nOver=1, nDown=1) * countTrees(treeMap, nOver=3, nDown=1) * countTrees(treeMap, nOver=5, nDown=1) * countTrees(treeMap, nOver=7, nDown=1) * countTrees(treeMap, nOver=1, nDown=2)

