library(dplyr)
library(tidyr)


#############################
######### FUNCTION ##########
#############################

get_instruction <- function(df){
  ### EXTRACT THE FIRST ROW AND SPLIT "RL" INTO "R" "L"
  
  instruction <- df[1,]
  instruction <- strsplit(instruction, "")[[1]]
  return(instruction)
}
  



clean_map <- function(df){
  ### GET THE LARGE MAP IN A DATAFRAME FORMAT
  ### Output: df["position", "direction_left", "direction_right"]
  
  df <- df[-1,]
  df <- as.data.frame(df)
  df <- df %>% 
    separate(df, into = c("position", "direction"), sep = "=") 
  
  
  df$direction <- as.character(df$direction)
  
  df$direction <- substr(df$direction , 3, nchar(df$direction) - 1)
  
  df <- df %>% 
    separate(direction, into = c("direction_left", "direction_right"), sep = ",")
  df <- data.frame(lapply(df, function(x) gsub(" ", "", x)))
  
  return(df)
}


get_new_position <- function(index_instruction, index_map){
  ### FROM INDEXES --> GET THE NEWS DIRECTION REGARDING THE R OR L MOVE
  if (instruction[index_instruction] == "R"){
    next_position <- map$direction_right[index_map]
  }
  if (instruction[index_instruction] == "L"){
    next_position <- map$direction_left[index_map]
  }
  return(next_position)
}
 
get_new_position(index = 1) 


nav <- function(){
  ### FOLLOW ALL THE INSTRUCTION AND RETURN THE NB OF STEPS
  next_index <- which(map$position == "AAA")
  new_pos <- map[next_index,1]
  c <- 1
  total <- 1
  while (new_pos != "ZZZ"){
    print(c)
    if (c ==1 & map[c,1] != "AAA"){
      print("error start")
    }
    
    n <- length(instruction)
    if (c > n){
      c <- 1
    }
    
    new_pos <- get_new_position(index_instruction = c, index_map = next_index)
    print(new_pos)
    next_index <- which(map$position == new_pos)
    print(paste0("NExt index is: ",next_index))
    
    c <- c+1
    total <- total+1
    print(c)
    print(total)
  }
  print(paste0("Number of steps from AAA to ZZZ is: ", total-1))
  # return(c-1)
}


# PART 1
### EXAMPLE 1
df <- read.table("Day8/input/example1.txt", sep = "\n")
instruction <- get_instruction(df)
map <- clean_map(df = df)

nav() #2


### EXAMPLE 2
df <- read.table("Day8/input/example2.txt", sep = "\n")
instruction <- get_instruction(df)
map <- clean_map(df = df)

nav() #8

### INPUT 1
df <- read.table("Day8/input/input1.txt", sep = "\n")
instruction <- get_instruction(df)
map <- clean_map(df = df)

nav()

### PART 2
df <- read.table("Day8/input/example3.txt", sep = "\n")
instruction <- get_instruction(df)
map <- clean_map(df = df)
map$position_last <- substr(map$position, nchar(map$position), nchar(map$position))

start_map <- map %>% filter(position_last == 'A')
start_pos_node <- start_map$position

end_map <- map %>% filter(position_last == 'Z')
end_pos_node <- end_map$position

nav <- function(){
  ### FOLLOW ALL THE INSTRUCTION AND RETURN THE NB OF STEPS
  # next_index <- which(map$position == "AAA")
  # print(start_pos_node)
  start_pos_node <- start_pos_node
  c <- 1
  total <- 1
  
  
  last_position <- end_pos_node
  
  while (length(last_position)>0){
    # print(paste0("STEPS : ",total))
    c_together <- 0
    for (node in seq(length(start_pos_node))){
      # print(paste0("Node : ",node))
      pos <- start_pos_node[node]
      next_index <- which(map$position == pos)
      # print(c)
      
      n <- length(instruction)
      
      if (c > n){
        c <- 1
      }
      
      new_pos <- get_new_position(index_instruction = c, index_map = next_index)
      # print(new_pos)
      # print(paste0("NEW POSITION : ",new_pos))
      next_index <- which(map$position == new_pos)
      # print(paste0("Next index is: ",next_index))
      start_pos_node[node] <- new_pos
      # print(start_pos_node)
      pos_check <- new_pos
      # print(pos_check)
      if(pos_check %in% last_position){
        # print(pos_check)
        c_together <- c_together+1
      }
      
      
      # print(paste0("NEW start POSITION :",start_pos_node))
    }
    
    
    if (total %% 10000 == 0) {
      print(paste("Iteration:", total))
    }

    if (c_together > 1){
      print(c_together)
      
    }
    
    if (c_together == length(last_position)){
      return(total)
    }
    c <- c+1
    total <- total+1
    # print("--------------")
  }
  
  # print(paste0("Number of steps from AAA to ZZZ is: ", total-1))
  # return(c-1)
}



df <- read.table("Day8/input/input2.txt", sep = "\n")
instruction <- get_instruction(df)
map <- clean_map(df = df)
map$position_last <- substr(map$position, nchar(map$position), nchar(map$position))

start_map <- map %>% filter(position_last == 'A')
start_pos_node <- start_map$position

end_map <- map %>% filter(position_last == 'Z')
end_pos_node <- end_map$position

nav()

