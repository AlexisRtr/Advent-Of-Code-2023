


### INPUT 1
# Example 1
example1 <- read.table("day3/input/example1.txt")
df <- example1 %>% separate(V1, names_col, sep = "")
df <- df %>% replace(is.na(.), ".")

## GET ALL THE SYMBOLS
get_symbols <- function(dataset){
  symbols <- c()
  for (i in dataset){
    new_i <- gsub("[A-Za-z0-9.]", "", i)
    
    symbols <- c(symbols,new_i)  
  }
  symbols <- paste0(symbols,collapse = "")
  symbols <- strsplit(symbols, NULL)[[1]]
  
  return(symbols)
}

symbols <- get_symbols(dataset = example1)




check_around <- function(dataset,position_x, position_y){
  
  # LEFT
  if (position_y >= 3){
    print("LEFT")
    new_y <- position_y - 1
    value <- dataset[position_x,new_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # RIGHT
  if (position_y < ncol(dataset)){
    print("RIGHT")
    new_y <- position_y + 1
    value <- dataset[position_x,new_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # UP
  if (position_x >= 2){
    print("UP")
    new_x <- position_x - 1
    value <- dataset[new_x,position_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # UP LEFT
  if (position_y >= 3 & position_x >=2){
    print("UP LEFT")
    new_x <- position_x - 1
    new_y <- position_y - 1
    # print(c(new_y,new_x))
    value <- dataset[new_x,new_y]
    # print(value)
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # UP RIGHT
  if (position_y < ncol(dataset) & position_x >= 2){
    print("UP RIGHT")
    new_x <- position_x - 1
    new_y <- position_y + 1
    value <- dataset[new_x,new_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # BOTTOM
  if (position_x < ncol(dataset)){
    print("BOTTOM")
    new_x <- position_x + 1
    value <- dataset[new_x,position_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # BOTTOM LEFT
  if (position_y >= 2 & position_x < ncol(dataset)){
    print("BOTTOM LEFT")
    new_x <- position_x + 1
    new_y <- position_y - 1
    value <- dataset[new_x,new_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
  
  # BOTTOM RIGHT
  if (position_x < ncol(dataset) & position_y < nrow(dataset)){
    print("BOTTOM RIGHT")
    new_x <- position_x + 1
    new_y <- position_y + 1
    value <- dataset[new_x,new_y]
    if(value %in% symbols){
      return(TRUE)
    }
  }
}


##TEST 
check_around(dataset = df, position_x = 5, position_y = 4) #RIGHT
check_around(dataset = df, position_x = 10, position_y = 7) # UP
check_around(dataset = df, position_x = 10, position_y = 8) #"UP LEFT"
check_around(dataset = df, position_x = 10, position_y = 4) #UP RIGHT
check_around(dataset = df, position_x = 1, position_y = 3)
check_around(dataset = df, position_x = 1, position_y = 4) #BOTTOM RIGHT
check_around(dataset = df, position_x = 1, position_y = 5) #BOTTOM
check_around(dataset = df, position_x = 10, position_y = 10)
check_around(dataset = df, position_x = 1, position_y = 1)


row <- 1
for (i in example1){
  print("NEW ROW")
  i <- strsplit(i, NULL)[[1]]
  
  col <- 1
  for (r in i){
    print(r)
    
    is_digit <- grepl("^\\d+$", r)
    if(is_digit){
      print(r)
      
      
    }
    
    
    col <- col+1
  }
}

