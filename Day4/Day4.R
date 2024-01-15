




processing <- function(dataset){
  
  ### REMOVING THE ":" IN COLUNM 2
  dataset$V2 <- gsub(":", "", dataset$V2)
  
  index_symbol <- get_index_symbol(dataset = dataset)
  
  ### DATASET WINNING/PLAYED
  index_winning <- seq(3,index_symbol-1)
  index_played <- seq(index_symbol+1, ncol(example1))
  
  winning_numbers <- example1[,index_winning]
  played_numbers <- example1[,index_played]
  
  return(list(winning_numbers,played_numbers))
}

get_index_symbol <- function(dataset){
  
  first_row <- paste0(dataset[1,])
  index_symbol <- which(first_row == "|")
  
  return(index_symbol)
}


check_win <- function(dataset_winning, dataset_played) {
  df <- data.frame(Card = "",
                   Number = "")
  
  for (i in seq(nrow(dataset_winning))) {
    c <- 0
    for (j in seq(1, ncol(dataset_winning))) {
      ### Check if winning number j in played numbers
      row_played <- paste(dataset_played[i, ])
      
      if (dataset_winning[i, j] %in% row_played) {
        c <- c + 1
        # print(dataset_winning[i,j])
        # print(row_played)
      }
    }
    new_row <- data.frame(Card = example1[i, 2],
                          Number = c)
    df <- rbind(df, new_row)
    
  }
  df <- df[-1, ]
  df$Number <- as.numeric(df$Number)
  df$Points <- 2 ^ (df$Number - 1)
  df <- df %>% filter(Points >= 1)
  
  return(df)
}



# Example 1
example1 <- read.table("day4/input/example1.txt")
example1
df <- processing(dataset = example1)

win <- df[[1]]
play <- df[[2]]

res <- check_win(dataset_winning = win, dataset_played = play)
sum(res$Points)


# INPUT 1
input <- read.table("day4/input/input1.txt")
df <- processing(dataset = input)

win <- df[[1]]
play <- df[[2]]

res <- check_win(dataset_winning = win, dataset_played = play)
sum(res$Points)


### PART 2


check_win <- function(dataset_winning, dataset_played) {
  cards_id <- seq(1, ncol(dataset_winning))
  nb <- rep(0, ncol(dataset_winning))
  
  df <- data.frame(Card = cards_id,
                   Number = nb)
  df[,2] <- as.numeric(df[,2])

  for (i in seq(nrow(dataset_winning))) {
    print("-----------------")
    print(i)
    c <- 0
    for (j in seq(1, ncol(dataset_winning))) {
      ### Check if winning number j in played numbers
      row_played <- paste(dataset_played[i, ])
      
      
      if (dataset_winning[i, j] %in% row_played) {
        c <- c + 1
        print(dataset_winning[i, j])
        print(row_played)
        # print("Value c")
        # print(c)
      }
    }
    # df[i,2] <- c
    # if (){
    #   df[i,2] <- 1 # 1st row to initialize
    # }
    print(paste0("row : ",i, " Value c: ", c))
    
    
    
    if (c > 0){
      number_copies <- df[i,2]
      df[i,2] <- df[i,2]+1 ## ADD VALUE IF MATCH
      
      index_copied <- seq(i+1,i+c)
      print(index_copied)
      df[c(index_copied),2] <- df[c(index_copied),2]+1+number_copies ## ADD COPY
      # print( df)
    }
    print(df)
  }
  df$Number <- as.numeric(df$Number)
  # df$Points <- 2 ^ (df$Number - 1)
  # df <- df %>% filter(Points >= 1)
  
  return(df)
}


example1 <- read.table("day4/input/example1.txt")
example1
df <- processing(dataset = example1)

win <- df[[1]]
play <- df[[2]]
res <- check_win(dataset_winning = win, dataset_played = play)
# na.omit()
sum(res$Number)

example1 <- read.table("day4/input/input1.txt")
example1
df <- processing(dataset = example1)

win <- df[[1]]
play <- df[[2]]
res <- check_win(dataset_winning = win, dataset_played = play)
# na.omit()
sum(res$Number)
