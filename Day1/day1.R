


### Day 1: Input 1

remove_letters <- function(string){
  ### REMOVE ALL THE LETTER OF A STRING
  ### parameter = a list of string 
  ### return = a list of string without letters
  
  list_filter <- c()
  for(i in string){
    filter_i <- gsub("[^0-9.-]", "", i)
    # print(filter_i)
    list_filter <- c(list_filter, filter_i)
  }
  return(list_filter)
}


select_index <- function(string){
  ### FROM A LIST OF STRING, SELECT THE 1ST AND LAST ITEM AND REMOVE THE REST
  ### parameter = a list of string 
  ### return = a list of string of the 1st and last item
  
  list_index <- c()
  for (i in string){
    # print(nchar(i))
    if (nchar(i) == 1){
      list_index <- c(list_index, paste0(i,i))
    }
    else{
      first <- substr(x = i, start = 1, stop = 1)
      last <- substr(x = i, start = nchar(i), stop = nchar(i))
      list_index <- c(list_index, paste0(first, last))
    }
  }
  list_index <- as.numeric(list_index)
  return(list_index)
}

example <- read.table("day1/example.txt")
input <- read.table("day1/input1.txt")
new <- remove_letters(string = input)
new <- select_index(string = new)
sum(unlist(new)) # [1] 55123


# Day 1: option 2
digit <- c("one", "eight","two", "three",  "four", "five", "six", "seven",  "nine")
digit_index <- c("1","8","2","3","4","5", "6", "7", "9")
input

transform_letter_to_number <- function(string){
  
  list_transformed <- c()
  
  for (i in string[,1]){
    new <- i
    n <- nchar(i)
    print(i)
    index_end_word <- 5
    last <- j+5
    for (j in seq(1,n-5)){
      start <- j
      
      print(i)
      small <- substr(x = new, start = start, stop = last)

      print(small)
      for (d in digit){
        index <- which(digit == d)
        
        ## AVOID ISSUE EIGHTWO == EIGH2
        new <- sub(d, digit_index[index], new)
        sub(d, digit_index[index], new)
      }
      print(new)
    }
    break
    
    
    list_transformed <- c(list_transformed,new)
  }
  return(list_transformed)
}

input <- read.table("day1/example2.txt")
input
input <- transform_letter_to_number(string = input)
new <- remove_letters(string = input)
new <- select_index(string = new)
new
sum(unlist(new)) # [1] 55123
