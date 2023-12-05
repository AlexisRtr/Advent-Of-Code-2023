library(dplyr)
library(tidyr)


#############################
######### FUNCTION ##########
#############################

get_dataset <- function(string) {
  ### GO OVER ALL ROWS TO EXTRACT A LONG DATASET
  ### Parameter: TABLE WITH STING
  ### Return: df [ID, Color, Balls]
  
  for (i in string) {
    game <- strsplit(i, ":")
    df <- data.frame(ID = "",
                     Color = "",
                     Balls = "")
    
    for (g in game) {
      id <- strsplit(g[1], " ")
      id <- id[[1]][2]
      
      subset <- strsplit(g[2], ";")
      
      for (i in subset[[1]]) {
        set <- strsplit(i, ",")
        
        cubs <- strsplit(set[[1]], " ")
        for (c in cubs) {
          new_row <- data.frame(ID = id,
                                Color = c[3],
                                Balls = c[2])
          
          df <- rbind(df, new_row)
        }
      }
    }
    
  }
  df$Balls <- as.numeric(df$Balls)
  return(df)
}

get_wide_max <- function(dataset){
  ### GET THE MAX OF BALLS PER COLOR AND ID IN WIDE FORMAT
  ### Parameter: df long format
  ### Return:  WIDE df [ID, Color, MAX Balls]
  
  max_df <- dataset %>% group_by(ID,Color) %>% summarise(max_value = max(Balls))
  max_df <- as.data.frame(max_df[-1,])
  
  wide_df_max <- pivot_wider(max_df, names_from = Color, values_from = max_value)
  
  return(wide_df_max)
}

get_filter_df <- function(wide_dataset){
  ### FILTER THE WIDE DF ON MAX BALLS POSSIBLE
  ### Parameter: TABLE WITH STING
  ### Return: df filtered [ID, Color, Balls]
  
  wide_df_max_filtered <- wide_dataset %>% filter((blue <= 14) & (green <= 13) & (red <= 12))
  wide_df_max_filtered$ID <- as.numeric(wide_df_max_filtered$ID)
  
  return(wide_df_max_filtered)
}

get_power <- function(df){
  ### MULTIPLY ALL BALLS NB AND SUM ALL TO GET THE POWER
  ### Parameter: WIDE DF
  ### Return: POEWR = SUM ( max blue* max red * max green)
  
  df$Power <- df$blue*df$green*df$red
  return(sum(df$Power))
}


### INPUT 1
# Example 1
example1 <- read.table("day2/input/example1.txt", sep = "\n")
df <- get_dataset(example1)
wide <- get_wide_max(dataset = df)
wide_filter <- get_filter_df(wide_dataset = wide)
### INPUT 1
sum(wide_filter$ID) # [1] 8
### INPUT 2
get_power(wide) # 2286

# INPUT 1
input <- read.table("day2/input/input1.txt", sep = "\n")
df <- get_dataset(input)
wide <- get_wide_max(dataset = df)
wide_filter <- get_filter_df(wide_dataset = wide)
sum(wide_filter$ID) # [1] 2512
### INPUT 2
get_power(wide) # 67335



