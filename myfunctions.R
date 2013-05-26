remove_column <- function(data, col_names){
  return(data[,!(names(data) %in% col_names)]);
}

replace_in_data <- function(data, col, before, after){
  levels(col) <- c(levels(col), after)
  col[col==before] <- after
  data$new_col <- col
  return(data)
}

replace_in_data_with_regex <- function(data, col, before, after){
  col <- sapply(col,gsub,pattern= before, replacement=after)
  data$new_col <- col
  return(data)
}

add_01_columns <- function(data, original_col, map){
  
}