remove_column <- function(data, col_names){
  return(data[,!(names(data) %in% col_names)]);
}

replace_in_data <- function(data, col_name, before, after){
  col <- eval(parse(text=(paste("data$",col_name,sep=""))))
  levels(col) <- c(levels(col), after)
  col[col==before] <- after
  eval(parse(text=(paste("data$",col_name,"<- col",sep=""))))
  return(data)
}

replace_in_data_with_regex <- function(data, col_name, regex, after){
  col <- eval(parse(text=(paste("data$",col_name,sep=""))))
  col <- sapply(col,gsub,pattern= regex, replacement=after)
  eval(parse(text=(paste("data$",col_name,"<- col",sep=""))))
  return(data)
}

add_comma <- function(value){
  return(paste('\"',value,'\"',sep=""))
}

add_01_columns <- function(data, original_col){
  values <- eval(parse(text=(paste("levels(data$",original_col,")",sep=""))))
  for(value in values){
    eval(parse(text=paste("data$",original_col,"_",value,"<- ifelse(data$",original_col,"==",add_comma(value),",1,0)",sep="")))
  }
  return(data)
}

