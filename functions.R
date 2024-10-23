cc_var <- function(x){
  chk_data(x)
  x <- names(x)
  y <- gsub("_|\\.", "", x)
  
  for(i in seq_along(x)) {
    if(str_detect(y[i], "[[:punct:]]") |
       str_detect(x[i], "^\\d|\\s|^_") |
       str_detect(x[i], "^\\.\\d")) {
      
      x[i] <- paste0("`", x[i], "`")
    }
  }
  
  cat(x, sep = ", ")
  
}

cc_char <- function(x, col = FALSE, unique = TRUE) {
  chk_character(x)
  
  if(unique) x <- unique(x)
  
  if(col){
    cat(cat("'"), cat(x, sep = "',\n'"), "'", sep = "")
  } else {
    cat(cat("'"), cat(x, sep = "', '"), "'", sep = "")
  }
}

cc_num <- function(x, col = FALSE, unique = TRUE) {
  chk_numeric(x)
  
  if(unique) x <- unique(x)
  
  if(col){
    cat(x, sep = ",\n")
  } else {
    cat(x, sep = ", ")
  }
}
