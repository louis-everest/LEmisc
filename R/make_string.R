make_string <- function(x){
  out <- ""
  for(i in 1:length(x)){
    out <- paste0(out,"'", x[[i]],"'", ", ")
  }
  out <- paste0("[", substr(out, 1, nchar(out)-2),"]")
  return(out)
}
