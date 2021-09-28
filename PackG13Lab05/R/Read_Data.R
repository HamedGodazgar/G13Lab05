
#' @author Hamed , Omid
#' @description Reading an Excel file directly from internet
#' @example read_data()
#' @export read_data
#' @export file_size
#' @import readxl
#' @import httr
#' @import RCurl
#' @name read_data
#' @references \url{https://data.val.se/val/val2014/statistik/index.html}
#' @return read excel file as a dataframe df1
#' @title read_data
#' @usage read_data(url)
#' @param url the url of a file which we wanna read as a excel file

file_size <- function(url){
  size <- as.numeric(httr::HEAD(url)$headers$`content-length`)
  if (size > 2097152) {
    stop()
  }
} 

read_data <- function(url){
  
  url1<-'https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.xls'
  stopifnot(url == url1)
  stopifnot(url.exists(url))
  file_size(url)
  GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
  #Sys.sleep(15)
  df1 <- readxl::read_excel(tf, 1L)
  df1 <- df1[-1,]
  names(df1) <- as.matrix(df1[1, ])
  df1 <- df1[-1, ]
  #df1[] <- lapply(df1, function(x) type.convert(as.character(x)))
  output <- list(df1, nrow(df1), ncol(df1))
  return(output)
  
}



