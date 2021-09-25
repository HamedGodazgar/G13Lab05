
#' @author Hamed , Omid
#' @description Reading an Excel file directly from internet
#' @example read_data()
#' @export read_data
#' @import readxl
#' @import httr
#' @name read_data
#' @references \url{https://data.val.se/val/val2014/statistik/index.html}
#' @return read excel file as a dataframe df1
#' @title read_data
#' @usage read_data()

read_data <- function(){
  url1<-'https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.xls'
  
  GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
  df1 <- readxl::read_excel(tf, 1L)
  df1 <- df1[-1,]
  names(df1) <- as.matrix(df1[1, ])
  df1 <- df1[-1, ]
  df1[] <- lapply(df1, function(x) type.convert(as.character(x)))
  return(df1)
}



