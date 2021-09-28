
context('read_data')

df1 <- read_data('https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.xls')

test_that('Downloading a big query',{
  expect_error(file_size(url))
})

test_that('Testing the inputs and outputs of your functions',{
  expect_equal(
    df1[[2]]
    ,290)
  
})

test_that('Testing the inputs and outputs of your functions',{
  expect_equal(
    df1[[3]]
    ,32)
  
})

test_that('input is a valid url',{
  expect_error(
    read_data('https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommu.xl')
  )
})

test_that('Recieve the same URL',{
  expect_error(read_data(url))
})

test_that('Number of LÄNs',{
  expect_equal(
    nrow(df1[[1]][!duplicated(df1[[1]]$"LÄN"), ]),21)
})

test_that('Valid Votes and Registered Voters equal to:',{
  expect_equal(sum(as.numeric(df1[[1]]$"Rost Giltiga"))
    ,6231573)
  expect_equal(sum(as.numeric(df1[[1]]$"Rostb"))
    ,7330432)
})

test_that('Voters per Registered rate for Karlshamn', {
  expect_equal(round( (as.numeric(df1[[1]]$`BL tal`)[1] + 
                         as.numeric(df1[[1]]$`OG tal`)[1] + 
                         as.numeric(df1[[1]]$`Rost Giltiga`))[1] / 
                        as.numeric((df1[[1]]$Rostb))[1] * 100,2),
               as.numeric(df1[[1]]$VDT)[1])
})

test_that("Sum of Winner's Percentage1",{
  expect_equal(
   ( as.numeric((df1[[1]]$"M proc")[1])+
      as.numeric(df1[[1]]$"C proc")[1] +
      as.numeric((df1[[1]]$"FP proc"))[1] +
      as.numeric((df1[[1]]$"KD proc"))[1] +
      as.numeric((df1[[1]]$"S proc"))[1] +
      as.numeric((df1[[1]]$"V proc"))[1] +
      as.numeric((df1[[1]]$"MP proc"))[1] +
      as.numeric((df1[[1]]$"SD proc"))[1] +
      as.numeric((df1[[1]]$"FI proc"))[1] +
      as.numeric((df1[[1]]$"OVR proc"))[1])
    
    ,100)
})

