
context('read_data')

test_that('Downloading a big query',{
  expect_error(file_size(url))
})

test_that('Testing the inputs and outputs of your functions',{
  expect_equal(
    read_data('https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.xls')[[2]]
    ,290)
  
})

