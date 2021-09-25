
expect_that(10 , equals(10))

expect_that(10 , is_identical_to(10))

expect_that(c('one'=1 , 'two'=2), is_equivalant_to(1:2))

model <- lm(mpg~wt , data = mtcars)
expect_that(model, is_a('lm'))

string <- 'Testing is Fun!'
expect_that(strting, matches('Testing'))

a <- list(1:10 , letters)
expect_that(str(a), prints_text('List of 2'))

expect_that(library(mgcv), shows_message('This is mgcv'))

expect_that(log(-1), gives_warning())

expect_that(1/'a', throw_error('on-numeric argument'))

expect_that(x, is_true()) /// expect_true(x)
expect_that(x, is_false()) /// expect_false(x)
expect_that(x, is_a(y)) /// expect_is(x, y)
expect_that(x, equals(y)) /// expect_equal(x, y)
expect_that(x, is_equivalent_to(y)) /// expect_equivalent(x, y)
expect_that(x, is_identical_to(y)) /// expect_identical(x, y)
expect_that(x, matches(y)) /// expect_matches(x, y)
expect_that(x, prints_text(y)) /// expect_output(x, y)
expect_that(x, shows_message(y)) /// expect_message(x, y)
expect_that(x, gives_warning(y)) /// expect_warning(x, y)
expect_that(x, throws_error(y)) /// expect_error(x, y)

context('read_data')

test_that('Downloading a big query',{
  
})

test_that('Testing the limits of the API configuration',{
  
})

test_that('Testing the inputs and outputs of your functions',{
  
})

test_that('function allways return a given value for a specific set of inputs',{
  
})

test_that("lenreg rejects errounous input", {
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

# linreg <- lm
# 

test_that("class is correct", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_s3_class(linreg_mod, "linreg")
})

test_that("print() works", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_output(print(linreg_mod),"linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  expect_output(print(linreg_mod),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("pred() works", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_equal(round(unname(pred(linreg_mod)[c(1,5,7)]),2), c(1.85, 1.53, 1.09))    
})

test_that("resid() works", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_equal(round(unname(resid(linreg_mod)[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
})

test_that("coef() works", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_true(all(round(unname(coef(linreg_mod)),2) %in% c(-2.52, -1.34, 1.78)))
})


test_that("summary() works", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_output(summary(linreg_mod), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")  
  expect_output(summary(linreg_mod), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(summary(linreg_mod), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
  expect_output(summary(linreg_mod), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")
})












