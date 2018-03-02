context("Homework functions")

test_that("func1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(func1(x), x_list)
         })

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  # Testing output for when it is wrong
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func3 computes loglikelihood",
{
  ga_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
  mloglga <- function(alpha, x) {
    if (length(alpha) < 1) stop("alpha must be scalar")
    if (alpha <= 0) stop("alpha must be positive")
    return(- sum(dgamma(x, shape = alpha, log = TRUE)))
  }
  out <- nlm(mloglga, mean(ga_data), x = ga_data)
  out$estimate

  expect_equal(out$estimate, func3(ga_data), tolerance = 1e-3)
})

test_that("func4 computes weighted quantities for mean, var, sd",
{
 x <- 1:10
 y <- runif(10)
 p <- y/sum(y)
 
 d <- cbind(x, p)
 d <- as.data.frame(d)
 
 a = sum(d[,1] * d[,2])
 b = sum(((d[,1] - a)^2) * d[,2])
 c = sqrt(b)
 problist <- list(mean=a,var=b,sd=c)
 expect_identical(func4(d), problist)
})

test_that("func5 computes weighted quantities for mean, var, sd with user check",
{
  # Really no different from func4 in unit testing
  # Could possibly check for if it stops with error; let's show this in unit testing for func6
  x <- 1:10
  y <- runif(10)
  p <- y/sum(y)
  
  d <- cbind(x, p)
  d <- as.data.frame(d)
  
  a = sum(d[,1] * d[,2])
  b = sum(((d[,1] - a)^2) * d[,2])
  c = sqrt(b)
  problist <- list(mean=a,var=b,sd=c)
  expect_identical(func4(d), problist)
})

test_that("func6 throws error for invalid arguments",
{
  notnumeric <- rep("red", length(dat[,1]))
  notnumeric <- cbind(notnumeric, dat[,2])
  
  dat2 <- dat
  dat2[1, 2] <- Inf
  notfinite <- dat2
  
  x1 <- dat[,1]
  size <- length(x1)
  x2 <- 1:size
  badprobability <- cbind(x1, x2)
  
  expect_error(func6(notnumeric))
  expect_error(func6(notfinite))
  expect_error(func6(badprobability))
})

test_that("func7 computes general loglikelihood",
{
  ga_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
  cau_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p7c.txt"))
  bin_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p7b.txt"))

  ga  = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  cau = function(theta, x) dcauchy(x, location = theta, log = TRUE)
  bin = function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)

  mloglga <- function(alpha, x) {
    if (length(alpha) < 1) stop("alpha must be scalar")
    if (alpha <= 0) stop("alpha must be positive")
    return(- sum(dgamma(x, shape = alpha, log = TRUE)))
  }

  mloglcau <- function(alpha, x) {
    if (length(alpha) < 1) stop("alpha must be scalar")
    if (alpha <= 0) stop("alpha must be positive")
    return(- sum(dcauchy(x, location = alpha, log = TRUE)))
  }

  mloglbin <- function(alpha, x) {
    if (length(alpha) < 1) stop("alpha must be scalar")
    #if (alpha <= 0) stop("alpha must be positive")
    return(- sum(dbinom(x, 20, prob = 1 / (1 + exp(- alpha)), log = TRUE) ) )
  }

  # Mean is robust estimator for gamma data
  out1 <- nlm(mloglga, mean(ga_data), x = ga_data)
  out1$estimate

  out2 <- nlm(mloglcau, median(cau_data), x = cau_data)
  out2$estimate

  out3 <- nlm(mloglbin, mean(bin_data), x = bin_data)
  out3$estimate

  expect_equal(out1$estimate, func7(ga_data, ga, c(0, 3)), tolerance = 1e-3)
  expect_equal(out2$estimate, func7(cau_data, cau, c(0, 10)), tolerance = 1e-3)
  expect_equal(out3$estimate, func7(bin_data, bin, c(-100, 100)), tolerance = 1e-3)
})

test_that("bin is numeric data",
{
  expect_true(is.numeric(bin(theta, x)))
})

test_that("cau is numeric data",
{
  expect_true(is.numeric(cau(theta, x)))
})

test_that("ga is numeric data",
{
  expect_true(is.numeric(ga(theta, x)))
})

test_that("xAx computes a scalar value",
{
  a <- matrix(c(1, 2, 3, 2, 4, 5, 3, 5, 6), nrow = 3, byrow = TRUE)
  x <- matrix(c(7, 8, 9), nrow = 3, byrow = TRUE)
  expect_that(is.numeric(xAx(a, x)))
  expect_length(xAx(a, x) %xAx% x, 1)
})

test_that("%xAX% computes a scalar value as a binary operator",
{
  a <- matrix(c(1, 2, 3, 2, 4, 5, 3, 5, 6), nrow = 3, byrow = TRUE)
  x <- matrix(c(7, 8, 9), nrow = 3, byrow = TRUE)
  expect_that(is.numeric(a %xAx% x))
  expect_length(a %xAx% x, 1)
})

test_that("standardize function returns standardized columns of matrix",
{
  a <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
  standardize2 <- function(a)
  {
    stopifnot(nrow(a) > 1)
    colmeans <- apply(a, 2, mean)
    colsds <- apply(a, 2, sd)
    for (i in 1:nrow(a))
    {
      a[i, ] <- (a[i,] - colmeans)/(colsds)
    }
    #a <- t(apply(a, 1, function(a) (a - colmeans)/colsds))
    return(a)
  }
  standardize(a)
  standardize2(a)
  expect_equal(standardize(a), standardize2(a))
})

test_that("myapply function returns apply() family output",
{
  fred <- matrix(1:6, ncol = 2)
  expect_equal(myapply(fred, 1, mean), apply(fred, 1, mean))
  expect_equal(myapply(fred, 2, mean), apply(fred, 2, mean))
  expect_equal(myapply(fred, 1, max), apply(fred, 1, max))
  expect_equal(myapply(fred, 2, max), apply(fred, 2, max))
  #expect_equal(myapply(fred, 1, function(x) quantile(x, 0.75)), apply(fred, 1, function(x) quantile(x, 0.75)))
  #expect_equal(myapply(fred, 2, function(x) quantile(x, 0.75)), apply(fred, 2, function(x) quantile(x, 0.75)))
})

test_that("p6wrapper returns output for median along 3D margins",
{
  skip("This isn't really a function, although it is a problem on the homework")
})

test_that("plotstuff outputs nothing",
{
  skip("This function is a wrapper that outputs a plot from the package ggplot2 on some life expectancy and gdp per capita data")
})
