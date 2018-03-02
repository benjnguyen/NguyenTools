
           #'
           #' This function takes dbinom of data
           #' @export
           #' @examples
           #' x <- 1:10
           #' bin(2, x)
           bin = function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)
           