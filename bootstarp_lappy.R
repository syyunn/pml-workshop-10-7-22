library(parallel)
library(MASS)

x <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- seq(1, 1000000)
boot_fx <- function(trial) {
  ind <- sample(100, 100, replace=TRUE)
  result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
  r <- coefficients(result1)
  res <- rbind(data.frame(), r)
}

system.time({
  results <- lapply(trials, boot_fx)
})
