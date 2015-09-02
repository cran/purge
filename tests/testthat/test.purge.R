library(purge)
library(lme4)

context("Purged model predictions")

purge_test_helper <- function(unpurged.model, purged.model, test.new.data) {
  preds.purged <- predict(purged.model, newdata=test.new.data)
  expect_equal(length(preds.purged), nrow(test.new.data))
  preds.unpurged <- predict(unpurged.model, newdata=test.new.data)
  expect_equal(preds.purged, preds.unpurged)
}

test_that("lm purge works correctly", {
  sample.size <- 1000
  x <- rnorm(sample.size)
  y <- rnorm(sample.size)
  unpurged.model <- lm(y ~ x)
  purged.model <- purge(unpurged.model)
  test.new.data <- data.frame(x=1:10)
  expect_is(purged.model, 'lm')
  purge_test_helper(unpurged.model, purged.model, test.new.data)
})

test_that("glm purge works correctly", {
  sample.size <- 1000
  x <- rnorm(sample.size)
  y <- as.factor(runif(sample.size) > 0.5)
  unpurged.model <- glm(y ~ x, family=binomial())
  purged.model <- purge(unpurged.model)
  test.new.data <- data.frame(x=1:10)
  expect_is(purged.model, 'glm')
  purge_test_helper(unpurged.model, purged.model, test.new.data)
})

test_that("merMod purge works correctly", {
  library(lme4)
  sample.size <- 1000
  x <- rnorm(sample.size)
  y <- rnorm(sample.size)
  z <- as.factor(runif(sample.size) > 0.5)
  unpurged.model <- lmer(y ~ x + (1|z))
  purged.model <- purge(unpurged.model)
  test.new.data <- data.frame(x=1:10, z=as.factor(runif(10) > 0.5))
  expect_is(purged.model, 'merMod')
  purge_test_helper(unpurged.model, purged.model, test.new.data)
})

test_that("glmerMod purge works correctly", {
  library(lme4)
  sample.size <- 1000
  x <- rnorm(sample.size)
  y <- as.factor(runif(sample.size) > 0.5)
  z <- as.factor(runif(sample.size) > 0.5)
  unpurged.model <- glmer(y ~ x + (1|z), family=binomial())
  purged.model <- purge(unpurged.model)
  test.new.data <- data.frame(x=1:10, z=as.factor(runif(10) > 0.5))
  expect_is(purged.model, 'glmerMod')
  purge_test_helper(unpurged.model, purged.model, test.new.data)
})

test_that("rpart purge works correctly", {
  library(rpart)
  sample.size <- 1000
  x <- rnorm(sample.size)
  y <- x + rnorm(sample.size)
  unpurged.model <- rpart(y ~ x)
  purged.model <- purge(unpurged.model)
  test.new.data <- data.frame(x=1:10)
  expect_is(purged.model, 'rpart')
  purge_test_helper(unpurged.model, purged.model, test.new.data)
})

test_that("randomForest purge works correctly", {
  library(randomForest)
  sample.size <- 1000
  x <- rnorm(sample.size)
  y <- x + rnorm(sample.size)
  unpurged.model <- randomForest(y ~ x, ntree=10)
  purged.model <- purge(unpurged.model)
  test.new.data <- data.frame(x=1:10)
  expect_is(purged.model, 'randomForest')
  purge_test_helper(unpurged.model, purged.model, test.new.data)
})
