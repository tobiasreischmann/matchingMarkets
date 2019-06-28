library(testthat)
library(matchingMarkets)

test_that("Test decentralized DA",{
  equaldist <- function(x) {
    runif(x)
  }
  category <- function(c) {
    function(x) {
      round(runif(x) * c + 0.5)
    }
  }
  
  nStudents <- 200
  nColleges <- 40
  mean <- nStudents/nColleges
  sd <- mean/2
  
  capacityfun <- function(n, mean, sd=1) {
    sapply(round(rnorm(n, mean=mean, sd=sd)), function(x) max(1,x))
  }
  nSlots <- capacityfun(nColleges, mean, sd)
  sum(nSlots)
  daresult <- stabsim3(m=1, nStudents=nStudents, nSlots=nSlots, verbose=FALSE, 
             colleges = c("cx","cy","opening"), 
             students = c("sx", "sy", "idist", "iopen", "age"),
             colleges_fun = c(equaldist,equaldist,category(2)), 
             students_fun = c(equaldist,equaldist,equaldist,equaldist,category(20)),
             outcome = ~ I(sqrt((cx-sx)**2 + (cy-sy)**2)), 
             selection = c(
               student = ~ I(idist * sqrt((cx-sx)**2 + (cy-sy)**2)) + I(iopen * opening),
               colleges = ~ I(age/20) + I(sqrt((cx-sx)**2 + (cy-sy)**2))),private_college_quota = 0.1)
  damatching <- daresult$OUT
  
  expect_gt(daresult$iterations[[1]],1)
  
  hriresult <- hri(s.prefs = daresult$s.prefs,c.prefs = daresult$c.prefs, nSlots=nSlots)
  hrimatching <- hriresult$matchings[hriresult$matchings$cOptimal == 1,]
  
  merged <- merge(damatching, hrimatching, by.x=c('s.id'), by.y=c('student'))
  
  equal_allocations <- sum(merged$c.id == merged$college) 
  num_allocations <- length(merged$c.id)
  
  expect_equal(equal_allocations, num_allocations)
})