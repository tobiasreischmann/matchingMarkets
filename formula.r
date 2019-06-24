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
profvis({
stabsim3(m=1, nStudents=nStudents, nSlots=nSlots, verbose=FALSE, 
	 colleges = c("cx","cy","opening"), 
	 students = c("sx", "sy", "idist", "iopen", "age"),
	 colleges_fun = c(equaldist,equaldist,category(2)), 
	 students_fun = c(equaldist,equaldist,equaldist,equaldist,category(20)),
	 outcome = ~ I(sqrt((cx-sx)**2 + (cy-sy)**2)), 
	 selection = c(
		       student = ~ I(idist * sqrt((cx-sx)**2 + (cy-sy)**2)) + I(iopen * opening),
		       colleges = ~ I(age/20) + I(sqrt((cx-sx)**2 + (cy-sy)**2))),private_college_quota = 0.1)
})
