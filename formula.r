equaldist <- function(x) {
  runif(x)
}
category <- function(c) {
  function(x) {
    round(runif(x) * c + 0.5)
  }
}

stabsim3(m=1, nStudents=3, nSlots=c(2,3,5), verbose=FALSE, 
	 colleges = c("cx","cy","opening"), 
	 students = c("sx", "sy", "idist", "iopen", "age"),
	 colleges_fun = c(equaldist,equaldist,category(2)), 
	 students_fun = c(equaldist,equaldist,equaldist,equaldist,category(20)),
	 outcome = ~ I(sqrt((cx-sx)**2 + (cy-sy)**2)), 
	 selection = c(
		       student = ~ I(idist * sqrt((cx-sx)**2 + (cy-sy)**2)) + I(iopen * opening),
		       colleges = ~ I(age/20) + I(sqrt((cx-sx)**2 + (cy-sy)**2))))
