my_rnorm_abs <- function(x) {
  abs(rnorm(x, sd=sqrt(0.5)))
}

my_cat10 <- function(x) {
  round(abs(runif(x)) * 10 + .5)
}
my_cat2 <- function(x) {
  round(abs(runif(x)) * 2 + .5)
}

nSlots = c(10,8,20,14,12,15,4,23,17,8,20,14,12,15,4,23,17)
nColleges = length(nSlots)
nStudents = 1000

#nSlots = c(3,4,2)
#nColleges = length(nSlots)
#nStudents = 9


sim <- stabsim3(m=1, nStudents=nStudents, nSlots=nSlots, verbose=FALSE,
         colleges = c("cx","cy","opening"),
         students = c("sx", "sy", "idist", "iopen", "age"),
         colleges_fun = c(my_rnorm_abs, my_rnorm_abs, my_cat2),
         students_fun = c(my_rnorm_abs, my_rnorm_abs, my_rnorm_abs, my_rnorm_abs, my_cat10),
         outcome = ~ I(sqrt((cx-sx)**2 + (cy-sy)**2)),
         selection = c(
           student = ~ I(idist * sqrt((cx-sx)**2 + (cy-sy)**2)) +
             I(iopen * opening), colleges = ~ age))
           #colleges = ~ age + I(sqrt((cx-sx)**2 + (cy-sy)**2))))

sim$SELs$Vs
c.prefs <- matrix(sim$SELs$Vs, ncol=nColleges, nrow=nStudents, byrow=FALSE)
s.prefs <- matrix(sim$SELc$Vc, ncol=nStudents, nrow=nColleges, byrow=TRUE)

c.prefs <- apply(-1*c.prefs, 2, order)
s.prefs <- apply(-1*s.prefs, 2, order)

x <- iaa2(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots)
x2 <- hri(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots)

length(x2$matchings[x2$matchings$cOptimal == 0,1])

sx <- x$matchings[sort(x$matchings$student, index.return = TRUE)$ix,]
sx <- sx[sx$college != 0,]
sx2 <- x2$matchings[sort(as.numeric(x2$matchings[x2$matchings$cOptimal == 1,]$student), index.return = TRUE)$ix,]
sum(sx$college == sx2$college) == nStudents - length(x$singles)
