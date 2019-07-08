equaldist <- function(x) {
  runif(x)
}
category <- function(c) {
  function(x) {
    round(runif(x) * c + 0.5)
  }
}

nruns = 1
nStudents <- 2000
nColleges <- 180
mean <- nStudents/nColleges
sd <- mean/2

capacityfun <- function(n, mean, sd=1) {
  sapply(round(rnorm(n, mean=mean, sd=sd)), function(x) max(1,x))
}
nSlots <- capacityfun(nColleges, mean, sd)

results <- matrix(nrow = 10, ncol = 2)
areasize <- 40
colnames(results) <- c('value', 'result')
for (i in seq(1,11)) {
quota <- (i - 1)/10
daresult <- stabsim3(m=nruns, nStudents=nStudents, nSlots=nSlots, verbose=TRUE, 
                     colleges = c("cx","cy","opening", "provider"), 
                     students = c("sx", "sy", "idist", "iopen", "iprovider1", "iprovider2", "iprovider3", "iprovider4", "iprovider5", "age"),
                     colleges_fun = c(category(areasize),category(areasize),category(2),category(5)), 
                     students_fun = c(category(areasize),category(areasize),equaldist,equaldist,equaldist,equaldist,equaldist,equaldist,equaldist,category(20)),
                     outcome = ~ I(sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2)) + I(iopen * (opening-1)), 
                     selection = c(
                       student = ~ I(idist * sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2)) + I(iopen * (opening-1))
                       + I(iprovider1 * (provider==1))
                       + I(iprovider2 * (provider==2))
                       + I(iprovider3 * (provider==3))
                       + I(iprovider4 * (provider==4))
                       + I(iprovider5 * (provider==5)),
                       colleges = ~ I(age/20) + I(sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2))),private_college_quota = quota,
                     count.waitinglist = function(x) {x}, s.prefs.count = 60)

  sum(nSlots) - length(daresult$OUT[[1]]$s.id)
  iteration <- daresult$iterations[[1]]
  iterationtable <- t(as.matrix(iteration[,-1]))
  bp <- barplot(iterationtable, main="Development of held offers over number of iterations",
              xlab="Number of iterations", col=c("darkblue","darkgreen","brown"),
              legend.text = row.names(iterationtable),ylim=c(0,1)) 
  axis(side = 1, at = bp, tick = FALSE, labels=seq(iterationtable[1,]))
  results[i,1] <- quota
  results[i,2] <- sum(iteration$new+iteration$altered > 0.05) + 1
}

