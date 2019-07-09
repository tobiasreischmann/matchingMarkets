equaldist <- function(x) {
  runif(x)
}
category <- function(c) {
  function(x) {
    round(runif(x) * c + 0.5)
  }
}
for (i in c(50,100,150,200,300,400)) {
nruns = 1
nStudents <- 2000
nColleges <- i
mean <- nStudents/nColleges
sd <- mean/2

capacityfun <- function(n, mean, sd=1) {
  sapply(round(rnorm(n, mean=mean, sd=sd)), function(x) max(1,x))
}
nSlots <- capacityfun(nColleges, mean, sd)

results <- matrix(nrow = 10, ncol = 2)

colnames(results) <- c('value', 'result')
quota <- 0.4#(i - 1)/10

areasize <- ceiling(sqrt(i/3))
private <- function(x) {
  runif(x) < quota
}
daresult <- stabsim3(m=nruns, nStudents=nStudents, nSlots=nSlots, verbose=TRUE, 
                     colleges = c("cx","cy", "firstpref", "secondpref", "quality", "cidiocat", "cidio1", "cidio2", "private"), 
                     students = c("sx", "sy", "social", "sidiocat", "idist", "iidio", "sidio1", "sidio2", "iquality"),
                     colleges_fun = c(category(areasize),category(areasize),category(3),category(2),equaldist,category(5),equaldist,equaldist,private), 
                     students_fun = c(category(areasize),category(areasize),category(100),category(5),equaldist,equaldist,equaldist,equaldist,equaldist),
                     outcome = ~ I(sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2)), 
                     selection = c(
                       student = ~ I((1000**(firstpref %% 3)) * (abs(cx-sx)<=1) * (abs(cy-sy)<=1)) 
                                 + I((1000**((firstpref + secondpref) %% 3)) * social)
                                 + I((1000**((firstpref - secondpref) %% 3)) * private * ceiling((cidio1 + sidio1 %% 1) * 100)),
                       colleges = ~ I(idist * sqrt(((cx-sx)/areasize)**2 + ((cy-sy)/areasize)**2))
                                  + I(iquality * quality) 
                                  + I(iidio * (cidiocat == sidiocat))
                                  + I(iidio *(cidio2 + sidio2 %% 1))
                       ),
                     private_college_quota = quota,
                     count.waitinglist = function(x) {x}, s.prefs.count = 40)

  sapply(1:nColleges, function(x){
       sum(sapply(1:nStudents, function(y){x %in% daresult$s.prefs[[1]][,y]})
      )})
  sapply(1:nColleges, function(x) {nSlots[x] - sum(daresult$OUT[[1]]$c.id == x)})

  sum(nSlots) - length(daresult$OUT[[1]]$s.id)
  nStudents - length(daresult$OUT[[1]]$s.id)
  iteration <- daresult$iterations[[1]]
  iterationtable <- t(as.matrix(iteration[,-1]))
  bp <- barplot(iterationtable, main=paste("Development of held offers over number of iterations (i=",i,")"),
              xlab="Number of iterations", col=c("darkblue","darkgreen","brown"),
              legend.text = row.names(iterationtable),ylim=c(0,1)) 
  axis(side = 1, at = bp, tick = FALSE, labels=seq(iterationtable[1,]))
  #results[i,1] <- quota
  #results[i,2] <- sum(iteration$new+iteration$altered > 0.05) + 1
}
