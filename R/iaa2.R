# ----------------------------------------------------------------------------
# R-code (www.r-project.org/) for the Deferred Acceptance Algorithm
#
# Copyright (c) 2013 Thilo Klein
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file LICENSE
#
# ----------------------------------------------------------------------------

#' @title College-Proposing Deferred Acceptance Algorithm for two-sided matching markets
#'
#' @description Finds the optimal assignment of students to colleges in the
#' \href{http://en.wikipedia.org/wiki/Hospital_resident}{college admissions} problem
#' based on the Boston mechanism. The algorithmen is also applicable to the stable marriage problem. The option \code{acceptance="deferred"} instead uses the Gale-Shapley
#' (1962) Deferred Acceptance Algorithm with student offer. The function works with either
#' given or randomly generated preferences.
#'
#' @param nStudents integer indicating the number of students (in the college admissions problem)
#' or men (in the stable marriage problem) in the market. Defaults to \code{ncol(s.prefs)}.
#' @param nColleges integer indicating the number of colleges (in the college admissions problem)
#' or women (in the stable marriage problem) in the market. Defaults to \code{ncol(c.prefs)}.
#' @param nSlots vector of length \code{nColleges} indicating the number of places (i.e.
#' quota) of each college. Defaults to \code{rep(1,nColleges)} for the marriage problem.
#' @param s.prefs matrix of dimension \code{nColleges} \code{x} \code{nStudents} with the \code{j}th
#' column containing student \code{j}'s ranking over colleges in decreasing order of
#' preference (i.e. most preferred first).
#' @param c.prefs matrix of dimension \code{nStudents} \code{x} \code{nColleges} with the \code{i}th
#' column containing college \code{i}'s ranking over students in decreasing order of
#' preference (i.e. most preferred first).
#' @param short_match (Optional)  If \code{FALSE} then in the returned matching, free capacities will be indicated with 0 entries. If \code{TRUE}, free capacities will not be reported in the returned matching but an additonal data.frame is returned that contains free capacities. Defaults to \code{TRUE}.
#' @param seed (Optional) integer setting the state for random number generation.
#'
#' @export
#'
#' @section Minimum required arguments:
#' \code{iaa} requires the following combination of arguments, subject to the matching problem.
#' \describe{
#' \item{\code{nStudents, nColleges}}{Marriage problem with random preferences.}
#' \item{\code{s.prefs, c.prefs}}{Marriage problem with given preferences.}
#' \item{\code{nStudents, nSlots}}{College admissions problem with random preferences.}
#' \item{\code{s.prefs, c.prefs, nSlots}}{College admissions problem with given preferences.}
#' }
#' @return
#' \code{iaa} returns a list with the following elements.
#' \item{s.prefs}{student-side preference matrix.}
#' \item{c.prefs}{college-side preference matrix.}
#' \item{iterations}{number of interations required to find the stable matching.}
#' \item{matchings}{edgelist of matches}
#' \item{singles}{identifier of single (or unmatched) students/men.}
#' @author Thilo Klein
#' @keywords algorithms
#' @references Gale, D. and Shapley, L.S. (1962). College admissions and the stability
#' of marriage. \emph{The American Mathematical Monthly}, 69(1):9--15.
#'
#' Kojima, F. and M.U. Unver (2014). The "Boston" school-choice mechanism. \emph{Economic Theory}, 55(3): 515--544.
#'
#' @examples
#' ##\dontrun{
#' ## --------------------------------
#' ## --- College admission problem
#'
#' s.prefs <- matrix(c(1,2,3,
#'                     1,2,3,
#'                     1,2,3,
#'                     2,1,3,
#'                     2,1,3),
#'                   byrow = FALSE, ncol = 5, nrow = 3); s.prefs
#' c.prefs <- matrix(c(1,4,2,3,5,
#'                     5,2,3,4,1,
#'                     1,2,3,4,5),
#'                   byrow = FALSE, ncol = 3, nrow = 5); c.prefs
#' nSlots <- c(2,2,1)
#'
#' ## Gale-Shapley algorithm
#'  iaa2(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots)$matchings
#'
#' ## Same results for the Gale-Shapley algorithm with hri2() function (but different format)
#'  set.seed(123)
#'  iaa2(nStudents=7, nSlots=c(3,3), acceptance="deferred")$matchings
#'  set.seed(123)
#'  hri2(nStudents=7, nSlots=c(3,3))$matchings TODO: hri2 is not college-proposing
#'  ##}



iaa2 <- function(nStudents=ncol(s.prefs), nColleges=ncol(c.prefs), nSlots=rep(1,nColleges), 
                 s.prefs=NULL, c.prefs=NULL,short_match = TRUE, seed = NULL, c.hist = NULL,
                 matching=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  ## If 'nColleges' not given, obtain it from nSlots
  if(is.null(nColleges)){
    nColleges <- length(nSlots)
  }
  ## If no prefs given, make them randomly:
  if(is.null(s.prefs)){
    s.prefs <- replicate(n=nStudents,sample(seq(from=1,to=nColleges,by=1)))
  }
  if(is.null(c.prefs)){
    c.prefs <- replicate(n=nColleges,sample(seq(from=1,to=nStudents,by=1)))
  }
  
  ## Consistency checks:
  if( dim(s.prefs)[1] != dim(c.prefs)[2] | dim(s.prefs)[2] != dim(c.prefs)[1] |
      dim(s.prefs)[2] != nStudents | dim(c.prefs)[2] != nColleges |
      dim(c.prefs)[1] != nStudents | dim(s.prefs)[1] != nColleges ){
    stop("'s.prefs' and 'c.prefs' must be of dimensions 'nColleges x nStudents' and 'nStudents x nColleges'!")
  }
  if( length(nSlots) != nColleges | length(nSlots) != dim(c.prefs)[2] ){
    stop("length of 'nSlots' must equal 'nColleges' and the number of columns of 'c.prefs'!")
  }
  
  iter <- 0
  
  if(is.null(matching)){
    s.hist    <- rep(0,length=nStudents)  # current college
    c.hist    <- lapply(nSlots, function(x) list())  # student id, which received an offer
    c.slots   <- lapply(nSlots, function(x) rep(0,length=x))  # current students
    c.vacant  <- 1:nColleges 
    s.singles <- 1:nStudents
  } else {
    c.slots   <- lapply(1:nColleges, function(i) {
      students = matching[matching$college == i,]$student
      union(students, rep(0,length=nSlots[i] - length(students)))
    })  # current students
    c.slots  <- mapply(sort,c.slots)
    c.vacant <- (1:nColleges)[lapply(c.slots, `[[`, 1) == 0]
    s.singles <- matching[matching$college == 0,]$student
    s.hist    <- matching[order(matching$student),]$college  # current college
    c.hist    <- lapply(nSlots, function(x) list())  # student id, which received an offer
    #c.hist    <- lapply(1:nColleges, function(x) {
    #                worstcurrentlyheld <- max((1:nStudents)[c.prefs[,x] %in% c.slots[[x]]])
    #                c.prefs[1:worstcurrentlyheld,x]})  # student id, which received an offer
  }

  while(Reduce(min,lapply(c.hist[c.vacant],length)) < nStudents){
    # look at market: all unfilled colleges
    # if history not full (been rejected by all students in their prefs)
    # look at unfilled colleges' history
    # propose to next n students on list
    # while n is the number of free places at the college
    iter         <- iter + 1
    offers       <- list()
    
    ## Look at unfilled colleges that have not yet applied to all students
    temp.colleges <- c(na.omit( c.vacant[unlist(lapply(c.vacant, function(x) length(c.hist[[x]]) < sum(c.prefs[,x] != 0)))] ))
    if(length(temp.colleges)==0){ # if unassigned students have used up all their offers: stop
      return(finish(s.prefs,c.prefs,iter,s.hist,s.singles,c.vacant,short_match))
    }
    
    ## Add to students' offer history
    for(i in 1:length(temp.colleges)){
      # calculate number of offers from minimum out of vacant places and number of students the college had not yet proposed to.
      offerstsofar <- length(c.hist[[temp.colleges[i]]])
      numoffers <- min(sum(c.slots[[temp.colleges[i]]] == 0), nStudents - offerstsofar)
      if(numoffers <= 0){   # Skip college if it has already applied to all students
        offers[[i]] <- list()
        next()
      }
      newoffers = c.prefs[(offerstsofar+1):(offerstsofar+numoffers),temp.colleges[i]]
      c.hist[[temp.colleges[i]]] <- union(newoffers, c.hist[[temp.colleges[i]]])  # add new offers to history of college i.
      offers[[i]] <- newoffers  # offer if unassigned i is index of current round college
    }
    
    ##print(paste("Iteration: ",iter))
    approached <- unique(unlist(offers))	# index of students who received offers
    
    # Dont approach college 0 since it means that the student prefers to stay unmatched
    approached <- approached[!approached == 0]
    
    # Group offers by approached student.
    offersbyapproached <- list()
    for(j in approached){
      offersbyapproached[[j]] <- list()
      }
    for (i in 1:length(temp.colleges)) {
      for (o in offers[[i]]) {
        offersbyapproached[[o]] <- append(offersbyapproached[[o]], temp.colleges[i])
      }
    }

    for(j in approached){
      all_proposers <- offersbyapproached[[j]]

      curr = s.hist[j]
      
      relevant_proposers = union(c(curr),all_proposers)
      sorted_proposers = s.prefs[s.prefs[,j] %in% relevant_proposers,j]
      best = sorted_proposers[1]
      
      if (curr != best) { # if best proposer is different to current held offer, switch to better college
        s.hist[j] <- best
        s.singles = s.singles[s.singles != j] # permanently remove student j from singles
        if (curr != 0) {
          c.slots[[curr]] <- replace(c.slots[[curr]],c.slots[[curr]]==j,0)
          c.slots[[curr]]  <- sort(c.slots[[curr]])
        }
      
        c.slots[[best]][1] = j # add student to list of new college
        c.slots[[best]]  <- sort(c.slots[[best]])  # reset slot order after changes
      }
    }
    c.vacant <- (1:nColleges)[lapply(c.slots, `[[`, 1) == 0]

    if(length(c.vacant)==0){	# if no unassigned students left: stop
      #current.match <- sapply(1:nColleges, function(x) s.mat[,x] %in% c.hist[[x]])
      
      return(finish(s.prefs,c.prefs,iter,s.hist,s.singles,c.vacant,short_match))
    }
    #current.match <- sapply(1:nColleges, function(x) s.mat[,x] %in% c.hist[[x]])
  }
  return(finish(s.prefs,c.prefs,iter,s.hist,s.singles,c.vacant,short_match))
}


# To Sum up and format the output
finish <- function(s.prefs,c.prefs,iter,s.hist,s.singles,c.vacant,short_match){
  if(short_match == FALSE){
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,iterations=iter,matchings=edgefun(x=s.hist),singles=s.singles, vacant=c.vacant))
  }
  else {
    # Format matching
    matching <- edgefun(x=s.hist)
    
    free_caps <- lapply(1:ncol(c.prefs), function(col){
      return(nrow(matching[matching$college == col & matching$student == 0,]))
    })
    free_caps <- data.frame(free_caps)
    colnames(free_caps) <- 1:ncol(c.prefs)
    
    matching <- matching[matching$student != 0, ]
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,iterations=iter,matchings=matching,singles=s.singles, vacant=c.vacant, free_cap = free_caps))
  }
}

## convert match matrix to edgelist
edgefun <- function(x){
  res <- data.frame(student = c(unlist( sapply(1:length(x), function(i){
    rep(i,length(x[[i]]))
  }) )),
  college = unlist(x),
  stringsAsFactors = FALSE)
  #browser()
  res <- with(res, res[order(college, student),])
}