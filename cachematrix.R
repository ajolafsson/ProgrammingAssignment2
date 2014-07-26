## The goal of these functions are to store calculations that could become
## time consuming to reduce processing time.

## This function creates an object that stores the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list (set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}


## This function calculates the inverse of the matrix in the function above
## but only does it if the inverse hasn't already been calculated.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get ()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m     
  
  ## Return a matrix that is the inverse of 'x'
}


### testing
a <- seq(1:9)
r <- matrix(a, 3, 3, byrow = TRUE)
c <- matrix(a, 3, 3, byrow = FALSE)
