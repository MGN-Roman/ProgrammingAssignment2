## NOTHING NEW. WORKS THE SAME WAY AS GIVEN EXPAMPLE ABOUT CACHED MEAN

##  This function initializes to NULL
##  the "mx_inv" variable wich contains the inverse of matrix
##  and creates and returns a list of four functions:
##  set(new_mx) - updates the original matrix in enviroment and sets "mx_inv" to NULL
##                (when we type "smthn <- makeCacheMatrix(my_matrix)" this function is called
##                 by default with new_mx = my_matrix ????)
##  get() - returns the original matrix
##  setinv(new_mx_inv) - makes this action: "mx_inv <- new_mx_inv" (both variables are matrises)
##  getinv() - returns the "mx_inv" variable

makeCacheMatrix <- function(x = matrix()) {
  mx_inv <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    mx_inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv_mx){
    mx_inv <<- inv_mx
  }
  getinv <- function() {
    mx_inv
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## If you make "smthn <- makeCacheMatrix(my_matrix)" and then call "cacheSolve(smthn)"
## this function will return the inverse of  "my_matrix" matrix.
## If you call "cacheSolve(smthn)" for the first time the function calculates the 
## inverse of "my_matrix" matrix and puts it in cache. Next time you call "cacheSolve(smthn)"
## it returns the inverse of "my_matrix" from cache. If "my_matrix" was changed you have to do
## "smthn <- makeCacheMatrix(my_matrix)" action again and then call "cacheSolve(smthn)"
cacheSolve <- function(x, ..., silent = FALSE) {
        ## Return a matrix that is the inverse of 'x'
  mx_inv <- x$getinv()
   if(!is.null(mx_inv)) {
    if (!silent) {message("getting cached data...")}
    return(mx_inv)
  }
  if (!silent) {message("no cached data. solving...")}
  data <- x$get()
  mx_inv <- solve(data, ...)
  x$setinv(mx_inv)
  mx_inv
}

## USAGE EXAMPLE (CHANGE FALSE to TRUE to run)
if (FALSE) {
my_matrix<-matrix(1:8, ncol=3,nrow=3)
mx_object<-makeCacheMatrix(my_matrix)
cacheSolve(mx_object)
}