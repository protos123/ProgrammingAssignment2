## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function is the one that contains the 4 functions
#needed. Contains get,set,getinv,setinv

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL # this variable saves the inversion of the result
  set <- function(y) {
    x <<- y
    matinv <<- NULL # it also initialises mativ to null
  }
  get <- function() x # this returns the input matrix
  setinv <- function(inv) matinv <<- inv # this is for setting the inversed matrix
  getinv <- function() matinv # this returns the inversed matrix
  #this is the list that contains the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function
#this function solves the matrix returning the inverse

cacheSolve <- function(x, ...) {
  matriz <- x$getinv() # gets the inverted matrix from object 
  if(!is.null(matriz)) { # if this value is null
    message("Getting Cached Data")
    return(matriz) # return the calculated inversion
  }
  data <- x$get() # if this isn't null we grab the value of x$get
  matriz <- solve(data) # solving matrix
  x$setinv(matriz) # set value of object x$setinv
  matriz # return matrix
}
