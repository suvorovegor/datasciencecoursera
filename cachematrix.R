## Put comments here that give an overall description of what your
## functions do

## The first of the two functions, "makeCacheMatrix," caches your matrix and 
## creates the variables that will store the matrix's inverse when it is
## computed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse<- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## The second function, "cacheSolve," computes the inverse of the cached matrix
## and stores it so it can be quickly gotten later.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data...")
    return(inv)
  }
  mat<- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Regretfully, I have only been able to make this "cacheSolve" work with 
## square matrices, as that is the puprpose of the "solve" function.
## I am aware that there are packages that enhance R's capability in finding
## inverses, however, that appears to be outside the scope of the assignment.
