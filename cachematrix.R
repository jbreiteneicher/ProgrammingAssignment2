## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # set value of the inverse of matrix
  setinverse <- function(solve) m <<- solve
  
  # get value of the inversematrix
  getinverse <- function() m
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function
### his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #get value of inverse
  m <- x$getinverse()
  
  #check to see if inverse is already computed
  if( !is.null( m ) ) {
    message("getting cached data")
    
    #if inverse is cached, get the cached value and return
    return( m )
    
  }
  
  #otherwise calculates inverse of data
  invertiblematrix <- x$get()
  m <- solve(invertiblematrix, ...)
  
  #set mean to cached value
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
