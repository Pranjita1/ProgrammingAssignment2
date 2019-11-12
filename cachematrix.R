## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#here I have defined a function named: makeCacheMatrix, where the function creates a special matrix object that can cache it's inverse.

#steps followed: set matrix -> get value -> set inverse -> get value
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  
  
  set <- function(y) {
    
    
    
    x <<- y
    
    
    
    inv <<- NULL
    
    
    
  }
  get <- function() x
  
  
  
  setinv <- function(inverse) inv <<- inverse
  
  
  
  getinv <- function() inv
  
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
  
  
}

  

}


## Write a short comment describing this function
#computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  
  
  if(!is.null(inv)) {
    
    
    
    message("getting cached result")
    
    
    
    return(inv)
    
    
    
  }
  
  
  
  data <- x$get()
  
  
  
  inv <- solve(data, ...)
  
  
  
  x$setinv(inv)
  
  
  
  inv
  
  
  
}

#small bit to check if implementation is correct
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
