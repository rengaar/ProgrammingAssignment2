## function makeCacheMatrix consists of several functions with the purpose to
## store desired matrix, get that matrix, cache the inverse of that matrix,
## and get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  #assigns the cached value, initially NULL is assigned
    inv <- NULL
    
  #function that stores the matrix
    set <- function(y) {
      x <<- y
  #deletes the cache since new matrix is assigned
      inv <<- NULL
    }
  
  #returns the stored matrix
    get <- function() x
  
  #caches the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
  
  #gets the cached inverse
    getInverse <- function() inv
  
  #list where each element is a function above
  list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
}

## Function cacheSolve calculates the inverse of the matrix stored by the function
## makeCacheMatrix. If the inverse was already computed, then it returns the value
## of the inverse from the cache without the need to compute it again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
  ## returns the cached inverse if it is already stored  
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  ## gets the matrix, calculates its inverse and stores it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
  ## returns the inverse
    inv
  }
}
