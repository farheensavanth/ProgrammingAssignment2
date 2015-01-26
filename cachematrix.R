## makeCachematrix is a function that returns a list of 4 functions
# It stores a matrix and a cached value of the inverse of the matrix 
# it consists of the following 4 functions
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInv   get the cached value (inverse of the matrix)
# * getInv     get the cached value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
  # cache is set to null initially as nothing is cached
  cache <- NULL
  
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
}

#get matrix returns the stored matrix
getMatrix <- function() {
  x
}

# cache the given argument and solve calculates the inverse of the matrix
cacheInv <- function(solve) {
  cache <<- solve
}

# get the cached value
getInv <- function() {
  cache
}

# return a list. Each named element of the list is a function
list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInv = cacheInv, getInv = getInv)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInv()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  else{
    #  get the matrix, caclulate the inverse and store it in the cache
    d <- y$getMatrix()
    inverse <- solve(d)
    
    y$cacheInv(inverse)
    
   
    return(inverse)
  }
}


