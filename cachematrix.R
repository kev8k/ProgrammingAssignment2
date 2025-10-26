rm(list = ls(all.names = T))
getwd()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # 1. Set the matrix
    x <<- y
    inv <<- NULL   # clear previous inverse
  }
  
  get <- function() x # 2. Get the matrix
  setInverse <- function(inverse) inv <<- inverse # 3. Set the inverse
  getInverse <- function() inv# 4. Get the inverse
  list(set = set,  # Return all 4 as a list
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {# If inverse already cached
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()# Otherwise compute it
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##test the function

m <- matrix(c(2, 3, 2, 4), 2, 2)
cm<-makeCacheMatrix(m)

cacheSolve(cm)
cm$set(matrix(c(1,2,3,4),2,2))
cacheSolve(cm)


