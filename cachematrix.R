## These 2 functions calculates or stores the inverse of an input matrix


## The makeCacheMatrix function returns a list itself containing functions fo:
# 1°) setting a matrix,
# 2°) getting a matrix,
# 3°) inversing the matrix
# 4°) getting the inverted matrix  

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invmat <<- solve
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The cacheSolve function gets a list as an input of the form of makeCachematrix
## it tests whether the inverse matrix is already in cache and either gets it or calculates it and puts it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
  
  
}
