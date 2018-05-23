makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse=matrix()) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
zz<-makeCacheMatrix(x=matrix(c(2,-3,4,1),2,2))


cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if((!is.null(m))  ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

cachesolve(zz)
