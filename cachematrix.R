#  makecachematrix function tis created to initialize a list with 
# 4 functions that get a matrix, set a matrix, get its inverse and set its inverse.

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

#in the next line i am running the makecachematrix function by providing a
#matrix as an argument for which its inverse exists. the output is assined to
#"zz". the output will be a list of 4 functions, get, set, getinverse, setinverse.
# getinverse can be used to fetch the inverse if it exists.

zz<-makeCacheMatrix(x=matrix(c(2,-3,4,1),2,2))

#this is the cachesolve function which will take the list created by 
#the previous function and use it to calculate inverse if it does not exist exists
#already. if it exists, the inverse will be fetched from the cache.


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

# in the next line i am running the cachesolve function using the value "zz" 
# returned by the 
# makeCacheMatrix function. This will return the inverse either by calculating it
# or by fetching from cache if it already exists.

cachesolve(zz)
