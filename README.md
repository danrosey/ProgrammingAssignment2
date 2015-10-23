## This is my submission for Programming Assignment 2: Lexical Scoping.
## This closely matches the example provided within the course,
## and I have subbed in "solve"  rather than "mean."

## The makeCacheMatrix create a special matrix object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL 
      set <- function(y){
            x <<- y
            cache <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) cache <<- inverse
      getinverse <- function() cache
      list(set=set,get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}

## The cachSolve creates the inverse of makeCacheMatrix.
## If inverse is already stored, it pulls from the cache.

cacheSolve <- function(x,...){
      cache <- x$getinverse()
      if(!is.null(cache)){
            message("getting cached data")
            return(cache)
      }
      data <- x$get()
      cache <- solve(data,...)
      x$setinverse(cache)
      cache
}

