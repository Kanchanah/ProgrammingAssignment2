##Caching the inverse of a matrix
#Matrix Inversion is usually a costly computation and it may benefit to cache the inverse of a matrix rather than compute it repeatedly. 
#The assignment is to write a pair of functions that cache the inverse of a matrix.
 
## The first function makeCacheMatrix creates a special matrix that
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         
         get <- function() x
         setinverse <- function(inverse) m <<- inverse
         getinverse <- function() m
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }
 
 
#This function cacheSolve creates inverse but it checks to see if the inverse has already been created.
#If so,it gets the inverse from the cache and skips computation.
#Otherwise, it gives the inverse of the matrix and sets the inverse of the matrix in the cache by the setinverse function
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        
 }

