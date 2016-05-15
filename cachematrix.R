## Assignment 2 Week 3 R Programming
## The two functions below help increase efficiency of calculating the inverse of many matrices by
## setting up a cache of results so calculations dont need to be repeated

## Creates a matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ##setmean <- function(mean) m <<- mean
        setinverse <- function(inverse) m <<- inverse
        
        ##getmean <- function() m
        getinverse <- function() m
        
        list(set = set, get = get,
             ##setmean = setmean,
             ##getmean = getmean)
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function solves for the ivnerse of the aobve matrix object unless it is cached then it reutrns the chached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##m <- x$getmean()
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ##m <- mean(data, ...)
        m <- solve(data, ...)
        ##x$setmean(m)
        x$setinverse(m)
        m
        
}
