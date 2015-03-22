## These functions allow to cache the inverse of a matrix
## in order to avoid having to compute it a few times.

## This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function(){x}
        setinverse<-function(i){inverse<<-i}
        getinverse<-function(){inverse}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}