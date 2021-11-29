## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and CacheSolve enables us to cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix consists of set, get, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     #initializing inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}  #function to get a matrix
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function(){inv}     #function to obtain the inverse of the matrix
        list(set =set,
             get= get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This is used to get cached data

cacheSolve <- function(x, ...) #gets cache data
{
        inv <- x$getInverse()
        if(!is.null(inv)) {  #checking whether inverse is null
                message("getting cached data!")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...) ##Calculates inverse value
        x$setInverse(inv)
        inv              ## Return a matrix that is the inverse of 'x'
}

