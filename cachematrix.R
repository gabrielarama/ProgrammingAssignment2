## makeCacheMatrix defines the value of a given matrix and stores its inverse 
## in cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ 
                x <<- y ## setting and caching the matrix value as x
                inv <<- NULL
        }
        
        get <- function() {x} ## getting the value of x
        setInverse <- function(inverse) {inv <<- inverse} ## caches inv 
        getInverse <- function() {inv} ## getting the value of inv
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve returns the inverse of matrix 'x' assigned in the aforementioned
## function (makeCacheMatrix)

cacheSolve <- function(x, ...) {
        inv <- x$getInverse ## gets the inverse calculated previously
        if(!is.null(inv)){ ## if inv is NULL, then the inverse is cached!
            ("getting cached data")
                return(inv) ## returns cached data
        }
        ## if reaches here, inverse was not calculated previously
        mt <- x$get() ## gets matrix x defined previously
        inv <- solve(mt, ...) ## calculates matrix inverse
        x$setInverse(inv) ## sets inv as the now calculated inverse
        inv ## returns inv to the console
}
