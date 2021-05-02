## Functions for caching the matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initializing the inverse property
    i <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get and return the matrix
    get <- function() {
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Inversing the matrix and return inverse property
    getInverse <- function() {
        i
    }

    ## Returning list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated, then the cachesolve func should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Returning matrix thats inverse of x
    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from object
    data <- x$get()

    ## Calculate the inverse and set it
    m <- solve(data) %*% data
    x$setInverse(m)

    ## Return the matrix
    m
}
