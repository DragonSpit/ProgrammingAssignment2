## 
##

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( x = matrix()) {
		matrixInverse <- NULL
        set <- function( y ) {
				## x is the original matrix passed in (i.e. not inverted)
                x <<- y
				matrixInverse <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function( mi ) matrixInverse <<- mi
        getMatrixInverse <- function() matrixInverse
        list( set = set, get = get,
              setMatrixInverse = setMatrixInverse,
              getMatrixInverse = getMatrixInverse )
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then retrieves the inverse from the cache.

cacheSolve <- function( x, ... ) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getMatrixInverse()
        if( !is.null( matrixInverse )) {
                message("getting cached matrix inverse")
                return( matrixInverse )
        }
		# matrixInverse has not been computed yet
        matrixOriginal <- x$get()
        matrixInverse <- solve( matrixOriginal, ... )
        x$setMatrixInverse( matrixInverse )
        matrixInverse
}
