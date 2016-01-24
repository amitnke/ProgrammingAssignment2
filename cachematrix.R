# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. We are creating two function that can be used to create a matrix 
# that can cache it's inverse and second will be used to compute the inverse of matrix.

#The first function, `makeCacheMatrix` creates a special "matrix" object 
#that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
# setMatrix the value of the matrix
        setMatrix <- function(y){
                x <<- y
                inverse <<- NULL
        }
# getMatrix the value of the matrix
        getMatrix <- function() x
# setInverse the value of the inverse
        setInverse <- function(inv){
                inverse <<- inv
        }
# getInverse the value of the inverse
        getInverse <- function(x) inverse
        
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix, 
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        mat <- x$getMatrix()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
