## Programming Assignment 2 - R Programming (Coursera rprog-031)
## Written by: ddraayer

# Sample usage:
# > m <- makeCacheMatrix(matrix(c(1,2,3, 0,2,4, 0,5,1), nrow=3))
# > m$get()
# > cacheSolve(m)
# > cacheSolve(m) %*% m$get()
# > m$set(t(m$get()))
# > cacheSolve(m)


# Return a list of getters and setters that compute and cache the inverse of
# matrix m (assumed to be an invertible matrix)

makeCacheMatrix <- function(m = matrix()) {
    inv.cached <- NULL
    get <- function() { m }
    set <- function(m.new) {
        m <<- m.new
        inv.cached <<- NULL
    }
    getinverse <- function() { inv.cached }
    setinverse <- function(inv.new) { inv.cached <<- inv.new }
    list(get = get,
         set = set,
         getinverse = getinverse,
         setinverse = setinverse
    )
}


# Compute and return the inverse of matrix m$get, where m is a list returned by
# makeCacheMatrix.  Also cache the inverse so it can be retrieved again without
# recomputing it.

cacheSolve <- function(m, ...) {
	inv <- m$getinverse()
	if (is.null(inv)) {
		message('Computing and caching inverse')
		inv <- solve(m$get(), ...)
		m$setinverse(inv)
	} else {
		message('Returning cached inverse')
	}
	inv
}
