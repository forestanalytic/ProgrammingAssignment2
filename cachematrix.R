##
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
##
## cacheSolve computes the inverse of the special "matrix", first checking to see
## if an inverse has already been computed and cached.  If the inverse has been cached,
## it retrieves the inverse from the cache.  If the inverse has not been cached, it 
## calculates the inverse.  The inverse is returned.
##
##  Example use:
##
##  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##  h8 <- hilbert(8)
##  m <- makeCacheMatrix(h8)
##  cacheSolve(m) # this will calculate and cache the inverse
##  cacheSolve(m) # this will retrieve the inverse from the cache
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinv <- function(solve) m <<- solve
    getinv <- function()m
    list (set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)){
        message ("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) 
    x$setinv(m)
    m
}
