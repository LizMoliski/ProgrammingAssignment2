## makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to do the following:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix
## Note that there is NO ERROR CHECKING. This function assumes that an invertible
## square, numeric matrix will be passed ot it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
   
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cachSolve calculates the inverse of the special "matrix" created with 
## makeCacheMatrix.However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise,it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the solve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } 
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


## This was handy during testing. It creates a 4x4 hilbert matrix, h4 and calculates
## its inverse, sh4, and then multiplies the two matrices, which should result in
## an identify matrix
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h4 <- hilbert(4); h4
## sh4 <- solve(h4)
## round(sh4 %*% h4, 3)
