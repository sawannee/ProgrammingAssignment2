## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix tries to do the following: 
# 1. set the value of the matrix, 
# 2. get the value of the matrix,
# 3. set the value of the inverse matrix,
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve tries to get the calculated inverse matrix from the cache 
# but if the inverse matrix has never been calculated, the function cacheSolve will caculate it. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat,...)
    x$setInverse(i)
    i
}
