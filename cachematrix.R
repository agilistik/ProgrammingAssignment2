## Create a special matrix, 
## which is an object (list) containing functions to 
## set and get the matrix data, 
## and set and get the inverse of that matrix.

## makeCacheMatrix creates a special matrix object 
## Returns a list of functions to set and get the matrix,
## and set and get that matrix' inverse

makeCacheMatrix <- function(x = matrix()) {
    #Declaring inverse of the matrix:
    i <- NULL

    #Setting 'x' in the parent scope to 'm',
    #and setting inverse to NULL
    set <- function (m) {
        x <<- m
        i <<- NULL
    }
    get <- function () x

    #Setting 'i' in the partent scope to the value passed by 'setinverse' function
    setinverse <- function(inverse) i <<- inverse

    #Returning the inverse
    getinverse <- function () i

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the matrix' inverse has been cached, return it from the cache.
## Otherwise, calcluate the inverse, save it to the cache, and return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse
    if (!is.null(i) {
        message ("loading cached inverse of the matrix")
        return (i)
    }
    m <- x$get()
    i <- solve(m)
    x$setinverse(i)
    i
}
