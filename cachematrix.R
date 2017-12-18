## makeCacheMatrix take in a matrix and  
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        i <<- NULL
    }
    
    get <- function() x #return matrix
    setinverse <- function(inverse) i <<- inverse # set parameter i with the inverse
    getinverse <- function() i #return inverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation
