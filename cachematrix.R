## makeCacheMatrix take in a matrix and has a list of functions to set and get its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #init a variable for the inverse 
    set <- function(y) {
        x <- y
        inv <<- NULL
    }
    
    get <- function() x #return matrix
    setinverse <- function(inverse) inv <<- inverse # set parameter inv with the inverse
    getinverse <- function() inv #return inverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) { #if there was no inverse set, the original value would be NULL
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #get matrix
    inv <- solve(data, ...) #getting inverse
    x$setinverse(inv) #set inv
    inv
}

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation
