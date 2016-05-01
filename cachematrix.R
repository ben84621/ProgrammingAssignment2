## 
## makeCacheMatrix receives a matrix and returns a list of functions to be called to
## set the matrix, get the matrix itself, set the inverse matrix, and get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {

    inv = NULL    # first, clear the inverse variable
    
    # the set function receives a variable (y), sets the 'x' matrix to y, and clears the inverse variable
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # the get function simply returns the x matrix
    get <- function() x
    
    # the setinverse function receives an inverse and sets the inv variable to the new inverse
    setinverse = function(inverse) inv <<- inverse 
    
    # getinverse function simply returns the existing inverse matrix
    getinverse = function() inv
    
    #return set for this function
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## 
## cacheSolve receives a matrix (x) and checks whether the inverse has already been calculated.
## if yes, it returns the inverse matrix from cache. if no, it calculates the inverse, stores it, and returns it.
##

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()  # pulls the inverse function from cache
  
    # if it's a new matrix (inverse not cached), calculate the inverse
    # and set the cached inverse to the new value
    if (is.null(inv)){
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
    }
    
    # return the inverse
    inv
}
