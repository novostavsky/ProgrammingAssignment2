## coursera.org - rprog-010 - asmnt2

## These funcations are written to speed up calculations od
## inverted matrices. The first one is a special function
## containing spec "matrices", and the second one calculates
## inversions for these matrices. See details below.


##The function makeCacheMatrix contains a number of internal
##functions "getters" and "setters" for matrix itself
##and cached value of inverted one (if was calculated before) 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ##setter for matrix
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ##getter for matrix
    get <- function() x
    ##setter for inverted matrix
    setinverse <- function(inverse) inv <<- inverse
    ##getter for inverted matrix
    getinverse <- function() inv
    ##the resulting list
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##The cacheSolve funcation gets a matrix and 
##returns inverted one. If the inverted was
##calculated before, the function gets cached version
##to improve speed/performance.
cacheSolve <- function(x, ...) {
    ##try to get inverted matrix from cache
    inv <- x$getinverse()
    ##check if it's null (wasn't calculated)
    ##if sucess -> print the mesage and return cache
    if(!is.null(inv)){
        message("get chached inverse")
        return(inv)
    }
    ##if fail -> we need to calculate it 
    data <- x$get()
    inv <- solve(data)
    ##and update cache with calculated value
    x$setinverse(inv)
    ##return a matrix that is the inverse of 'x'
    inv
}
