## R Programming Week 3 Assignment
## Pair of Functions that cache the inverse of a matrix.

## makeCacheMatrix: function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inverse<-NULL
    set<-function(y) {
        x<<-y
        x_inverse<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) x_inverse<<-inverse
    getinverse<-function() x_inverse
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## cacheSolve: function computes the inverse of the special
## "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse<-x$getinverse()
    if(!is.null(x_inverse)){
        message("getting cached inverse data")
        return(x_inverse)
    } else {
        x_inverse<-solve(x$get())
        x$setinverse(x_inverse)
        return(x_inverse)
    }
}

