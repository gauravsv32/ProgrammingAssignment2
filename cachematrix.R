## There are 2 functions in this file - makeCacheMatrix and cacheSolve
### The first function builds a list of a matrix and it's inverse
### It also caches the value of a freshly computed inverse
### the cached value is retained until the matrix is overwritten with a
### fresh value
### The second function returns the inverse of the matrix contained in the
### supplied list
### It only computes the inverse afresh if the supplied list doesn't contain
### a cached copy of the inverse already 


## builds and returns a list that contains the supplied matrix and it's inverse which is set to NULL

makeCacheMatrix <- function(mat = matrix()) {
        inversemat <- NULL
        set <- function(inmat) {
                mat <<- inmat
                inversemat <<- NULL
        }
        setinverse <- function(inversematin) {
                inversemat <<- inversematin
        }
        get <- function() {
                mat
        }
        getinverse <- function() {
                inversemat
        }
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## returns the inverse of the matrix contained in the supplied list. only computes the inverse afresh if a cached value is not contained in the supplied list.

cacheSolve <- function(matList, ...) {
        ## Return a matrix that is the inverse of 'matList$get()'
        inversemat <- matList$getinverse()
        if(!is.null(inversemat)){
                message("getting cached inverse matrix")
                return(inversemat)
        }
        mat <- matList$get()
        inversemat <- solve(mat)
        matList$setinverse(inversemat)
        inversemat
}
