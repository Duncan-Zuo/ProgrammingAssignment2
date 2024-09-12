## The following two functions aim to cache the inverse of a matrix. With the pair of functions, if the inverse of 
## the matrix has been calculated, then the function directly gets the result, which saves redundant computation.


## The function makeCacheMatrix creates a list containing a function to do the following things:
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmat <<- inverse
        getinverse <- function() invmat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix of the one created with the above function. 
## Firstly, it checks to see if the inverse of the matrix has already been calculated. If so, it gets the 
## computed matrix from the cache and skips the computation. Otherwise, it calculates the inverse of the 
## original matrix and sets the computed matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        invmat <- x$getinverse()
        if(!is.null(invmat)) {
                message("getting cached inverse of a matrix")
                return(invmat)
        }
        orimat <- x$get()
        invmat <- solve(orimat)
        x$setinverse(invmat)
        invmat
}
