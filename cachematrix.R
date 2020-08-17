## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is the function creating a special "matrix" object that can caches its inverse.

makeCacheMatrix <- function(x = matrix()) {            ## set up the makeCacheMatrix function
        inv <- NULL
        set <- function (y) {
        x <<- y
                inv <<- NULL
                }
        get <- function() x                        ## command to take the special matrix 
        setinverse <- function(inverse)            ## command to set the function to take the inverse of the matrix.
                {inv <<- inverse}
        getinverse <- function() {inv}             ## command to get the inverse of the special matrix. 
        list(set= set, get=get,
             setinverse =setinverse,              ## list of the objectives in the makeCacheMatrix function.
             getinverse = getinverse)
        }

               
## Write a short comment describing this function
### cacheSolve is the function computing the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## return a matrix that is the inverse of 'x'
        inv <- x$getinverse()                 ## take the inverse matrix from the maatrix returned my makeCacheMatrix.
        if(!is.null(inv)) {                        ## if the inverse already calculated (the matrix has not changed) 
                message("getting cached data")      ## it retrieve the inverse from the cache and return the inverse matrix.
                return(inv)
                }
       mat <- x$get()                                  ## or it can get the special matrix in makeCacheMatrix above
        inv <- solve(mat,...)                       ## and solve this matrix to get the inverse matrix and return the result.
        x$setinverse(inv)
        inv
}


### Example from these functions
matrix_1 <- makeCacheMatrix(matrix(c(4,6,8,10), 2,2))
matrix_1$get()
     [,1] [,2]
[1,]    4    8
[2,]    6   10
matrix_1$getinverse() ## before using the cacheSolve function, the inverse matrix has not solved
NULL
> cacheSolve(matrix_1)
      [,1] [,2]
[1,] -1.25  1.0
[2,]  0.75 -0.5
matrix_1$getinverse() 
      [,1] [,2]
[1,] -1.25  1.0
[2,]  0.75 -0.5



