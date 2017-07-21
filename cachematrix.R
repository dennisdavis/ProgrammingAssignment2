## two functions used to computer and cache the inverse of a matrix
## ex usage:
##     cached_matrix<-makeCacheMatrix(matrix(rnorm(100),10,10))
##     cacheSolve(cached_matrix, mymatrix)


## ----------------------------------------
## function name : makeCacheMatrix
## description   : returns a list with a getter and setter for 
##                 a cached matrix
## ----------------------------------------

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    
    get <- function() x
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }

    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    
    list(get = get, set = set, setmatrix = setmatrix, getmatrix = getmatrix)
}

## ----------------------------------------
## function name : cacheSolve
## description   : return the inverse of a cached matrix,
##                 if not cached will compute the inverse and then cache it
## ----------------------------------------

cacheSolve <- function(x, ...)
{
    m <- x$getmatrix()
    
    if (is.null(m))
    {
        m <- solve(x$get(), ...)
        x$setmatrix(m)
    }
    else
    {
        print("matrix has already been cached! Retrieving now.")
    }
    m
}
