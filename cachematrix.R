## By these functions, you can compute the inverse of an nxn matrix
## and cache its solution. Therefore, you won't compute the inverse
## of the matrix repeatedly to save the computing time.


## makeCacheMatrix(an nxn matrix X)
## {
##      set(value): set X to be an new matrix
##      get():get the X matrix
##      setInverse(): Set the inverse of X to be cached.
##      getInverse(inverse matrix): get the cached Inverse of X      
## }
## Initialize the special matrix which can cache the solution of its
## inverse. 

makeCacheMatrix <- function(x = matrix())
{
    if (nrow(x)!=ncol(x)) 
    {
        message("The input matrix should be an nxn matrix!")
        return 
    }
    m <- NULL
    
    set <- function(y = matrix())
    {
        if (nrow(y)!=ncol(y)) 
        {
            message("The input matrix should be an nxn matrix!")
            invisible(return) 
        }
        x <<- y
        m <<- NULL
    }    
    get <- function() x    
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve (X, ...)
## Solve the inverse of X, and cached it into the memory.
## If the solution was cached, print an message and output the cached data. 

cacheSolve <- function(x, ...)
{
    m <- x$getInverse()
    if (!is.null(m))
    {
        message("Getting cached inverse of matrix.")
        return(m)
    }
    
    data <- x$get()
    if( det(data)== 0)
    {        
        message("The inverse doesn't exist in this matrix")
        invisible(return)
    }
    else 
    {
        m<-solve(data)    
        x$setInverse(m)
        return(m)
    }
}
