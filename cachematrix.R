makeCacheMatrix <- function(m = matrix()) 
{
        inverse <- NULL # inverse starts uncomputed
        # set(matrix)
        ## sets the  matrix and invalidates the cached inverse
        set <- function(y) 
        {
                m <<- y
                inverse <<- NULL
        }
        
        ## returns the matrix
        get <- function()
        {
                m
        }
        
        ## sets the value of the inverse  matrix
        setinverse <- function(inv) 
        {
                inverse <<- inv
        }
        
        ## return the cached value of the inverse of the matrix.
        ## returns NULL if the inverse has not yet be set, or the underlying
        ##   matrix has changed since the last invocation
        getinverse <- function() 
        {
                inverse
        }
        
        ## return a list of getter and setter methods as a result of object creation
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}



cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        inv <- m$getinverse()
        if(!is.null(inv)) 
        {
                ## Inverse already calculated
                message("getting cached data")
                return(inv)
        }
        
        new_matrix <- m$get()
        inv <- solve(new_matrix)
        m$setinverse(inv)
        inv    
}


