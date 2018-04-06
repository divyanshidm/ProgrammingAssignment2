
## Assignment submission by Divyanshi Mangal

## makeCacheMatrix method is a list of four functions which are used to get the input matrix, get the cached result,
## set the result and initialize the variables also.

## cacheSolve method checks whether a result is already cached, if so it returns result directly,
## else, it calculates the result store it in cache and return the result.

#a function having multiple functions
makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL         #variable to insert cached inverse
          
          set <- function(y)       #function to initialize variables
          {
            inverse <<- NULL      
            x <<- y
          }
          
          #function to get the value of input matrix
          get <- function() x
          
          #function to cache the matrix
          setInverse <- function(newInverse) inverse<<- newInverse
          
          #function to get inverse from cache
          getInverse <- function() inverse
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        }




cacheSolve <- function(x, ...) {
        
        #get the cached result
        mat <- x$getInverse()
        
        #condtition to check whether cached result exist
        if(!is.null(mat))
        {
          message("getting cached inverse matrix")
          return(mat)
        }
        
        #calculate and store the inverse if it doesn't exist
        data<-x$get()
        newInverse <- solve(x)
        x$setInverse(newInverse)
        newInverse
  
        }
