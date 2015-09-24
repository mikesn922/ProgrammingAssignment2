## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#cerate a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    #initial inverseValue as NULL
    inverseValue <- NULL
    
    #set the value of the matrix
    set<- function(y){
        #check whether two matrix are identical
        #if not identical, update new matrix
        if(!identical(x, y)){
        x <<- y
        inverseValue <<- NULL
        }
    }
    
    #get the matrix
    getMatrix <- function() x
    
    #set inverse
    setInverse <- function(newInverse) inverseValue <<- newInverse
    
    #get the cached inverse value
    getInverse <- function() inverseValue
    
    #return a list of all the functions
    list(set = set, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    #firstly, get the inverse value
    inverseValue <- x$getInverse()
    
    #then check whether the inverse value is NULL or whether the matrix has been changed
    if(!is.null(inverseValue)){
        #output cached value
        message("getting cached data")
        return(inverseValue)
    }
    
    #if the inverse is not cached or it is a new matrix
    #get the new matrix
    data <- x$getMatrix()
    #calculate inverse
    inverseValue <- solve(data)
    #save the value
    x$setInverse(inverseValue)
    
    #return the calculated inverse
    inverseValue
}
