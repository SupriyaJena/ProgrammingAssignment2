## Author: Supriya Jena
## Short description of the assignment "Caching the Inverse of a Matrix"

## Following two functions are for the R Programming 
## Assignment 2 - "Caching the Inverse of a Matrix" of Data Science Specialization Track 
## offered on Coursera through the Johns Hopkins School for Public Health
##
## Function 1: makeCacheMatrix
##      This function creates a special "matrix" object that can cache its inverse.
##      Note: Matrix inversion is usually a costly computation, specifically for large size matrices. 
##      Therefore it would be benficial to compute and cache the inverse of matrix to avoid repeted compution.
##
## Function 2: cacheSolve
##      This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##      If the inverse has already been calculated (and the matrix has not changed),
##      then the cachesolve will retrieve the inverse from the cache.
##
## ********    *******  ******* makeCacheMatrix *******  *******  ******
## The makeCacheMatrix function is created using the same format as the 
## assignment example. It creates a special matrix object that will be 
## a list containing four (4) functions
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
        # Initially set to NULL. It will change when the user sets the value       
        invr <- NULL
        
        set <- function(y) 
        {
                # Below set function will set the matrix itself but not the inverse
                x <<- y
                invr <<- NULL
        }
        # Below get function will get the matrix itself but not the inverse
        get <- function() x
        
        # Manually set the inverse
        setinverse <- function(inverse) 
                invr <<- inverse
        
        # Get the inverse
        getinverse <- function() invr
        
        # List summary
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## ****  ********    *******  ******* cacheSolve *******  *******  ******  ****
## The cacheSolve function is created using the same format as the assignment 
## example. It computes the inverse and cache the result. If cacheSolve function 
## is used again on the same special matrix, then it will avoid the recomputation 
## and will return the pre-computed result. An informative message will be shown 
## in the command prompt when the pre-computed result is returned instead.
## 
cacheSolve <- function(x, ...) 
{
        ## Get the existing inverse and see if it has been computed yet
        invr <- x$getinverse()
        
        ## If it has...
        if(!is.null(invr)) 
        {
                # Return the precomputed inverse		
                message("Getting cached matrix")
                return(invr)
        }
        
        # If it hasn't...
        # Then get the matrix itself
        data <- x$get()
        
        # Compute the inverse
        invr <- solve(data, ...)
        
        # Cache the inverse result in the object
        x$setinverse(invr)
        
        # Return the computed result
        invr
}