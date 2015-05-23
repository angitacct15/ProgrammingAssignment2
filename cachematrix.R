## Put comments here that give an overall description of what your
## functions makeCacheMatrix return a list of objects. Even of the individual object is a function
## CacheSolve function does matrix inverse and stores it. If matrix inverse already exists then 
## returns it from the cache.

## Write a short comment describing this function
## set_default function sets the input matrix to input_matrix variable and inverse variable to null
## get_matrix function returns the matrix stored
## set_inverse function calculates the matrix inverse and stores into inverse variable
## get_inverse function returns the inverse of the matrix

## finally a list is returned which consists of 4 objects each of type function. To call them later
## on we assign each function a name. 

makeCacheMatrix <- function(input_matrix = matrix()) {
    
    inverse <- NULL
    
    set_default <- function(y) {
        input_matrix <<- y
        inverse <<- NULL
    }
    get_matrix <- function() {
        input_matrix
    }
    
    set_inverse <- function(solve) {
        inverse <<- solve
    }
    
    get_inverse <- function() {
        inverse
    }
    
    list(set = set_default, get = get_matrix,
         setinverse = set_inverse,
         getinverse = get_inverse)
    
}


## Write a short comment describing this function

## takes a matrix as input. Calculates an inverse and store it. 
## if the inverse already exists then return the one stored in cache
## else, return the calculated inverse matrix

cacheSolve <- function(input_matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- input_matrix$getinverse()
    if(is.null(inverse)) {
        data <- input_matrix$get()
        inverse <- solve(data, ...)
        input_matrix$setinverse(inverse)
        
    } else {
        message("getting cached data")
    }
    inverse
}