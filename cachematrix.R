## Caching the Inverse of a matrix
## Programming Assignment 2

makeCacheMatrix <- function(x = matrix()) {
    
    ## This variable would be used to hold the inverse of the matrix. 
    inverseMatrix <- matrix()
    
    
    ## This function would create a new matrix based and assign it to the variable x
    createMatrix <- function(sampleMatrix){
        x <<- sampleMatrix
        #inverseMatrix <<- NULL
    }
    
    ## This function retrieves the matrix created earlier 
    getMatrix <- function(){
        x
    }
    
    ## This function computes and stores the inverse of the matrix
    createInverse <- function(){
        
            inverseMatrix <<- solve(x)
    }
    
    ## This function returns the inverse of the matrix
    getInverse <- function(){
        return(inverseMatrix)
    }
    
    
    ## The list of functions is returned
    list(createMatrix = createMatrix , getMatrix = getMatrix, 
         createInverse = createInverse , getInverse = getInverse)
    
    

}


## Function to check and return a cached version if exists, else compute it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- matrix()
    inv <- x$getInverse()
    
    ## Check if the dimensions of the matrix are not (1,1), then it is assumed to be non-empty
    if( !(dim(inv)[1]==1 & dim(inv)[2]==1)){
        ## The inverse has already been calculated. Return the cached copy
        print("Retrieving cached inverse matrix.")
        return(inv)
        
    }
    
    ## Cached copy not found, hence computing and returning the same
    print("Cached copy of inverse matrix not found, computing it now!")
    data <- x$getMatrix()
    
    ## Compute the inverse and return it
    inv <-solve(data)
    inv
    
}
