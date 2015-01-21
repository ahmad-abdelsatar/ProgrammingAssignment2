## These Functions work together to Instantite a special matrix object  that can 
## store its Inverse as an attribute at the first call to the cacheSolve   
## function the Inverse is computed and then stored for later retrival

## this function takes a matrix and return a list of matrix functions (methods)
## to get and set the Matrix or its Inverse

makeCacheMatrix <- function(x = matrix()) {
        
        matInv <- NULL
        
        set <- function(y){
                
                x <<- y
                matInv <<- NULL    ##Sets the inverse to Null when x is Changed
        }
        
        get <- function() x
        
        setInverse <- function(inverse = matrix()) matInv <<- inverse
        
        getInverse <- function() matInv
        
        list(set = set, get = get , 
             setInverse = setInverse, 
             getInverse = getInverse )
        

}


## This Function returns the Inverseof  special matrix created with 
## makeCacheMatrix it first computes the Inverse then stores it as an attribute
## of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matInv <- x$getInverse()
        
        if(!is.null(matInv)){
                
                return(matInv)
        }
        
        mat <- x$get()
        matInv <- solve(mat,...)
        x$setInverse(matInv)
        matInv
        
        
        
}
