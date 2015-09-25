## This programs refes to the Assignement 2 of the R courses on coursera 
## it provide 2 functions
## - makeCacheMatrix (x=matrix()): stores 4 functions
##        * setmatrix: stores the original matrix
##        * getmatrix: returns the original matrix
##        * setmatrixinv: stores the inverted matrix
##        * getmatrixinv: returns the inverted matrix

## - cachesolve (store=list, matrix=matrix())
##  get the information from makeCacheMatrix() and compare it to "matrix"
## if the matrix stored in "store" is identical to "matrix" and store don't have an inverse matrix
## then the matrix is inverse and information are stored in "store"
## otherwise the inverse matrix stored is returned 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## stores the matrix, init the inverse
    setmatrix <- function(y = matrix()) 
        {
          x <<- y
          inv <<- NULL
    }  
    
    ## print the matrix
    getmatrix <- function() x             
      
    ## stores the inverse matrix
    setmatrixinv <- function(y = matrix()) {inv <<- y}  
    
    ## print the inverse matrix
    getmatrixinv <- function() inv             
      
    ## list
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setmatrixinv = setmatrixinv, 
         getmatrixinv = getmatrixinv
    )
}


## 
cacheSolve <- function(store,matrix) {

    ## get matrixes already stored
    a <- store$getmatrix()
    ainv <- store$getmatrixinv()
    
    ## if "matrix" is identical to what is stored and there is an inverted matrix then no inversion
    ## the stored inverted matrix is returned
    if (  identical(a, matrix) && !is.null(ainv) )
        {
          message("matrix already inverted")
          xinv <- ainv
    }
    ## otherwise the matrix is inversed and the two matrixes (original and inversed) are stored    
    else
        {
          message("inverting the matrix")
          xinv <- solve(matrix) 
      
          message("matrix and inverse cached")
          store$setmatrix(matrix)
          store$setmatrixinv(xinv)
      
        }  
        return(xinv)
}

## testmatrix <- matrix(data = c(1,2,3,4), nrow=2, ncol=2)
## teststore <- makeCacheMatrix(x=testmatrix)
## cacheSolve(teststore, testmatrix)
 