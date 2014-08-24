## Code to calculate the inverse of a matrix and cache the result
## based on an object oriented type of structure and use of the
## superassign operator to create data objects that can be accessed
## in the environments outside of the defining function.
##
## This function stores the value of the matrix and an indication
## as to whether the inverse has been previously calculated along
## with the previously calculated inverse result that can be passed
## to other functions.
##   This code establishes the methods that are applied to
##   the data (original matrix and the inverse. The 'set' and 'get' functions
##   are methods that act on the original matrix data. The 'setInverse'
##   and 'getInverse' functions are methods that recall the matrix
##   inverse and store a matrix inverse.
##
## NOTES:
##   1. 'set' method is not required but included to make a full function
## completment. 
##   2. The use of x in both top level functions definitions
## is confusing and would have been altered, however, they were given
## in the original problem construct, so left intact.
##   3. Per problem instructions input matrix is assumed to be square and
## invertable.

makeCacheMatrix <- function(x = matrix()) {

     A_inverted <- NULL     #init the varible holding the inverted matrix
     
     set <- function(new_matrix){     #method to load new matrix
          x <<- new_matrix
          A_inverted <<- NULL         #also init inverse varible
     }
     
     get <- function() x     #method to read the current matrix
     
     setInverse <- function(A_inverse) A_inverted <<-A_inverse    #method to save inverse
     
     getInverse <- function() A_inverted     #method to retreive inverse of matrix
     
     list(set=set,                           #list of methods to be passed out
          get=get,
          setInverse=setInverse,
          getInverse=getInverse)
     
}

## This function first checks to see if the inverse has been obtained.
## If the inverse is has been previously calculated for the matrix, then
## the inverse is recalled and returned with a message so indicating. If 
## the matrix is different then the inverse is not available for recall
## and will be calculated.

cacheSolve <- function(x, ...) {
     
        A_inverted <- x$getInverse()     #get the current value of inverse
        if(!is.null(A_inverted)){        # if not NULL, then msg and return value
             message("Inverted matrix recalled from cached data")
             return(A_inverted)
             
        }
        
        B_mat <- x$get()                 #No inverse value to get matrix
        A_inverted <- solve(B_mat)       #Solve for inverse
        x$setInverse(A_inverted)         #Save by using set method
        A_inverted                       #Return the inverse value
}
