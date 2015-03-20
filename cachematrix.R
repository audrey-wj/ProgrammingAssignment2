#########################################
##Make a special vecotr, which is a list containing 4 functions
##

makeMatrix <- function(x = matrix(, nrow = 0, ncol = 0)) 
        #set x as the matrix, the default value for x is an empty matrix
{
        #reset inv to be an empty matrix
        inv <- matrix(, nrow = 0, ncol = 0)
        
        #set, or reset, the matrix with matrix y
        #this new matrix, y, will be passed to x in the makeMatrix frame
        set <- function(y) {
                x <<- y
                #also reset the inv matrix to empty if x is being changed
                inv <<- matrix()
        }
        
        #return the value of the matrix x
        #if matrix is reset by set() function, return the reset value
        #otherwise return the matrix that was passed when makeMatrix() function was called
        get <- function() x
        
        #set inv to be the inverse matrix
        #if the setinv() function is not explicitly called, inv will remain an empty matrix
        setinv <- function(inverse) inv <<- inverse
        
        #return inv
        #if setinv() is never called, an empty matrix will be returned
        #otherwise, the matrix that's set by the setinv() function will be returned
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
}

#########################################
##Getting the inverse of the matrix in the makeMatrix() function
##If the inverse matrix was found by the getinv() function, return it
##If the inverse matrix was not found, calculate it and return it


cacheInverse <- function(x, ...) 
        #the argument x here needs to be the list of function,
        #like it is defined by the makeMatrix() function
{
        #pass the getinv() result in the makeMatrix() function to inv
        inv <- x$getinv()
        
        #if inv is not empty, i.e., either the row # or the col # of inv >0, return this inv
        #print the message "getting cached data" if inv is found in this step
        if(nrow(inv) > 0 | ncol(inv) > 0) {
                message("getting cached data")
                return(inv)
        }
        
        #if getinv() fuction returns an empty matrix, calculate inv
        
        #use the get() function in makeMatrix() function to pass the Matrix to mat
        mat <- x$get()
        #calculate the inverse matrix of mat, and pass it to inv
        inv <- solve(mat, ...)
        
        #pass this inverse matrix back to the makeVector() function using setinv() function
        #so when the cacheInverse() function is called next time, the cached inverse will be used
        x$setinv(inv)
        
        #return inv
        #if inv was found in the cache instead of being calculated in the cacheInverse() function
        #the message "getting cached data" will not show
        inv
}
