#########################################
##Make a special vecotr, which is a list containing 4 functions
##

makeMatrix <- function(x = matrix(, nrow = 0, ncol = 0)) 
#set x as the matrix, the default value for x is an empty matrix
{
        
        inv <- matrix(, nrow = 0, ncol = 0)     #reset inv to be an empty matrix.
        
        set <- function(y)                      #set, or reset, the matrix with matrix y.
                                                #this new matrix, y, will be passed to x 
                                                #in the makeMatrix() frame.
        {
                x <<- y
                inv <<- matrix()                #reset the inv matrix to empty if x is changed.
        }
        
        get <- function()  x                    #return the value of matrix x.
                                                #if matrix is reset by set() function, 
                                                #return the reset value.
                                                #otherwise return the matrix that was passed 
                                                #when makeMatrix() function was called.
        
        setinv <- function(inverse)             #set inv to be the inverse matrix.
                                                #if the setinv() function is never explicitly 
                                                #called, inv will remain an empty matrix.
        {
                inv <<- inverse
        }
        
        getinv <- function() inv                #return inv.
                                                #if setinv() is never called, an emptry matrix
                                                #will be returned.
                                                #otherwise, the matrix that's set by the setinv()
                                                #function will be returned.
        
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
        
        inv <- x$getinv()                       #pass the getinv() result to inv
        
        if(nrow(inv) > 0 | ncol(inv) > 0)       #if inv is not empaty, i.e., either the row# or
                                                #the col# >0, return this inv
        {
                message("getting cached data")  
                return(inv)
        }
        
        #if getinv() fuction returns an empty matrix, calculate inv
        
        mat <- x$get()                          #use the get() function in makeMatrix() function 
                                                #to pass the Matrix to mat
        
        inv <- solve(mat, ...)                  #calculate the inverse matrix of mat, 
                                                #and pass it to inv
        
        x$setinv(inv)                           #pass this inverse matrix back to the makeVector()
                                                #fuction using setinv() function, so when the 
                                                #cashInverse() function is called next time, the
                                                #the cached inverse can be returned.
        
        inv                                     #return the calculated inv
}
