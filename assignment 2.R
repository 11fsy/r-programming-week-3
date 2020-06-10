makeCacheMatrix <- function(x = matrix()){
        inv <- NULL  ##initialize inv as NULL/ then the value of the inverse will be assinged  
        set <- function(y) {  ##define the set function to assign new values
                x <<- y  ##assign the value of matrix in parent environment
                inv <<- NULL  ## if there is a new matrix, reset inv to NULL
        }
        get <- function()x  ##define the get function-returns value of the matrix argument
        setinverse <- function(inverse)inv <<- inverse  ## assigns value of inv in parent environment
        getinverse <- function()inv  ## get the value of inv where called
        list(set = set, gett= get, setinverse = setinverse, getinverse = getinverse) #a list containing a function to set/get the value of the matrix and get/set the value of the inverse matrix, so that it can refer to the functions with the $ operator
}       

cacheSolve <- function(x, ...){
        ##return a matrix that is the inverse of 'x'
        inv <- x$getinverse() ##get the inverse value from the function
        if(!is.null(inv)){  ##to see if the inverse has already been calculated.
                message("getting cached data")  ##if so, gets the inverse value from the cache and skips the computation
                return(inv)
        }
        data <- x$get() ##if not, get the matrix from the function
        inv <- solve(data, ...) ##get the inverse from the solve funtion
        x$setinverse(inv) ## get the inverse matrix
        inv
}