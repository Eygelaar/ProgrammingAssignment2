# The combination of the following two function decreases the computational strain on a computer by returing the inverse of a matrix
# from the cache, if it has been calculated before, rather than to calculate the inverse of the matrix each time.

# The makeCacheMatrix effectively creates a list of 4 functions capable of:
# setting the value of the matrix used as input in the cache,
# getting the value of the matrix used as input in the cache,
# setting the value of the inverse of the matrix used as input in the cache,
# getting the value of the inverse of the matrix used as input in the cache.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                                                     # Create empty variable for the inverse matrix
        set <- function(y) {                                                            # Create the set function with y as input
                x <<- y                                                                 # Set input matrix equal to x variable
                inv <<- NULL                                                            # Create empty variable for the inverse matrix
        }
        get <- function() x                                                             # Create the get function
        setinverse <- function(inverse) inv <<- inverse                                 # Create the setinverse function
        getinverse <- function() inv                                                    # Create the getinverse function
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)            # Create a list containing the 4 functions
}

# The cacheSolve function takes a matrix (always assuming a invertable matrix) as input and returns the inverse of the matrix to 
# the users. The first process in this function is to check whether or not the inverse of the matrix have already been calculated
# and stored in the cache. If the inverse of the matrix is stored in the cache, the inverse of the matrix will be returned without any
# calculations required. If the inverse of the matrix is not stored in the cache, the function will calculate the inverse and 
# then store it in the cache using the setinverse function. After this is done the function will return the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                                           
        if(!is.null(inv)) {                                             # check if matrix is stored in cache
                message("Retrieving cached data.")                      # message if matrix is stored in cache
                return(inv)                                             # returns the inverse of the matrix from tha cache
        }
        data <- x$get()                                                 # sets the matrix equal to a variable
        inv <- solve(data)                                              # calculate the inverse of a matrix using the solve() function
        x$setinverse(inv)                                               # store the inverse of the matrix in the cache using setinverse() function
        inv                                                             # displays the inverse of the matrix that wass just calculated
}