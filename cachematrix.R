## rabornd 2014/11/23 - init

## Encapsulate a matrix and its inverse, cacheing the inverse on
## first request to minimize inverse calculations

## makeCacheMatrix():
##   Produces an object that stores a matrix and
##   its inverse but does NOT calc the inverse.
makeCacheMatrix <- function(x = matrix())
{
    # init var to hold inverse
    i<-NULL

    # function to set matrix
    set<-function(y)
    {
        # x is stored in parent env.
        x<<-y

        # if we are calling this function then 
        # we are using new matrix so reset inverse
        # to null. i is stored in parent env.
        i<<-NULL
    }

    # get matrix
    get<-function()
    {
        x
    }
    
    # set matrix's inverse
    setinverse<-function(inv)
    {
        # i is stored in parent env.
        i<<-inv
    }

    # get matrix's inverse
    getinverse<-function()
    {
        i
    }
   
    # functions of returned object 
    list(
        set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse
    )
}


## cacheSolve():
##   Calculates and caches (if not already cached) and
##   returns the inverse of a matrix stored in a
##   makeCacheMatrix object.
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'

        # get inverse value
        i<-x$getinverse()

        # if no inverse value, calc and cache
        if(!is.null(i))
        {
            message("getting cached inverse")
            return(i)
        }
        mat<-x$get()
        i<-solve(mat)
        x$setinverse(i)

        # return inverse
        i
}
