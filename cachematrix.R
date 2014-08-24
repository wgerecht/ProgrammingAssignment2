#The following 2 functions are used to cache a matrix (that you define) and then 
#create its inverse andand cache it for future use.


#"makeCacheMatrix"creates"a"list"containing"a"function"to
#"1."set"the"value"of"the"matrix
#"2."get"the"value"of"the"matrix
#"3."set"the"value"of"inverse"of"the"matrix
#"4."get"the"value"of"inverse"of"the"matrix

makeCacheMatrix<-function(x=matrix()){
        inv<-NULL  #Sets inv to Null to start with
        set<-function(y){
                x<<-y
                inv<<-NULL #Resets any stored inv to Null
        }
        #Sets and Gets for list to be built
        get<-function() x
        setInverse<-function(inverse) inv<<-inverse
        getInverse<-function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
#This following function returns the"inverse the matrix.  It will first check if
# the inverse has already been computed and cheched.   If not it calculates the inverse
# and then cahes it.  It hen returns it.


#Note this function assumes the matrix
#is always invertible
cacheSolve<-function(x,...){
        inv<-x$getInverse() #Gets stored inverse if present
        if(!is.null(inv)){ #Checks to see if anything stored
                message("getting cached data")
                return(inv) #Returns stored inverse if present
        }
        #If no stored inverse gets matrix and creaates and stores inverse
        data<-x$get()
        inv<-solve(data)
        x$setInverse(inv)
        return(inv)
        
}


