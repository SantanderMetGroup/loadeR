login <- function(username, password) {
    message("[",Sys.time(), "] Setting credentials...")
    J("ucar.httpservices.MyHTTPFactory")$setCredentials(username, password)
    message("[",Sys.time(), "] Credentials set for user ", username, ".")
    message("[",Sys.time(), "] Please, note that invalid credentials will generate unauthorized errors when accessing the data.")
}
