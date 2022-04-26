login <- function(host, username, password) {
    message("[",Sys.time(), "] Setting credentials...")
    J("ucar.httpservices.HTTPFactory")$setCredentials(host, username, password)
    message("[",Sys.time(), "] Credentials set for user ", username, "@",host ".")
    message("[",Sys.time(), "] Please, note that invalid credentials will generate unauthorized errors when accessing the data.")
}
