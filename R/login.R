login <- function(host, username, password) {
    J("ucar.httpservices.HTTPFactory")$setCredentials(host, username, password)
}
