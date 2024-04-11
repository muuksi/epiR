




require(usethis)
require(gitcreds)


get_newgithubtoken <- function(){
  usethis::create_github_token()
}


set_token <- function(){

  gitcreds_set(url="https://github.com")

  gitcreds_get()
}


proxy <- function(url = "http://proxy.charite.de:8080"){
  proxy_site <- url
  check_proxy <- suppressWarnings(try(open.connection(url(proxy_site),open="rt",timeout=1),silent=T)[1])
  suppressWarnings(try(close.connection(proxy_site),silent=T))
  #print(check_proxy)
  if (is.null(check_proxy)) {
    print ("Achtung ! Proxy gefunden. Ich setze die Env-Variable auf den Proxy ...")
    Sys.setenv(https_proxy=proxy_site)   # hier wird die https-Variable gesetzt !
  } else {
    print ("kein Proxy zu sehen ... Env-Variable wird verworfen")
    Sys.setenv(https_proxy="")
  }
}


#Sys.getenv("https_proxy")
