




require(usethis)
require(gitcreds)
require(httr)

get_newgithubtoken <- function(){
  usethis::create_github_token()
}


set_token <- function(){

  gitcreds_set(url="https://github.com")

  gitcreds_get()
}


proxy <- function(url = "http://proxy.charite.de"){
  proxy_site <- url
  check_proxy <- suppressWarnings(try(open.connection(url(proxy_site),open="rt",timeout=1),silent=T)[1])
  suppressWarnings(try(close.connection(proxy_site),silent=T))
  #print(check_proxy)
  if (is.null(check_proxy)) {
    print ("Achtung ! Proxy gefunden. Ich setze die Env-Variable auf den Proxy ...")
    Sys.setenv(https_proxy="http://proxy.charite.de:8080")   # hier wird die https-Variable gesetzt !
    set_config(use_proxy(url="proxy.charite.de", port = 8080)   )        # httr config eintragen
  } else {
    print ("kein Proxy zu sehen ... Env-Variable wird verworfen")
    Sys.setenv(https_proxy="")
  }
}

#proxy()
#Sys.getenv("https_proxy")
