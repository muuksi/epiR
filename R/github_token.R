




require(usethis)
require(gitcreds)


get_newgithubtoken <- function(){
  usethis::create_github_token()
}


set_token <- function(){

  gitcreds_set(url="https://github.com")

  gitcreds_get()
}

