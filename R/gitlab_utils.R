# R/gitlab_utils.R

#' Check GitLab token and open .Renviron if needed
#' @return TRUE if token exists, otherwise FALSE and shows instructions
check_gitlab_token <- function() {
  token <- Sys.getenv("GITLAB_PAT")
  if (nzchar(token)) {
    message("✅ GitLab PAT gefunden.")
    return(TRUE)
  } else {
    message("❌ Kein GitLab Token (GITLAB_PAT) gefunden.")
    message("🔐 Um Zugriff auf GitLab via API zu erlauben, bitte wie folgt vorgehen:")
    message("1. GitLab PAT erstellen unter: https://git.bihealth.org/-/profile/personal_access_tokens")
    message("2. Öffne deine .Renviron-Datei und füge folgende Zeile ein:\n")
    message('   GITLAB_PAT="dein_token_hier"\n')
    message("3. Danach RStudio neu starten.")

    open_file <- tolower(readline("Möchtest du die .Renviron-Datei jetzt öffnen? (j/n): "))
    if (open_file == "j") {
      usethis::edit_r_environ()
    }
    return(FALSE)
  }
}

#' Create a GitLab project using the GitLab API
#'
#' @param project_name Name of the project to create
#' @param visibility "private" (default), "internal", or "public"
#' @return Repository URL (ssh or https)
#' @export
create_gitlab_project <- function(project_name, visibility = "private") {
  if (!check_gitlab_token()) return(NULL)
  if (!requireNamespace("gitlabr", quietly = TRUE)) {
    stop("Bitte installiere das Paket 'gitlabr'")
  }

  message("⏳ Starte GitLab-Projekterstellung via API...")

  # GitLab URL anpassen (bihealth spezifisch)
  gitlab_url <- "https://git.bihealth.org"

  # PAT setzen
  gitlabr::set_gitlab_connection(
    gitlab_url = gitlab_url,
    private_token = Sys.getenv("GITLAB_PAT")
  )

  # Projekt anlegen
  project <- gitlabr::gl_new_project(name = project_name, visibility = visibility)

  message("✅ Projekt auf GitLab erstellt: ", project$ssh_url_to_repo)
  return(project$ssh_url_to_repo)
}
