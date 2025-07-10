#' Check for GitLab token and abort setup with instructions if missing
#'
#' @param gitlab_url Character. URL to the GitLab server. Default: "https://git.bihealth.org"
#' @return TRUE if token is present; otherwise aborts with message and returns NULL
#' @export
helper_check_and_abort <- function(gitlab_url = "https://git.bihealth.org") {
  token <- Sys.getenv("GITLAB_PAT")

  if (nzchar(token)) {
    message("✅ GitLab PAT wurde gefunden.")
    return(TRUE)
  }

  message("❌ Kein GitLab Token (GITLAB_PAT) gefunden.")
  message("🔐 Um ein GitLab-Projekt automatisch zu erstellen, benötigst du ein gültiges Personal Access Token (PAT).")
  message("👉 Vorgehensweise:\n")
  message("1. Gehe zu: ", gitlab_url, "/-/profile/personal_access_tokens")
  message("2. Erstelle ein Token mit den Rechten: api, write_repository, read_user")
  message('3. Öffne deine .Renviron Datei und füge folgende Zeile hinzu:')
  message('   GITLAB_PAT="dein_token_hier"')
  message("4. Speichere die Datei und starte RStudio neu.\n")

  open_env <- tolower(readline("📂 Möchtest du deine .Renviron-Datei jetzt öffnen? (j/n): "))
  if (open_env == "j") {
    usethis::edit_r_environ()
  }

  message("\n🚫 Der Setup-Vorgang wird jetzt abgebrochen.")
  message("📝 Nach dem Neustart von RStudio und korrekt gesetztem Token kannst du die Funktion erneut aufrufen:\n")
  message('   create_analysis_template("dein/pfad", "projektname")\n')

  return(invisible(NULL))
}



#' Check GitLab connection and whether a project already exists
#'
#' @param project_name Name of the GitLab project to check.
#' @param namespace GitLab namespace (optional). Default: current user.
#' @param verbose Print status messages. Default: TRUE
#' @param gitlab_url GitLab server URL. Default: "https://git.bihealth.org"
#'
#' @return list(connected, exists, namespace, user)
#' @export
helper_gitlab_status <- function(project_name,
                                 namespace = NULL,
                                 verbose = TRUE,
                                 gitlab_url = "https://git.bihealth.org") {
  if (!requireNamespace("gitlabr", quietly = TRUE)) {
    stop("Bitte installiere das Paket 'gitlabr'")
  }

  # Token prüfen – bricht bei Fehlen automatisch ab
  if (!helper_check_and_abort(gitlab_url = gitlab_url)) {
    stop("❌ Kein gültiger Token – Verbindung zu GitLab wird nicht aufgebaut.")
  }

  gitlabr::set_gitlab_connection(
    gitlab_url = gitlab_url,
    private_token = Sys.getenv("GITLAB_PAT")
  )

  current_user <- tryCatch(gitlabr::gl_get_user(), error = function(e) NULL)
  if (is.null(current_user)) {
    stop("❌ Verbindung zu GitLab fehlgeschlagen. Token evtl. ungültig?")
  }

  if (is.null(namespace)) {
    namespace <- current_user$username
  }

  projects <- gitlabr::gl_list_projects(user = namespace)

  exists <- any(projects$name == project_name)

  if (verbose) {
    message("✅ Angemeldet bei ", gitlab_url, " als ", current_user$username)
    if (exists) {
      message("⚠️ Projekt '", project_name, "' existiert bereits unter Namespace: ", namespace)
    } else {
      message("✅ Projektname ist verfügbar.")
    }
  }

  return(list(connected = TRUE, exists = exists, namespace = namespace, user = current_user$username))
}


#' Create GitLab project via API and return repository URL
#'
#' @param project_name Name of the new GitLab project
#' @param visibility "private", "internal", or "public"
#' @param gitlab_url URL to GitLab server
#'
#' @return Repository SSH URL if successful, otherwise NULL
#' @export
create_gitlab_project <- function(project_name,
                                  visibility = "private",
                                  gitlab_url = "https://git.bihealth.org") {
  if (!requireNamespace("gitlabr", quietly = TRUE)) {
    stop("Bitte installiere das Paket 'gitlabr'")
  }

  # Token prüfen – bricht ab bei Fehlen
  if (!helper_check_and_abort(gitlab_url = gitlab_url)) {
    stop("❌ Kein gültiger Token – GitLab-Projekt kann nicht erstellt werden.")
  }

  message("⏳ GitLab-Projekt wird erstellt...")

  gitlabr::set_gitlab_connection(
    gitlab_url = gitlab_url,
    private_token = Sys.getenv("GITLAB_PAT")
  )

  # Projekt erstellen
  project <- tryCatch({
    gitlabr::gl_new_project(name = project_name, visibility = visibility)
  }, error = function(e) {
    message("❌ Fehler beim Erstellen des Projekts: ", e$message)
    return(NULL)
  })

  if (!is.null(project)) {
    message("✅ GitLab-Projekt erfolgreich erstellt: ", project$ssh_url_to_repo)
    return(project$ssh_url_to_repo)
  } else {
    return(NULL)
  }
}

