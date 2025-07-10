
#' Create a full analysis template
#'
#' @param path Project path
#' @param project_name Name of the project
#' @param gitlab_url GitLab URL (default: git.bihealth.org)
#' @export
create_analysis_template <- function(path, project_name = "analysis_project",
                                     git = TRUE, gitlab = TRUE,
                                     gitlab_url = "https://git.bihealth.org",
                                     open = TRUE) {

    full_path <- file.path(path, project_name)

    # 1. Ordner bereits vorhanden?
    if (dir.exists(full_path)) {
      message("⚠️ Der Ordner ", full_path, " existiert bereits.")
      overwrite <- tolower(readline("Möchtest du den bestehenden Ordner wirklich verwenden? (j/n): "))
      if (overwrite != "j") {
        message("❌ Abgebrochen.")
        return(invisible(NULL))
      }
    }

     if (!helper_check_and_abort()) return(invisible(NULL))

    # 2. Struktur erstellen
    # setup folder structure
    dir.create(file.path(full_path, "analysis"), recursive = TRUE)
    dir.create(file.path(full_path, "data/data_clean"), recursive = TRUE)
    dir.create(file.path(full_path, "data/data_raw"), recursive = TRUE)
    dir.create(file.path(full_path, "dissemination/manuscripts"), recursive = TRUE)
    #dir.create(file.path(full_path, "dissemination/posters"), recursive = TRUE)
    #dir.create(file.path(full_path, "dissemination/presentations"), recursive = TRUE)
    dir.create(file.path(full_path, "documentation"), recursive = TRUE)
    dir.create(file.path(full_path, "misc"), recursive = TRUE)

    # 3. Quarto Datei
    qmd_path <- file.path(full_path, "analysis.qmd")
    if (!file.exists(qmd_path)) {
      writeLines(c(
        "---",
        paste0("title: '", project_name, "'"),
        "format: html",
        "editor: visual",
        "---",
        "",
        "# Einführung",
        "",
        "Dies ist ein automatisch erstelltes Analyseprojekt.",
        "",
        "## Vorbereitung",
        "",
        "Bitte öffne zuerst das Projekt (`.Rproj`-Datei) in RStudio.",
        "",
        "Initialisiere dann manuell die Projektumgebung:",
        "",
        "```r",
        "renv::init()",
        "```",
        "",
        "Optional kannst du das Paket `here` nutzen, um Dateipfade projektbezogen zu verwalten.",
        "",
        "## Datenanalyse",
        "",
        "Hier beginnt deine Analyse."
      ), qmd_path)
    }


    # 4. Rproj Datei
    rproj_path <- file.path(full_path, paste0(project_name, ".Rproj"))
    if (!file.exists(rproj_path)) {
      writeLines(c(
        "Version: 1.0",
        "",
        "RestoreWorkspace: Default",
        "SaveWorkspace: Default",
        "AlwaysSaveHistory: Default",
        "",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8",
        "",
        "RnwWeave: knitr",
        "LaTeX: pdfLaTeX"
      ), rproj_path)
    }

    # 5. Git verwenden?
    #use_git <- tolower(readline("🔧 Möchtest du Git verwenden? (j/n): "))
    if (git) {
      usethis::create_project(full_path, open = FALSE, rstudio = TRUE)
      usethis::use_git()

    # 6. GitLab verwenden?
    #use_gitlab <- tolower(readline("🌐 Projekt auf GitLab automatisch erstellen? (j/n): "))
    if (gitlab) {

      # Verbindung und Projektstatus prüfen (nutzt helper_check_and_abort automatisch)
      status <- helper_gitlab_status(project_name, gitlab_url = gitlab_url)

      if (status$exists) {
        message("⚠️ GitLab-Projekt existiert bereits. Abbruch empfohlen, um versehentliches Überschreiben zu vermeiden.")
        proceed <- tolower(readline("Trotzdem fortfahren (existierendes Repo überschreiben)? (j/n): "))
        if (proceed != "j") {
          message("🛑 Setup wird abgebrochen.")
          return(invisible(NULL))
        }
      }

      url <- create_gitlab_project(project_name, visibility = "private") # ggf. gitlab_url als Argument nachrüsten
      if (!is.null(url)) {
        usethis::use_git_remote(name = "origin", url = url)
        system("git add .")
        system('git commit -m "Initial commit"')
        system("git push -u origin main")
        message("✅ GitLab-Projekt initialisiert und Code gepusht.")
      }
    }


    # 7. Projekt öffnen?
    #open_proj <- tolower(readline("🔓 Möchtest du das Projekt jetzt in RStudio öffnen? (j/n): "))
    if (open) {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::openProject(rproj_path, newSession = TRUE)
      } else {
        message("📦 Das Paket 'rstudioapi' ist nicht installiert – bitte manuell öffnen.")
      }
    }


    message("🎉 Setup abgeschlossen: ", full_path)
    }
}
