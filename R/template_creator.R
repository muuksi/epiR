#' Create a project template with folders and Quarto file
#'
#' @param path Character. Path where the analysis project will be created.
#' @param project_name Character. Name for the project folder and Rproj file.
#' @export
create_analysis_template <- function(path, project_name = "analysis_project") {
  if (!requireNamespace("usethis", quietly = TRUE)) stop("Please install the 'usethis' package.")
  if (!requireNamespace("renv", quietly = TRUE)) stop("Please install the 'renv' package.")

  full_path <- file.path(path, project_name)

  # Erstelle Ordnerstruktur
  dir.create(full_path, showWarnings = FALSE)
  #dir.create(file.path(full_path, "data"), showWarnings = FALSE)
  #dir.create(file.path(full_path, "output"), showWarnings = FALSE)
  #dir.create(file.path(full_path, "scripts"), showWarnings = FALSE)
  # setup folder structure
  dir.create(file.path(full_path, "analysis"), recursive = TRUE)
  dir.create(file.path(full_path, "data/data_clean"), recursive = TRUE)
  dir.create(file.path(full_path, "data/data_raw"), recursive = TRUE)
  dir.create(file.path(full_path, "dissemination/manuscripts"), recursive = TRUE)
  #dir.create(file.path(full_path, "dissemination/posters"), recursive = TRUE)
  #dir.create(file.path(full_path, "dissemination/presentations"), recursive = TRUE)
  dir.create(file.path(full_path, "documentation"), recursive = TRUE)
  dir.create(file.path(full_path, "misc"), recursive = TRUE)

  # Quarto Datei erstellen
  qmd_path <- file.path(full_path, "analysis.qmd")
  if (!file.exists(qmd_path)) {
    writeLines(c(
      "---",
      paste0("title: '", project_name, "'"),
      "format: html",
      "editor: visual",
      "author: ",
      paste0("created: " ,lubridate::today()),
      "---",
      "",
      "## Einführung",
      "",
      "Hier beginnt die Analyse.",
      ""
    ), qmd_path)
  }

  # Rproj Datei erstellen
  rproj_path <- file.path(full_path, paste0(project_name, ".Rproj"))
  if (!file.exists(rproj_path)) {
    writeLines(c(
      "Version: 1.0",
      "",
      "RestoreWorkspace: No",
      "SaveWorkspace: No",
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




## Interaktive Git-Aktivierung
use_git <- tolower(readline("Möchtest du Git initialisieren? (j/n): "))

# Git initialisieren
if (use_git == "j") {
  usethis::create_project(full_path, open = FALSE, rstudio = TRUE)
  usethis::use_git()

  use_gitlab <- tolower(readline("Projekt auf GitLab automatisch erstellen? (j/n): "))
  if (use_gitlab == "j") {
    url <- create_gitlab_project(project_name)
    if (!is.null(url)) {
      usethis::use_git_remote(name = "origin", url = url)
      system("git add .")
      system('git commit -m "Initial commit"')
      system("git push -u origin main")
      message("✅ Code wurde initial auf GitLab gepusht.")
    }
  }
}

## Renv aktivieren?
use_renv <- tolower(readline("Möchtest du renv initialisieren (Reproducibility)? (j/n): "))
if (use_renv == "j") {
  renv::init(bare = TRUE, project = full_path)
}

## here::here() vorbereiten
here_path <- file.path(full_path, "scripts", "init_here.R")
writeLines(c(
  "# Load project root with `here`",
  "library(here)",
  'message("Projekt root: ", here::here())'
), here_path)

message("Projekt erstellt unter: ", full_path)
rstudioapi::openProject(rproj_path)
}
