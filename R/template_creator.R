
  #' Create a complete analysis project folder with optional Git and GitLab integration
  #' @param path Character. Path to base directory.
  #' @param project_name Character. Project folder name.
  #' @export
  create_analysis_template <- function(path, project_name = "analysis_project") {
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
        "## Einführung",
        "",
        "Hier beginnt die Analyse."
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
    use_git <- tolower(readline("🔧 Möchtest du Git verwenden? (j/n): "))
    if (use_git == "j") {
      usethis::create_project(full_path, open = FALSE, rstudio = TRUE)
      usethis::use_git()

      # 6. GitLab verwenden?
      use_gitlab <- tolower(readline("🌐 Projekt auf GitLab automatisch erstellen? (j/n): "))
      if (use_gitlab == "j") {
        if (!check_gitlab_token()) {
          message("❌ GitLab-Konfiguration fehlgeschlagen. Projekt wird lokal erstellt.")
        } else {
          url <- create_gitlab_project(project_name)
          if (!is.null(url)) {
            usethis::use_git_remote(name = "origin", url = url)
            system("git add .")
            system('git commit -m "Initial commit"')
            system("git push -u origin main")
            message("✅ GitLab-Projekt initialisiert und Code gepusht.")
          }
        }
      }
    }

    # 7. renv verwenden?
    use_renv <- tolower(readline("📦 Möchtest du renv initialisieren (Reproducibility)? (j/n): "))
    if (use_renv == "j") {
      renv::init(bare = TRUE, project = full_path)
    }

    # 8. here verwenden?
    here_dir <- file.path(full_path, "scripts")
    if (!dir.exists(here_dir)) dir.create(here_dir, recursive = TRUE)

    here_path <- file.path(here_dir, "init_here.R")
    writeLines(c(
      "# Load project root with `here`",
      "library(here)",
      'message("Projekt root: ", here::here())'
    ), here_path)

    message("🎉 Setup abgeschlossen: ", full_path)
  }
