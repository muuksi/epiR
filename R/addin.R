#' Interaktives Addin zur Erstellung eines Analyseprojekts
#'
#' @export
create_analysis_template_addin <- function() {
  path <- rstudioapi::selectDirectory("Wähle ein Verzeichnis für dein Projekt")
  if (is.null(path)) return(invisible(NULL))

  project_name <- readline("📁 Projektname: ")
  if (!nzchar(project_name)) {
    message("❌ Kein Projektname angegeben.")
    return(invisible(NULL))
  }

  create_analysis_template(path = path, project_name = project_name)
}
