#' RStudio Addin UI zur Erstellung eines Analyseprojekts
#'
#' @export
create_analysis_template_addin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Projekt erstellen"),
    miniUI::miniContentPanel(
      shiny::textInput("project_name", "Projektname", value = "my_project"),
      shiny::textInput("path", "Zielverzeichnis", value = ""),
      shiny::actionButton("choose_dir", "📁 Verzeichnis wählen"),
      shiny::checkboxInput("use_git", "Git initialisieren?", value = TRUE),
      shiny::checkboxInput("use_gitlab", "GitLab-Projekt erstellen?", value = TRUE),
      shiny::textInput("gitlab_url", "GitLab URL", value = "https://git.bihealth.org"),
      shiny::checkboxInput("use_renv", "renv verwenden (reproducibility)?", value = TRUE)
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$choose_dir, {
      path <- rstudioapi::selectDirectory("Verzeichnis wählen")
      if (!is.null(path)) shiny::updateTextInput(session, "path", value = path)
    })

    observeEvent(input$done, {
      shiny::stopApp(list(
        project_name = input$project_name,
        path = input$path,
        use_git = input$use_git,
        use_gitlab = input$use_gitlab,
        gitlab_url = input$gitlab_url,
        use_renv = input$use_renv
      ))
    })

    observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  result <- shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Analyseprojekt erstellen", width = 600, height = 500))

  if (!is.null(result)) {
    create_analysis_template(
      path = result$path,
      project_name = result$project_name,
      git = result$use_git,
      gitlab = result$use_gitlab,
      renv = result$use_renv,
      gitlab_url = result$gitlab_url,
      open = TRUE
    )
  }
}
