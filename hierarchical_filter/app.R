library(shiny)


DOUBLE_UPPER_LETTERS <- paste0(LETTERS, LETTERS)
DOUBLE_LOWER_LETTERS <- paste0(letters, letters)


make_reactive_trigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      invisible(rv$a)
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

is_selection_restricted <- function(current_selection, old_selection) {
  all_selected <- is.null(current_selection)
  if (all_selected) 
    return(FALSE)
  all_previously_selected <- is.null(old_selection)
  if (all_previously_selected) 
    return(TRUE)
  selection_reduced <- length(current_selection) < length(old_selection)
  return(selection_reduced)
}

get_selectize_choices <- function(df, space, col_name) {
  all_selected <- is.null(space)
  choices <- if (all_selected) {
    df[, col_name]
  } else {
    df[space, col_name]
  }
  return(unique(choices))
}

update_selection <- function(current_selection, old_selection) {
  new_selection <- intersect(current_selection, old_selection)
  if (length(new_selection) == 0) 
    return(NULL)
  return(new_selection)
}

get_space <- function(df, input_value, col_name, space) {
  if (is.null(input_value))
    return (space)
  
  if (is.null(space)) 
    return (which(df[, col_name] %in% input_value))
  
  return (intersect(which(df[, col_name] %in% input_value), space))
}


df_hierarchy <- data.frame(
  domain = rep(DOUBLE_UPPER_LETTERS[1:2], each = 8),
  department = rep(LETTERS[1:4], each = 4),
  mpg = rep(DOUBLE_LOWER_LETTERS[1:8], each = 2),
  sku = letters[1:16],
  stringsAsFactors = FALSE
)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("domain"),
      uiOutput("department"),
      uiOutput("mpg"),
      uiOutput("sku"),
      actionButton("action", "Randomly affect department selection",
                   class = "btn-primary"),
      p("This button illustrates that the system can be influenced
        programmatically without crumbling down :)")
    ),
    mainPanel(
      h5("Overview of the data that captures dropdown choices 
         (notice the hierarchical dependency)"),
      tableOutput("hierarchy")
    )
  )
)


server <- function(input, output, session) {
  
  department_input_update <- make_reactive_trigger()
  department_space_update <- make_reactive_trigger()
  mpg_input_update <- make_reactive_trigger()
  mpg_space_update <- make_reactive_trigger()
  sku_input_update <- make_reactive_trigger()
  sku_space_update <- make_reactive_trigger()
  
  
  rv <- reactiveValues(
    domain_choices = df_hierarchy$domain,
    domain_selection = NULL,
    department_space_old = NULL,
    department_space_new = NULL,
    department_choices = df_hierarchy$department,
    department_selection = NULL,
    mpg_space_old = NULL,
    mpg_space_new = NULL,
    mpg_choices = df_hierarchy$mpg,
    mpg_selection = NULL,
    sku_space_old = NULL,
    sku_space_new = NULL,
    sku_choices = df_hierarchy$sku,
    sku_selection = NULL
  )
  
  
  output$hierarchy <- renderTable(df_hierarchy)
  
  output$domain <- renderUI({
    selectizeInput("domain", 
                   label = "Domain", 
                   choices = rv$domain_choices, 
                   selected = rv$domain_selected, 
                   multiple = TRUE,
                   options = list(placeholder = "All domains", 
                                  plugins = list("remove_button")))
  })
  output$department <- renderUI({
    selectizeInput("department", 
                   label = "Department", 
                   choices = rv$department_choices, 
                   selected = rv$department_selection, 
                   multiple = TRUE,
                   options = list(placeholder = "All departments", 
                                  plugins = list("remove_button")))
  })
  output$mpg <- renderUI({
    selectizeInput("mpg", 
                   label = "MPG", 
                   choices = rv$mpg_choices, 
                   selected = rv$mpg_selection, 
                   multiple = TRUE,
                   options = list(placeholder = "All MPGs", 
                                  plugins = list("remove_button")))
  })
  output$sku <- renderUI({
    selectizeInput("sku", 
                   label = "SKU", 
                   choices = rv$sku_choices, 
                   selected = rv$sku_selection, 
                   multiple = TRUE,
                   options = list(placeholder = "All SKUs", 
                                  plugins = list("remove_button")))
  })
  
  
  observeEvent(input$domain, ignoreInit = TRUE, ignoreNULL = FALSE, {
    department_space_update$trigger()
  })
  
  observeEvent(department_space_update$depend(), ignoreInit = TRUE, {
    rv$department_space_new <- get_space(df_hierarchy, input$domain, "domain", NULL)
    department_input_update$trigger()
  })
  
  observeEvent(department_input_update$depend(), ignoreInit = TRUE, {
    rv$department_choices <- get_selectize_choices(df_hierarchy, 
                                                   rv$department_space_new,
                                                   "department")
    if (is_selection_restricted(rv$department_space_new, rv$department_space_old)) {
      rv$department_selection <- update_selection(input$department, rv$department_choices)
    } else {
      rv$department_selection <- input$department
    }
    rv$department_space_old <- rv$department_space_new 
    mpg_space_update$trigger()
  })
  
  observeEvent(input$department, ignoreInit = TRUE, ignoreNULL = FALSE, { 
    mpg_space_update$trigger()
  })
  
  observeEvent(mpg_space_update$depend(), ignoreInit = TRUE, {
    new_space <- get_space(df_hierarchy, input$department, "department", 
                           rv$department_space_new)
    rv$mpg_space_new <- new_space
    mpg_input_update$trigger()
  })
  
  observeEvent(mpg_input_update$depend(), ignoreInit = TRUE, {
    rv$mpg_choices <- get_selectize_choices(df_hierarchy, 
                                            rv$mpg_space_new,
                                            "mpg")
    if (is_selection_restricted(rv$mpg_space_new, rv$mpg_space_old)) {
      rv$mpg_selection <- update_selection(input$mpg, rv$mpg_choices)
    } else {
      rv$mpg_selection <- input$mpg
    }
    rv$mpg_space_old <- rv$mpg_space_new
    sku_space_update$trigger()
  })
  
  observeEvent(input$mpg, ignoreInit = TRUE, ignoreNULL = FALSE, { 
    sku_space_update$trigger()
  })
  
  observeEvent(sku_space_update$depend(), ignoreInit = TRUE, {
    new_space <- get_space(df_hierarchy, input$mpg, "mpg", 
                           rv$mpg_space_new)
    rv$sku_space_new <- new_space
    sku_input_update$trigger()
  })
  
  observeEvent(sku_input_update$depend(), ignoreInit = TRUE, {
    rv$sku_choices <- get_selectize_choices(df_hierarchy, 
                                            rv$sku_space_new,
                                            "sku")
    if (is_selection_restricted(rv$sku_space_new, rv$sku_space_old)) {
      rv$sku_selection <- update_selection(input$sku, rv$sku_choices)
    } else {
      rv$sku_selection <- input$sku
    }
    rv$sku_space_old <- rv$sku_space_new
  })
  
  observeEvent(input$action, {
    rv$department_selection <- sample(rv$department_choices, 1)
  })
}

shinyApp(ui, server)