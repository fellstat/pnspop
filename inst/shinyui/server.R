library(shiny)
library(promises)
library(future)
library(ipc)
library(shinyhelper)
plan(multisession)
library(ggplot2)
library(pnspop)
library(RDS)
options(shiny.maxRequestSize=300*1024^2)

shinyServer(function(input, output, session) {
  observe_helpers()
  single_var_names <- c("subject","recruiter","degree","subject_hash", "subject_coupon")
  multi_var_names <- c("nbrs","coupons")
  get_raw_data <- reactive({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    dat <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    vars <- names(dat)

    for(name in single_var_names)
      updateSelectizeInput(session, name, choices = c("Choose Variable"="",vars))
    for(name in multi_var_names)
      updatePickerInput(session, name, choices = vars)
    dat
  })

  get_numeric <- function(name){
    dat <- get_raw_data()
    if(is.null(dat) || input[[name]] == "")
      return(NULL)
    as.numeric(dat[[input[[name]]]])
  }

  get_categorical <- function(name){
    dat <- get_raw_data()
    if(is.null(dat) || input[[name]] == "")
      return(NULL)
    as.character(dat[[input[[name]]]])
  }

  get_logical <- function(name){
    dat <- get_raw_data()
    if(is.null(dat) || input[[name]] == "")
      return(NULL)
    variable <- dat[[input[[name]]]]
    if(is.logical(variable))
      return(variable)
    if(is.numeric(variable))
      return(variable == max(variable, na.rm=TRUE))
    variable <- as.factor(variable)
    variable == max(levels(variable))
  }

  render_raw_table <- function(name){
    renderTable({
      dat <- get_raw_data()
      if(is.null(dat))
        return(NULL)
      v <- dat[[input[[name]]]]
      table(v, useNA = "always", dnn=name)
    })
  }

  render_table <- function(name){
    renderTable({
      v <- eval(parse(text=paste0("get_", name)))()
      table(v, useNA = "always", dnn=name)
    })
  }

  get_nbrs <- reactive({
    dat <- get_raw_data()
    rw_names <- input$nbrs
    if(is.null(dat) || length(rw_names) == 0)
      return(NULL)
    rw <- lapply(as.list(rw_names), function(name){
      as.character(dat[[name]])
    })
    as.data.frame(rw)
  })

  get_recruiter <- reactive({
    rec <- NULL
    if(input$recruiter == ""){
      if(input$subject_coupon == ""){
        showNotification("Error: No subject coupon specified")
      }
      if(is.null(input$coupons)){
        showNotification("Error: No coupons specified")
      }
      tr <- try(
        rec <- as.character(rid.from.coupons(
          get_raw_data(),
          subject.coupon = input$subject_coupon,
          coupon.variables = input$coupons
        ))
      )
      if(inherits(tr,"try-error")){
        showNotification("Error parsing coupons. Please check them and try again.")
        showNotification(tr)
        rec <- NULL
      }
    }else(
      rec <- get_categorical(input$recruiter)
    )
  })


  output$table <- renderDataTable({
    if(is.null(get_raw_data()))
      return(NULL)

    head(get_raw_data())
  })

  output$degree_plot <- renderPlot({
    degree <- get_numeric(input$degree)
    if(is.null(degree))
      return(NULL)
    print(qplot(degree, bins = 30))
  })

  observeEvent(input$calc_rho, {
    subject_hash <- na.omit(get_categorical(input$subject_hash))
    ns <- length(subject_hash)
    n_matches <- sum(sapply(subject_hash,function(h) sum(subject_hash==h,na.rm=TRUE) - 1))
    rho <- n_matches / (ns*(ns-1))
    updateNumericInput(session, "rho", value=rho)
  })


  nclicks <- reactiveVal(0)
  point_result <- reactiveVal()
  output$point_results <- renderTable({
    get_recruiter()
    nclicks(0)
    subject <- get_categorical(input$subject)
    recruiter <- get_categorical(input$recruiter)
    subject_hash <- get_categorical(input$subject_hash)
    degree <- get_numeric(input$degree)
    nbrs <- get_nbrs()
    rho <- input$rho
    method <- tolower(input$method)
    small_sample_fraction <- input$small_sample_fraction
    res <- cross_tree_pse(subject,recruiter,
                   subject_hash, degree,
                   nbrs, method=method,
                   small_sample_fraction = small_sample_fraction,
                   rho = rho)
    point_result(res)
    res
  }, rownames=FALSE,
  width="400px", digits=4)

  interruptor <- AsyncInterruptor$new()

  boot_result <- reactiveVal()
  observeEvent(input$run,{
    if(nclicks() != 0){
      print("Already running")
      return(NULL)
    }
    nclicks(nclicks() + 1)
    boot_result(data.frame(Status="Running..."))
    if(is.null(point_result()))
      return(NULL)

    progress <- AsyncProgress$new(message="Generating Boostrap Samples")
    prog <- function(i){
      interruptor$execInterrupts()
      progress$set(i/nrep)
    }
    n_bootstrap <- nrep <- input$nrep
    subject <- get_categorical(input$subject)
    recruiter <- get_categorical(input$recruiter)
    subject_hash <- get_categorical(input$subject_hash)
    degree <- get_numeric(input$degree)
    nbrs <- get_nbrs()
    rho <- input$rho
    method <- tolower(input$method)
    small_sample_fraction <- input$small_sample_fraction

    result <- finally(
      catch(
        future({
          bootstrap_pse(subject,recruiter,
                         subject_hash, degree,
                         nbrs, method=method,
                         small_sample_fraction = small_sample_fraction,
                         rho = rho,
                         n_bootstrap = n_bootstrap,
                         progress = prog)
        }, seed=TRUE)  %...>% boot_result,
        function(e) {
          boot_result(NULL)
          print(e$message)
          showNotification(e$message)
        }
      ),
      function(){
        progress$sequentialClose()
        nclicks(0)
      }
    )

    NULL
  })
  output$bootstrap1 <- renderTable({
    req(boot_result())
  },digits=4)

  observeEvent(input$cancel,{
    print("cancel")
    interruptor$interrupt("User Interrupt")
  })
})
