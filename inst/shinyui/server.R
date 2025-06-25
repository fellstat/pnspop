library(shiny)
library(promises)
library(future)
library(ipc)
library(shinyhelper)
plan(multisession)
library(ggplot2)
library(pnspop)
library(DT)
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
      x <- as.character(dat[[name]])
      x[x == ""] <- NA
      x
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
          subject.id = input$subject,
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
      rec <- get_categorical("recruiter")
    )
    rec
  })


  output$table <- DT::renderDT({
    if(is.null(get_raw_data()))
      return(NULL)

    head(get_raw_data())
  })

  output$degree_plot <- renderPlot({
    degree <- get_numeric("degree")
    if(is.null(degree))
      return(NULL)
    pl <- ggplot() + geom_histogram(aes(x=degree), bins=30)
    print(pl)
  })


  rho_value <- reactiveVal(0)
  rho_from_data <- reactiveVal(FALSE)
  observeEvent(input$calc_rho, {
    subject_hash <- na.omit(get_categorical("subject_hash"))
    ns <- length(subject_hash)
    n_matches <- sum(sapply(subject_hash,function(h) sum(subject_hash==h,na.rm=TRUE) - 1))
    rho <- n_matches / (ns*(ns-1))
    rho_from_data(TRUE)
    rho_value(rho)
    updateNumericInput(session, "rho", value=rho)
  })
  observeEvent(input$rho, {
    if(is.na(as.numeric(input$rho))){
      rho_from_data(FALSE)
      return()
    }
    if(rho_value() == input$rho || abs(rho_value()/input$rho - 1) < .000000001){
      return()
    }
    rho_from_data(FALSE)
  })

  output$descriptives_table <- renderTable({
    subject_hash <- get_categorical("subject_hash")
    subject_hash[subject_hash == ""] <- NA
    nbrs <- get_nbrs()
    overlap <- overlap_statistics(subject_hash, nbrs)
    res <- data.frame(
      `Total Contacts` = overlap$neighbors$total_nbrs,
      `Unique Contacts` = overlap$unique$unique_nbrs,
      `PNS Sample Size` = overlap$unique$sample_size,
      `Unique Contacts in Sample` = overlap$naive_crc_estimate$unique_nbrs_sample_overlap,
      `Unique Hashed Identifiers in Contacts and Sample` = overlap$unique$total_unique_ident,
      check.names = FALSE
    )
    res <- t(res)
    colnames(res) <- "Count"
    res <- as.data.frame(res)
    return(res)
  }, rownames = TRUE)

  output$seed_table <- renderTable({
    subject <- get_categorical("subject")
    recruiter <- get_recruiter()#get_categorical("recruiter")
    dat <- data.frame(subject,recruiter,degree=100)
    dat <- as.rds.data.frame(dat, "subject","recruiter","degree")
    res <- table(RDS::get.seed.id(dat))
    res <- data.frame(`Seed ID`= names(res), `Tree Size`=as.vector(res), check.names = FALSE)
    return(res)
  }, rownames = TRUE)

  nclicks <- reactiveVal(0)
  point_result <- reactiveVal()
  output$point_results <- renderTable({
    nclicks(0)
    subject <- get_categorical("subject")
    recruiter <- get_recruiter()#get_categorical("recruiter")
    subject_hash <- get_categorical("subject_hash")
    subject_hash[subject_hash == ""] <- NA
    degree <- get_numeric("degree")
    nbrs <- get_nbrs()
    rho <- input$rho
    if(is.na(as.numeric(rho))){
      point_result("Invalid Rho")
      return()
    }
    if(rho_from_data())
      rho <- NULL
    method <- tolower(input$method)
    small_sample_fraction <- input$small_sample_fraction
    #set.seed(1)

    if(input$method == "Within Tree (n2)"){
      res <- one_step_pse(subject,recruiter,
                              subject_hash, degree,
                              nbrs,
                              rho = rho)[1]
    }else{
      res <- cross_tree_pse(subject,recruiter,
                            subject_hash, degree,
                            nbrs, method=method,
                            small_sample_fraction = small_sample_fraction,
                            rho = rho)
    }

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
    subject <- get_categorical("subject")
    recruiter <- get_recruiter()#get_categorical("recruiter")
    subject_hash <- get_categorical("subject_hash")
    subject_hash[subject_hash == ""] <- NA
    degree <- get_numeric("degree")
    nbrs <- get_nbrs()
    rho <- input$rho
    if(rho_from_data()){
      rho <- NULL
    }
    method <- tolower(input$method)
    small_sample_fraction <- input$small_sample_fraction
    result <- finally(
      catch(
        future({
          #set.seed(1)
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

  observeEvent(input$method,{
    boot_result(NULL)
  })

  output$bootstrap1 <- renderTable({
    req(boot_result())
  },digits=4)

  observeEvent(input$cancel,{
    print("cancel")
    interruptor$interrupt("User Interrupt")
  })
})
