server <- function(input, output,session) {
  
  data <- reactiveValues() # store data to be used for tables

  # ask for ona username and password

    observeEvent(input$load_data, {
    
    showModal(modalDialog(
      textInput('login', 'Please enter your ona username'),
      passwordInput('password', 'Please enter your ona password'),
      footer=tagList(
        actionButton('submit', 'Submit'),
        modalButton('cancel')
      )
    ))
  })

 
 observeEvent(input$submit, {
    
    data$check <- get_data(isolate(input$login), isolate(input$password))
    removeModal()
    
    # updates picker input choices (filters)
    updatePickerInput(session, "filter_partner", choices = sort(unique((data$check)$Member_id), na.last=TRUE),selected = unique((data$check)$Member_id))
    updatePickerInput(session, "filter_date", choices = sort(unique((data$check)$today), na.last=TRUE),selected = unique((data$check)$today))
    updatePickerInput(session, "filter_district", choices = sort(unique((data$check)$District_Name), na.last=TRUE),selected = unique((data$check)$District_Name))
    updatePickerInput(session, "filter_username", choices = sort(unique((data$check)$ONA_username), na.last=TRUE),selected = unique((data$check)$ONA_username))
    
    ## valid surveys
    
    data <- data$check%>%
      data.frame()
      
    valid <- data%>%
      filter(Interview_criteria != "0" &
               consent != "No" &
               short !="TRUE")
    
    ## deaths
    deaths <- valid %>%
      select(today,District_Name,Community_Name,ends_with("Residency_status"),Bens_id,X_uuid,X_id)%>%
      pivot_longer(.,cols= ends_with("Residency_status"),names_to = "number", values_to = "Residency_status")%>%
      filter(Residency_status == "Dead")%>%
      select(number,Residency_status)

    ## suspected cases
member.suspected.cases <- valid %>%
      select(today,District_Name,ends_with("Actual_Symptoms"))%>%
      mutate_if(is.integer,as.character)%>%
      pivot_longer(.,cols = ends_with("Actual_Symptoms"), names_to = "HH_member", values_to = "Symptoms")%>%
      select(-"HH_member")%>%
      drop_na(Symptoms)%>%
      mutate(Symptoms = gsub("Shortness of breath or difficulty breathing", 0.6, Symptoms),
        Symptoms = gsub( "Sore throat", 0, Symptoms),
             Symptoms = gsub( "Headache", 0, Symptoms),
             Symptoms = gsub( "Diarrhoea or stomache pain", 0, Symptoms),
              Symptoms = gsub("[()]", "", Symptoms),
             Symptoms = gsub("Body aches muscle or joint pain", "0", Symptoms),
             Symptoms = gsub("Fever", 0.7, Symptoms),
             Symptoms = gsub("Fatigue", 0.4, Symptoms),
             Symptoms = gsub("Cough", 0.7, Symptoms),
             Symptoms = gsub("Loss or change in taste or smell", 0.9, Symptoms),
             Symptoms = gsub( "Positive laboratory test for COVID-19", 2, Symptoms),
             Symptoms = gsub( "None of the above", 0, Symptoms))%>%
      concat.split(data = .,split.col = 3,sep = " ",drop = TRUE)%>%
      mutate(`Covid-19 diagnostic score` = select(., as.numeric(starts_with("Symptoms"))) %>%
               rowSums(na.rm = TRUE))

    suspected.cases <- full_join(member.suspected.cases%>%
                                   filter(`Covid-19 diagnostic score` >=2)%>%
                                   group_by(today,District_Name)%>%
                                   summarise(`Number of suspected cases` = n())
                                 ,member.suspected.cases%>%
                                   group_by(today,District_Name)%>%
                                   summarise(`Number with atleast one symptom` = n()))%>%
      mutate(`Number of suspected cases` = replace_na(`Number of suspected cases`,0))


    ## Add values to the info boxes

    output$valid.interviews <- renderValueBox({
      valueBox(paste0(nrow(valid)," (",round(nrow(valid)/sum(target$Target)*100),"%)"),"Valid interviews (target achieved)")
    })

    output$duration <- renderValueBox({
      valueBox(paste0(round(median(data$interviewDuration)), " minutes"),"Interview duration (median)")
    })

    output$consent <- renderValueBox({
      valueBox(ifelse(table(data$consent)[["Yes"]]==dim(data)[[1]],"0",
                      table(data$consent)[["No"]]),"Non-consented interview(s)")
    })


    output$matching <- renderValueBox({
      valueBox(ifelse(table(data$Interview_criteria)[["1"]]==dim(data)[[1]],"0",
                      table(data$Interview_criteria)[["0"]]),"Non-matching")
    })


    output$deaths <- renderValueBox({
      valueBox(ifelse(nrow(deaths)>=1,nrow(deaths),"0"),"Death(s)")
    })

    output$c19 <- renderValueBox({
      valueBox(ifelse(sum(suspected.cases$`Number of suspected cases`) >=1,
                      sum(suspected.cases$`Number of suspected cases`),"0"),
               "Suspected Covid-19 case(s)")
    })
    
    ## target vs achieved data joining
    
    target_achieved <- reactive(
     
       left_join(
        valid%>%
          group_by_at(dplyr::vars(input$target_actual))%>%
          summarise(Achieved = n()),
        target%>%
          group_by_at(dplyr::vars(input$target_actual))%>%
          summarise(Targetted = sum(Target)))%>%
        mutate(`% Achieved` = round(Achieved/Targetted*100))%>%
        arrange(desc(`% Achieved`))%>%
        mutate(`% Achieved` = paste0(`% Achieved`,"%"))
    )
    
    output$target <- renderDataTable({
     
    datatable(target_achieved())
 })
    ##download
    output$download_target <- 
      downloadHandler(
        filename = function() {
          paste('Target vs achieved-', Sys.Date(), '.xlsx')},
        content = function(file){
          writexl::write_xlsx(target_achieved(),file)
        }
      )
## summary tab
    
    output$title <- renderUI({
      
      tags$span(style="font-style:italic;font-color:navy;font-size:16px;","An interview is invalid if; it is not consented, non-matching or is a short survey (conducted in less than 12 minutes)")
    
      })
    
    summary <- reactive({

      left_join(
        data%>%
        filter(Member_id%in%input$filter_partner,
             ONA_username %in% input$filter_username,
             today %in% input$filter_date,
             District_Name%in% input$filter_district)%>%
        group_by_at(dplyr::vars(input$summary_by))%>%
        dplyr::summarise(`N (all interviews - including invalid)`=sum(!is.na(today), na.rm=T),
                         `Average duration (mean)` = round(mean(interviewDuration, na.rm=TRUE),1),
                         `Average duration (median)` = round(median(interviewDuration, na.rm=TRUE),1),
                         `Short surveys` = sum(short),
                         `Long surveys` = sum(long)),
        
        data%>%
          filter(Interview_criteria != "0" &
                   consent != "No" &
                   short !="TRUE")%>%
        filter(Member_id%in%input$filter_partner,
               ONA_username %in% input$filter_username,
               today %in% input$filter_date,
               District_Name%in% input$filter_district)%>%
        group_by_at(dplyr::vars(input$summary_by))%>%
        dplyr::summarise(`Valid interviews`=sum(!is.na(today), na.rm=T)))%>%
        select(1:nrow(input$summary_by%>%data.frame()),match("N (all interviews - including invalid)",names(.)),length(.),everything())
      
    })

    output$summary <- renderDataTable({

      datatable(summary())
    })
## download
    output$download_summary <-
      downloadHandler(
        filename = function() {
          paste('Summary-', Sys.Date(), '.xlsx')},
        content = function(file){
          writexl::write_xlsx(summary(),file)
        }
      )
    
    ## all data tab
    all <- reactive({
      
      valid%>%
        select(1:33,396:413)%>%
        filter(Member_id%in%input$filter_partner,
               ONA_username %in% input$filter_username,
               today %in% input$filter_date,
               District_Name%in% input$filter_district)
    })
    
    output$all.data <- renderDataTable({
      
      datatable(all())
    })
    
    ## download
    output$download_all <-
      downloadHandler(
        filename = function() {
          paste('All_data-', Sys.Date(), '.xlsx')},
        content = function(file){
          writexl::write_xlsx(all(),file)
        }
      )
  })

}