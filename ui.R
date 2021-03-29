dashboardPage(skin="blue",
              dashboardHeader(title = "SN R5 data check dashboard",titleWidth = "900px",
                              tags$li(div(href = 'http://www.company.com',
                                          tags$img(src = 'logo.jpg',
                                                   title = "Company Home", height = "30px"),
                                          style = "padding-top:10px; padding-bottom:10px;padding-right:10px;"),
                                      class = "dropdown")),
              dashboardSidebar(disable = TRUE),
              dashboardBody(
             
                fluidRow(
                  ## custom-made sidebar
                  column(4,style = "background-color:#C1CDCD;",
                         br(),
                         actionButton("load_data", "Load data"),
                         pickerInput("target_actual", "Target vs achieved (first tab)", c("Member organisation" ="Member_id" , "Community" = "Community_Name", "District" = "District_Name"), selected = c("District_Name"), multiple = TRUE),
                         pickerInput("summary_by", "Summarise by (second tab):", c("Date" = "today","Member organisation"="Member_id" , "Community" = "Community_Name","District"= "District_Name","Username" = "ONA_username"), selected = c("today"), multiple = TRUE),
                         pickerInput("filter_date", "Filter by date",sort(unique(listData()$today), na.last=TRUE),selected = unique(listData()$today),options = list(`actions-box` = TRUE), multiple = T),
                         pickerInput("filter_partner", "Filter by member",sort(unique(listData()$Member_id), na.last=TRUE),selected = unique(listData()$Member_id),options = list(`actions-box` = TRUE), multiple = T),
                         pickerInput("filter_district", "Filter by district",sort(unique(listData()$District_Name), na.last=TRUE),selected = unique(listData()$District_Name),options = list(`actions-box` = TRUE), multiple = T),
                         pickerInput("filter_username", "Filter by username",sort(unique(listData()$ONA_username), na.last=TRUE),selected=unique(listData()$ONA_username),options = list(`actions-box` = TRUE), multiple = T),
                         ),
                  ## The info boxes
                  column(8,offset = 0.5,
                         valueBoxOutput("valid.interviews"),
                         valueBoxOutput("duration"),
                         valueBoxOutput("consent"),
                         valueBoxOutput("matching"),
                         valueBoxOutput("deaths"),
                         valueBoxOutput("c19")
                  ),
                  ## tabs
                  column(8,offset = 0.5,
                         tabsetPanel(type = "tabs",
                                     ## target vs achieved table
                                     tabPanel(tags$span(style="font-weight:bold;font-size:40;","Target vs achieved"),
                                              dataTableOutput("target"),
                                              absolutePanel(id = "absolutePanel",top = 45, right = "auto", left = "260", 
                                                            bottom = "auto",
                                                            width = 80,draggable = TRUE,
                                                            downloadButton(outputId = "download_target",
                                                                           label = "Download this table")
                                              
                                              )),
                                     ## summary (survey duration) table
                                     tabPanel(tags$span(style="font-weight:bold;font-size:40;","Summary (survey duration)"),
                                              br(),
                                              uiOutput("title"),
                                              br(),
                                               dataTableOutput("summary"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                              absolutePanel(id = "absolutePanel",top = 90, right = "auto", left = "260", 
                                                            bottom = "auto",
                                                            width = 80,draggable = TRUE,
                                                            downloadButton(outputId = "download_summary",
                                                                           label = "Download this table")
                                              )),
                                     ## all data table
                                     tabPanel(tags$span(style="font-weight:bold;font-size:40;","All data"),
                                              dataTableOutput("all.data"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                              absolutePanel(id = "absolutePanel",top = 45, right = "auto", left = "260", 
                                                            bottom = "auto",
                                                            width = 80,draggable = TRUE,
                                                            downloadButton(outputId = "download_all",
                                                                           label = "Download this table")
                                                            
                                              ))
                         )
                )
              )))
