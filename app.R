
#############################################################
##### data prep

#define top x specialties to investigate
num_specialties <- 20

#determine top spend by specialty
##group by specialty, take top x in sum spend, then sort by highest mean spend
top_spending_specialties <- claim_lines %>% 
  group_by(HICN, specialty) %>% 
  summarize(claim_lines = n(), 
            Paid_Amt = sum(Paid_Amt, na.rm = TRUE),
            HCC = mean(HCC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(specialty) %>% 
  summarize(num_patients = n(),
            sum_spend = sum(Paid_Amt, na.rm = TRUE),
            mean_spend = mean(Paid_Amt, na.rm = TRUE),
            mean_hcc = mean(HCC, na.rm = TRUE)) %>% 
  arrange(desc(sum_spend)) %>% 
  filter(!is.na(specialty)) %>% 
  filter(num_patients > 100)

specialty_list <- top_spending_specialties %>% 
  arrange(desc(mean_spend)) %$%
  as.character(specialty)

#group by specialist for use in charts
specialist_shiny <- claim_lines %>%
  filter(specialty %in% specialty_list) %>%
  group_by(HICN, Serv_NPI, specialty) %>%
  summarize(sum_paid_amt = sum(Paid_Amt, na.rm = TRUE),
            HCC = mean(HCC, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(Serv_NPI, specialty) %>%
  summarize(num_patients = n(),
            sum_spend = sum(sum_paid_amt, na.rm=TRUE),
            mean_spend = mean(sum_paid_amt, na.rm=TRUE),
            mean_hcc = mean(HCC, na.rm=TRUE)) %>%
  arrange(desc(mean_spend)) %>%
  filter(num_patients >= 3)

#data prep for injectables
injectables <- claim_lines %>% 
  bind_rows(claim_lines_2016) %>% 
  filter(str_detect(CPT_Code, "^J")) %>% 
  arrange(Beg_Serv_Dt, HICN) %>% 
  group_by(Beg_Serv_Dt, End_Serv_Dt, HICN, CPT_Code) %>% 
  summarize(procedures = n(),
            Paid_Amt = sum(Paid_Amt)) %>% 
  ungroup() %>% 
  group_by(year = year(End_Serv_Dt), quarter = quarter(End_Serv_Dt), 
           month = month(End_Serv_Dt), CPT_Code) %>% 
  summarize(count = n(),
            procedures = sum(procedures),
            sum_spend = sum(Paid_Amt),
            mean_spend = round(mean(Paid_Amt), 0)) %>% 
  ungroup() %>% 
  left_join(codes, by = c("CPT_Code" = "Code")) %>% 
  rename(CPT_Desc = Description) %>% 
  mutate(year = as.factor(year),
         quarter = as.factor(quarter),
         month = as.factor(month)) %>% 
  arrange(desc(sum_spend))

injectable_list <- injectables %>% 
  group_by(CPT_Code) %>% 
  summarize(count = n(),
            sum_spend = sum(sum_spend)) %>% 
  arrange(desc(sum_spend)) %>% 
  filter(sum_spend > 100000) %>% 
  filter(count >= 12) %$%
  as.character(CPT_Code)

#############################################################
##### build UI

ui <- fluidPage(
  titlePanel("Specialty and Provider Spend (Prototype)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("specialtyInput", "Select Specialty:",
                  choices = specialty_list),
      br(), br(),
      textInput("npiInput", "Select NPI:", value = NA),
      br(), br(),
      textInput("hicnInput", "Select HICN:", value = NA),
      br(), br(),
      selectInput("injInput", "Select Injectable:",
                  choices = injectable_list)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overall",
          plotOutput("overallHCCplot"),
          br(),
          tableOutput("overall_results")
        ),
        tabPanel("Specialty - Provider",
          plotOutput("spendHCCplot"),
          br(),
          tableOutput("specialty_results")
        ),
        tabPanel("Specialty - Procedure",
          tableOutput("CPT_results"),
          br(),
          tableOutput("DX_results")
        ),
        tabPanel("NPI",
          plotOutput("physician_spendHCCplot"),
          br(),
          tableOutput("physician_results")
        ),
        tabPanel("Patient",
           tableOutput("patient_claims")
        ),
        tabPanel("Injectables",
          plotOutput("injectables_select"),
          br(),
          tableOutput("injectables_select_table")
        )
      )
    )
  )
)

server <- function(input, output) {
#############################################################
##### create overall tab  

  output$overallHCCplot <- renderPlot({
    top_spending_specialties %>%
      head(num_specialties) %>% 
      arrange(desc(mean_spend)) %>% 
      ggplot(aes(x = mean_hcc, y = mean_spend, label = specialty)) + 
        geom_point(color = "#4B9CD3") +
        geom_text_repel() +
        scale_y_continuous(labels = scales::dollar) +
        labs(
          title="Spending Per Patient vs HCC Risk Score by Specialty",
          x="Average HCC Risk Score",
          y="Average Spend Per Patient") +
        theme_bw(base_size = 20)
  })
  
  output$overall_results <- renderTable({
    top_spending_specialties %>% 
      head(num_specialties) %>% 
      arrange(desc(mean_spend)) %>% 
      select(specialty, mean_spend, num_patients, mean_hcc) %>% 
      rename(Specialty = specialty,
             `Number of Patients` = num_patients,
             `Average Spend per Patient` = mean_spend,
             `Average Patient HCC Score` = mean_hcc)
  }, 
  caption = paste0("<p><h3> Top ", num_specialties, " Specialties by Highest Total Spend </h3></p>
                   <p><h4> Sorted by Average Spend Per Patient </h4></p>"),
  caption.placement = getOption("xtable.caption.placement", "top"), 
  width = '110%',
  hover = TRUE
  )

#############################################################
##### create specialty-provider tab  
  
  output$spendHCCplot <- renderPlot({
      specialist_shiny %>%  
      filter(specialty == input$specialtyInput) %>% 
      filter(mean_spend > 0) %>% 
      rename(`Number of Patients` = num_patients) %>% 
        ggplot(aes(x = mean_hcc, y = mean_spend, size = `Number of Patients`)) + 
        geom_point(color = "#4B9CD3") +
        scale_y_continuous(labels = scales::dollar) +
        geom_hline(aes(yintercept=mean(mean_spend, na.rm = TRUE)), color="#990000", linetype="dashed") +
        geom_vline(aes(xintercept=mean(mean_hcc, na.rm = TRUE)), color="#990000", linetype="dashed") +
        labs(
          title=paste0("Specialty - ", input$specialtyInput),
          x="Average HCC Risk Score",
          y="Average Spend Per Patient") +
        #guides(size = FALSE) +
        theme_bw(base_size = 20)
  })
  
  output$specialty_results <- renderTable({
    specialist_shiny %>% 
      filter(specialty == input$specialtyInput) %>% 
      left_join(npi_names, by = "Serv_NPI") %>% 
      select(specialty, Serv_NPI, `Provider Full Name`, num_patients, mean_spend, mean_hcc) %>% 
      rename(NPI = Serv_NPI, 
             Specialty = specialty,
             `Number of Patients` = num_patients,
             `Mean Spend` = mean_spend,
             `Mean HCC Score` = mean_hcc) %>% 
      head(5)
    }, 
    caption = "<h3> Top 5 Providers by Highest Mean Spend within Specialty </h3>",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    width = '90%',
    hover = TRUE
    )

#############################################################
##### create specialty-CPT/DX tab 
  
  #define mode function
  modelist <- function(x) with(rle(sort(x)), values[order(lengths, decreasing = TRUE)])
  
  #table of top 5 CPTs and corresponding DX codes
  output$CPT_results <- renderTable({
    claim_lines %>% 
      filter(specialty == input$specialtyInput) %>% 
      group_by(CPT_Code, CPT_Desc, specialty) %>% 
      summarize(count = n(), 
                mean_spend = mean(Paid_Amt, na.rm=TRUE),
                most_common_DX = modelist(DX_Code1)[1],
                second_most_common_DX = modelist(DX_Code1)[2],
                third_most_common_DX = modelist(DX_Code1)[3]) %>% 
      ungroup() %>% 
      arrange(desc(mean_spend)) %>% 
      filter(!is.na(CPT_Code)) %>% 
      filter(count >= 5) %>% 
      head(5) %>% 
      select(specialty, CPT_Code, CPT_Desc, count, mean_spend, most_common_DX, second_most_common_DX) %>% 
      rename(Specialty = specialty,
             `CPT Code` = CPT_Code,
             `CPT Description` = CPT_Desc,
             `CPT Occurances` = count,
             `Average Spend` = mean_spend,
             `Most Common DX Code` = most_common_DX,
             `Second Most Common DX Code` = second_most_common_DX)
  }, 
  caption = "<h3> Top 5 Most Expensive CPT Codes by Specialty </h3>",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  width = '90%',
  hover = TRUE
  )
  
  
  #table of top 5 DX codes and corresponding CPTs
  output$DX_results <- renderTable({
    claim_lines %>% 
      filter(specialty == input$specialtyInput) %>% 
      group_by(DX_Code1, DX_Desc1, specialty) %>% 
      summarize(count = n(), 
                mean_spend = mean(Paid_Amt, na.rm=TRUE),
                most_common_CPT = modelist(CPT_Code)[1],
                second_most_common_CPT = modelist(CPT_Code)[2],
                third_most_common_CPT = modelist(CPT_Code)[3]) %>% 
      ungroup() %>% 
      arrange(desc(mean_spend)) %>% 
      filter(!is.na(DX_Code1)) %>% 
      filter(count >= 5) %>% 
      head(5) %>% 
      select(specialty, DX_Code1, DX_Desc1, count, mean_spend, most_common_CPT, second_most_common_CPT) %>% 
      rename(Specialty = specialty,
             `DX Code` = DX_Code1,
             `DX Description` = DX_Desc1,
             `DX Occurances` = count,
             `Average Spend` = mean_spend,
             `Most Common CPT Code` = most_common_CPT,
             `Second Most Common CPT Code` = second_most_common_CPT)
  }, 
  caption = "<h3> Top 5 Most Expensive DX Codes by Specialty </h3>",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  width = '90%',
  hover = TRUE
  )
  
#############################################################
##### create physician tab
  
  output$physician_spendHCCplot <- renderPlot({
    claim_lines %>%  
      filter(Serv_NPI == input$npiInput) %>% 
      group_by(HICN, Serv_NPI, specialty) %>% 
      summarize(sum_spend = sum(Paid_Amt, na.rm=TRUE),
                HCC = mean(HCC, na.rm=TRUE)) %>%
      arrange(desc(sum_spend)) %>% 
      ggplot(aes(x = HCC, y = sum_spend)) + 
        geom_point(color = "#4B9CD3") +
        scale_y_continuous(labels = scales::dollar) +
        #geom_hline(aes(yintercept=mean(sum_spend, na.rm = TRUE)), color="#990000", linetype="dashed") +
        #geom_vline(aes(xintercept=mean(HCC, na.rm = TRUE)), color="#990000", linetype="dashed") +
        labs(
          title=paste0("NPI #", input$npiInput),
          x="Patient HCC Risk Score",
          y="Total Patient Spend") +
        #guides(size = FALSE) +
        theme_bw(base_size = 20)
  })
  
  output$physician_results <- renderTable({
    claim_lines %>%  
      filter(Serv_NPI == input$npiInput) %>% 
      group_by(HICN, Serv_NPI, `Provider Full Name`, specialty) %>% 
      summarize(`Total Spend` = sum(Paid_Amt, na.rm=TRUE),
                HCC = mean(HCC, na.rm=TRUE)) %>%
      rename(NPI = Serv_NPI,
             Specialty = specialty) %>% 
      arrange(desc(`Total Spend`)) %>% 
      select(NPI, Specialty, HICN, `Total Spend`, HCC) %>% 
      head(5)
  }, 
  caption = "<h3> Top 5 Most Expensive Patients by Total Spend </h3>",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  width = '95%',
  hover = TRUE
  )
  
#############################################################
##### create patient tab
  
  output$patient_claims <- renderTable({
    claim_lines %>%
      #filter(Serv_NPI == input$npiInput) %>% #not sure if I should include this filter or not
      filter(HICN == input$hicnInput) %>%
      arrange(Beg_Serv_Dt) %>% 
      mutate(Beg_Serv_Dt = as.character(Beg_Serv_Dt),
             End_Serv_Dt = as.character(End_Serv_Dt))
  },
  caption = paste0("<h3> Patient Claim Line Details </h3>"),
                   caption.placement = getOption("xtable.caption.placement", "top"),
                   width = '95%',
                   hover = TRUE
  )
  
#############################################################
##### create injectible tab  
  
  output$injectables_select <- renderPlot({
    injectables %>% 
      filter(CPT_Code == input$injInput) %>% 
      group_by(year, quarter, CPT_Desc) %>% 
      summarize(sum_spend = sum(sum_spend)) %>% 
      ungroup() %>% 
      ggplot(aes(x = quarter, y = sum_spend, color = year)) +
      geom_line(aes(group = year), size = 2) + 
      scale_x_discrete(name = "Quarter") + 
      scale_y_continuous(name = "Total Spend",
                         labels = scales::dollar) + 
      scale_color_manual(name = "Year",
                         values = c("#707C7C", "#4B9CD3")) +
      labs(
        title=paste0("Total Spend by Year and Quarter, CPT Code ", input$injInput),
        x="Quarter",
        y="Total Spend") +
      theme_bw(base_size = 20)
  })
  
  output$injectables_select_table <- renderTable({
    injectables %>% 
      filter(CPT_Code == input$injInput) %>% 
      group_by(year, quarter, CPT_Code, CPT_Desc) %>% 
      summarize(`Number of Instances` = sum(procedures),
                `Total Spend` = sum(sum_spend)) %>% 
      ungroup() %>% 
      rename(Year = year,
             Quarter = quarter) %>% 
      select(CPT_Code, CPT_Desc, everything())
  }, 
  caption = "<h3> Total Spend by Year and Quarter </h3>",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  width = '95%',
  hover = TRUE
  ) 
  

}

shinyApp(ui = ui, server = server)