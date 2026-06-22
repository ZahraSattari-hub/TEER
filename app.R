


# ==========================================================
# TEER Dashboard
# Save as: app.R
# Run with:
# shiny::runApp()
# ==========================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(readxl)
library(pracma)

# ==========================================================
# LOAD DATA
# ==========================================================

df <- read_excel("teer.xlsx") %>%
  rename(
    Animal = Cow_nr,
    Treatment = treatment
  ) %>%
  mutate(
    Animal = factor(Animal),
    Treatment = factor(
      Treatment,
      levels = c(0, 1),
      labels = c("Control", "DM treated")
    ),
    day = as.numeric(day)
  )

# ==========================================================
# AGGREGATED DATA FOR METRICS
# ==========================================================

trajectory_df <- df %>%
  group_by(Animal, Treatment, day) %>%
  summarise(
    mean_teer = mean(teer, na.rm = TRUE),
    .groups = "drop"
  )

# ==========================================================
# RESPONSE METRICS
# ==========================================================

metrics_df <- trajectory_df %>%
  arrange(day) %>%
  group_by(Animal, Treatment) %>%
  summarise(
    
    Peak_TEER =
      max(mean_teer, na.rm = TRUE),
    
    Minimum_TEER =
      min(mean_teer, na.rm = TRUE),
    
    Time_to_Peak =
      day[which.max(mean_teer)],
    
    AUC =
      trapz(day, mean_teer),
    
    .groups = "drop"
  )

# ==========================================================
# UI
# ==========================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "TEER Dashboard"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Study Description",
        tabName = "study",
        icon = icon("file-alt")
      ),
      
      menuItem(
        "Animal Dynamics",
        tabName = "animal",
        icon = icon("cow")
      ),
      
      menuItem(
        "Treatment Dynamics",
        tabName = "treatment",
        icon = icon("vials")
      ),
      
      menuItem(
        "Response Metrics",
        tabName = "metrics",
        icon = icon("table")
      ),
      
      menuItem(
        "Download Results",
        tabName = "download",
        icon = icon("download")
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ====================================================
      # OVERVIEW
      # ====================================================
      
      tabItem(
        
        tabName = "overview",
        
        fluidRow(
          
          valueBox(
            value = nrow(df),
            subtitle = "Total observations",
            icon = icon("database"),
            width = 3
          ),
          
          valueBox(
            value = nlevels(df$Animal),
            subtitle = "Animals",
            icon = icon("cow"),
            width = 3
          ),
          
          valueBox(
            value = nlevels(df$Treatment),
            subtitle = "Treatment groups",
            icon = icon("vials"),
            width = 3
          ),
          
          valueBox(
            value = length(unique(df$day)),
            subtitle = "Time points",
            icon = icon("clock"),
            width = 3
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Dataset Summary",
            
            DTOutput("overview_table")
          )
          
        )
        
      ),
      
      # ====================================================
      # STUDY DESCRIPTION
      # ====================================================
      
      tabItem(
        
        tabName = "study",
        
        fluidRow(
          
          box(
            width = 12,
            title = "Study Description",
            
            HTML("
            <h4>Experimental Design</h4>

            <ul>
            <li>Mammary epithelial cells (MEC) isolated from 4 cows.</li>
            <li>Each animal contained 6 DM-treated wells and 6 control wells.</li>
            <li>Each well was measured 3 times.</li>
            <li>TEER was monitored longitudinally to evaluate epithelial barrier integrity over time.</li>
            </ul>

            <h4>Dashboard Notes</h4>

            <ul>
            <li>Animal Dynamics reproduces the publication-style mean ± SE visualization by cow.</li>
            <li>Treatment Dynamics reproduces the publication-style mean ± SE visualization by treatment.</li>
            <li>Response metrics are calculated from the same mean TEER values used to construct the longitudinal trajectories displayed in the dashboard.</li>
            </ul>
            ")
            
          )
          
        )
        
      ),
      
      # ====================================================
      # ANIMAL DYNAMICS
      # ====================================================
      
      tabItem(
        
        tabName = "animal",
        
        fluidRow(
          
          box(
            width = 3,
            
            selectInput(
              "animal_select",
              "Animal",
              choices = levels(df$Animal),
              selected = levels(df$Animal),
              multiple = TRUE
            )
            
          ),
          
          box(
            width = 9,
            
            plotlyOutput(
              "animal_plot",
              height = "600px"
            )
            
          )
          
        )
        
      ),
      
      # ====================================================
      # TREATMENT DYNAMICS
      # ====================================================
      
      tabItem(
        
        tabName = "treatment",
        
        fluidRow(
          
          box(
            width = 3,
            
            checkboxGroupInput(
              "treatment_select",
              "Treatment",
              choices = levels(df$Treatment),
              selected = levels(df$Treatment)
            )
            
          ),
          
          box(
            width = 9,
            
            plotlyOutput(
              "treatment_plot",
              height = "600px"
            )
            
          )
          
        )
        
      ),
      
      # ====================================================
      # RESPONSE METRICS
      # ====================================================
      
      tabItem(
        
        tabName = "metrics",
        
        fluidRow(
          
          box(
            width = 12,
            
            DTOutput("metrics_table")
            
          )
          
        )
        
      ),
      
      # ====================================================
      # DOWNLOAD
      # ====================================================
      
      tabItem(
        
        tabName = "download",
        
        fluidRow(
          
          box(
            width = 12,
            
            h4("Download Response Metrics"),
            
            downloadButton(
              "download_metrics",
              "Download CSV"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)

# ==========================================================
# SERVER
# ==========================================================

server <- function(input, output, session){
  
  # ========================================================
  # OVERVIEW TABLE
  # ========================================================
  
  output$overview_table <- renderDT({
    
    summary_tbl <- tibble(
      
      Metric = c(
        "Total observations",
        "Animals",
        "Treatment groups",
        "Days"
      ),
      
      Value = c(
        nrow(df),
        nlevels(df$Animal),
        nlevels(df$Treatment),
        length(unique(df$day))
      )
      
    )
    
    datatable(
      summary_tbl,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "t"
      )
    )
    
  })
  
  # ========================================================
  # ANIMAL DYNAMICS
  # ========================================================
  
  output$animal_plot <- renderPlotly({
    
    animal_summary <- df %>%
      
      filter(
        Animal %in% input$animal_select
      ) %>%
      
      group_by(
        Animal,
        day
      ) %>%
      
      summarise(
        
        mean_teer = mean(teer),
        
        se =
          sd(teer) /
          sqrt(n()),
        
        .groups = "drop"
        
      )
    
    p <- ggplot(
      
      animal_summary,
      
      aes(
        x = day,
        y = mean_teer,
        color = Animal,
        group = Animal
      )
      
    ) +
      
      geom_line(linewidth = 1) +
      
      geom_point(size = 3) +
      
      geom_errorbar(
        aes(
          ymin = mean_teer - se,
          ymax = mean_teer + se
        ),
        width = 0.2
      ) +
      
      labs(
        title = "Animal Dynamics",
        x = "Day",
        y = "TEER (Ω)",
        color = "Animal"
      ) +
      
      theme_classic(base_size = 16)
    
    ggplotly(p)
    
  })
  
  # ========================================================
  # TREATMENT DYNAMICS
  # ========================================================
  
  output$treatment_plot <- renderPlotly({
    
    treatment_summary <- df %>%
      
      filter(
        day %in% c(0,2,3,4,6,7)
      ) %>%
      
      filter(
        Treatment %in% input$treatment_select
      ) %>%
      
      group_by(
        Treatment,
        day
      ) %>%
      
      summarise(
        
        mean_teer = mean(teer),
        
        se =
          sd(teer) /
          sqrt(n()),
        
        .groups = "drop"
        
      )
    
    p <- ggplot(
      
      treatment_summary,
      
      aes(
        x = day,
        y = mean_teer,
        color = Treatment,
        group = Treatment
      )
      
    ) +
      
      geom_line(linewidth = 1.5) +
      
      geom_point(size = 3) +
      
      geom_errorbar(
        aes(
          ymin = mean_teer - se,
          ymax = mean_teer + se
        ),
        width = 0.2
      ) +
      
      labs(
        title = "Treatment Dynamics",
        x = "Time (day)",
        y = "TEER (Ω)",
        color = "Treatment"
      ) +
      
      scale_x_continuous(
        limits = c(0,7),
        breaks = seq(0,7,1)
      ) +
      
      theme_classic(base_size = 18)
    
    ggplotly(p)
    
  })
  
  # ========================================================
  # METRICS TABLE
  # ========================================================
  
  output$metrics_table <- renderDT({
    
    datatable(
      metrics_df,
      rownames = FALSE,
      options = list(
        pageLength = 4,
        lengthMenu = c(1,2,3,4,6,7,8,9,10),
        scrollX = TRUE
      )
    )
    
  })
  
  metrics_df <- metrics_df %>%
    mutate(
      
      Peak_TEER = round(Peak_TEER, 2),
      
      Minimum_TEER = round(Minimum_TEER, 2),
      
      Time_to_Peak = round(Time_to_Peak, 2),
      
      AUC = round(AUC, 2)
      
    )
  

  
  # ========================================================
  # DOWNLOAD
  # ========================================================
  
  output$download_metrics <- downloadHandler(
    
    filename = function(){
      
      paste0(
        "TEER_metrics_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file){
      
      write.csv(
        metrics_df,
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}


# ==========================================================
# RUN APP
# ==========================================================

shinyApp(ui, server)

