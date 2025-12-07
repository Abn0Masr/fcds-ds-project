library(shiny)
library(bslib)
library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(DT)
library(ggcorrplot)


ui <- page_sidebar(
  "Food Delivery",
  sidebar = sidebar(
    title = "Control Panel",
    fileInput("dataset", "Upload dataset", accept = c(".csv", "text/csv")),
    hr(),
    h2("Algorithm Parameters"),
    sliderInput("support", label = "Min support value: ", min = 0.01, max = 1, value = 0.01, step = 0.01),
    sliderInput("confidence", label = "Confidence value: ", min = 0.4, max = 1, value = 0.4, step = 0.01),
    sliderInput("clusters", "Number of Clusters:", min = 2, max = 10, value = 3),
    hr(),
    downloadButton("download_data", "Download Data")
  ),
  mainPanel(
    width = 12,
    navset_tab(
      id = "tabs",
      nav_panel(
        "Overview",
        layout_column_wrap(
          card(card_header("Data overview"), card_body(DT::dataTableOutput("overview_table"))),
          card(card_header("Data before clean"), card_body(plotOutput("before_cleaned"))),
          card(card_header("Data Clean"), card_body(plotOutput("cleaned"))),
          width = 1
        )
      ),
      nav_panel(
        "Overall and Spread Delivery Time",
        layout_column_wrap(
          card(card_header("Delivery time frequancy"), card_body(plotOutput("hist_delivery_time"))),
          card(card_header("Delivery time central tendency"), card_body(plotOutput("centeral_delivery_time"))),
          width = 1,
        ),
        layout_column_wrap(
          value_box(title = "Mean",value = textOutput("mean")),
          value_box(title = "Median",value = textOutput("median")),
          value_box(title = "Min",value = textOutput("min")),
          value_box(title = "Max",value = textOutput("max"))
        )
      ),
      nav_panel(
        "Vehicle performance",
        layout_column_wrap(
          card(card_header("Percentage of Orders by Vehicle Type"), card_body(plotOutput("perc_orders_vehicle"))),
          card(card_header("Percentage of Status Delivery by Vehicle Type"), card_body(plotOutput("perc_setuts_del_vehicle"))),
          width = 1/2
        ),
        layout_column_wrap(
          card(card_header("Vehicle vs delivery time"), card_body(plotOutput("vehicle_delivery_time"))),
          card(card_header("Vehicle vs experience"), card_body(plotOutput("vehicle_experience"))),
          card(card_header("Vehicle vs distance"), card_body(plotOutput("vehicle_distance"))),
          card(card_header("Vehicle vs speed"), card_body(plotOutput("vehicle_speed"))),
          width = 1
        )
      ),
      nav_panel("Distribution",uiOutput("num_distrbution")),
      nav_panel("Orders by Category",uiOutput("ordrByctgry")),
      nav_panel("Correlation",plotOutput("corr")),
      nav_panel("Delivery Time Throughout Day",uiOutput("del_time_day")),
      nav_panel(
        "Association Rules",
        navset_tab(
          nav_panel("Patterns lead to late delivery",plotOutput("rule_late")),
          nav_panel("Patterns lead to ontime delivery",plotOutput("rule_ontime"))
        )
      ),
      nav_panel(
        "Clusters",
        layout_column_wrap(
          card(card_header("Plot"), card_body(plotOutput("kmeans_plot"))),
          card(card_header("plot"), card_body(plotOutput("kmeans_improve_plot"))),
        ),
        verbatimTextOutput("kmeans_summary")
      ),
      nav_panel(
        "Add New Record",
        numericInput("order_id", "Order ID: ", value = 0),
        numericInput("new_distance", "Distance (km):", value = 5, min = 0),
        numericInput("new_prep", "Preparation Time (min):", value = 10, min = 0),
        numericInput("new_experience", "Courier Experience (yrs):", value = 1, min = 0),
        numericInput("new_delivery", "Delivery Time (min):", value = 30, min = 0),
        selectInput("new_weather", "Weather:", choices = c("Clear", "Foggy", "Rainy", "Snowy", "Windy")),
        selectInput("new_traffic", "Traffic:", choices = c("Low", "Medium", "High")),
        selectInput("new_time", "Time of Day:", choices = c("Morning", "Afternoon", "Evening", "Night")),
        selectInput("new_vehicle", "Vehicle:", choices = c("Bike", "Car", "Scooter")),
        actionButton("add_record", "Add Record")
      )
    )
  )
)


server <- function(input, output, session) {
  # add data
  dataload <- reactiveVal()
  outliers <- reactiveVal()
  kmeans_result <- reactiveVal()

  # handle data insert
  observeEvent(input$add_record, {
    new_row <- data.frame(
      Order_ID = input$order_id,
      Distance_km = input$new_distance,
      Preparation_Time_min = input$new_prep,
      Courier_Experience_yrs = input$new_experience,
      Delivery_Time_min = input$new_delivery,
      Weather = input$new_weather,
      Traffic_Level = input$new_traffic,
      Time_of_Day = input$new_time,
      Vehicle_Type = input$new_vehicle
    )

    if (!is.null(dataload()) && input$order_id %in% dataload()$Order_ID) {
      showNotification(paste0("Order ID ", input$order_id, " already exists!"), type = "error", duration = 3)
      return(NULL)
    }

    if (is.null(dataload())) {
      dataload(new_row)
    } else {
      dataload(rbind(dataload(), new_row))
    }

    showNotification(paste0("Order", input$order_id, "was added"), type = "default", duration = 2)

    updateNumericInput(session, "order_id", value = max(dataload()$Order_ID) + 1)
    updateNumericInput(session, "new_distance", value = 5)
    updateNumericInput(session, "new_prep", value = 10)
    updateNumericInput(session, "new_experience", value = 1)
    updateNumericInput(session, "new_delivery", value = 30)
    updateSelectInput(session, "new_weather", selected = "Clear")
    updateSelectInput(session, "new_traffic", selected = "Low")
    updateSelectInput(session, "new_time", selected = "Morning")
    updateSelectInput(session, "new_vehicle", selected = "Bike")
  })
  observeEvent(input$dataset, {
    req(input$dataset)
    user_data <- read.csv(input$dataset$datapath)
    
    required_cols <- c("Order_ID","Distance_km","Preparation_Time_min","Courier_Experience_yrs",
                       "Delivery_Time_min","Weather","Traffic_Level","Time_of_Day","Vehicle_Type")
    
    if (!all(required_cols %in% colnames(user_data))) {
      showNotification("Invalid dataset file!", type = "error")
      return(NULL)
    }
    
    dataload(rbind(dataload(), user_data))
    updateNumericInput(session, "order_id", value = max(dataload()$Order_ID) + 1)
  })

  # data clean
  dataset <- reactive({
    data <- dataload()

    if (is.null(data)) {
      return(NULL)
    }

    # Rename data to be easy understand
    data <- data %>% rename(
      OrderID = Order_ID,
      Distance = Distance_km,
      Weather = Weather,
      Traffic = Traffic_Level,
      Time = Time_of_Day,
      Vehicle = Vehicle_Type,
      PreparationTime = Preparation_Time_min,
      Experience = Courier_Experience_yrs,
      DeliveryTime = Delivery_Time_min
    )

    # Replace NA values with median to more accurate analysis
    data_numeric_cols <- names(select(select(data, -OrderID), where(is.numeric)))
    for (col in data_numeric_cols) {
      data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }

    # Replace NA values with mod to more accurate analysis
    data_factor_cols <- names(select(data, where(is.character), where(is.factor)))
    get_mod <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    for (col in data_factor_cols) {
      mode_value <- get_mod(data[[col]])
      data[[col]][data[[col]] == ""] <- NA
      data[[col]][is.na(data[[col]])] <- mode_value
    }

    # Replace outliers with upper or lower bounders
    outliers(data %>% select(where(is.numeric), -OrderID)) # Add data before remove outliers to detect him and show
    remove_outliers <- function(df) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      for (col in num_cols) {
        Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
        Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
        IQR_val <- IQR(df[[col]], na.rm = TRUE)

        lower <- Q1 - 1.5 * IQR_val
        upper <- Q3 + 1.5 * IQR_val

        df[[col]][df[[col]] < lower] <- lower
        df[[col]][df[[col]] > upper] <- upper
      }
      return(df)
    }
    data <- remove_outliers(data)

    # Convert columns to correct type to use
    data <- data %>%
      mutate(
        OrderID = as.integer(OrderID),
        Weather = as.factor(Weather),
        Traffic = as.factor(Traffic),
        Time = as.factor(Time),
        Vehicle = as.factor(Vehicle),
        Experience = as.numeric(Experience),
        Distance = as.numeric(Distance),
        PreparationTime = as.numeric(PreparationTime),
        DeliveryTime = as.numeric(DeliveryTime)
      )
    
    data$Speed <- data$Distance / (data$DeliveryTime/60)
    
    ml <- lm(DeliveryTime ~ Distance +Experience+ Traffic + Weather + Vehicle, data=data)
    newData<-predict(ml,data)
    data$ExpectedTime <- newData
    data$DeliveryLate <- as.factor(ifelse(data$DeliveryTime > data$ExpectedTime, "Late", "Ontime"))
    data <- data %>% select(-ExpectedTime)

    df_k <- data %>% select(Distance,DeliveryTime)
    scaled_df <- scale(df_k)

    km <- kmeans(scaled_df, centers = input$clusters)
    kmeans_result(list(km = km, scaled_df = scaled_df))

    data$Cluster <- as.factor(km$cluster)

    data
  })
  
  # data overview
  observe({
    req(dataset())
    data<-dataset()
    
    output$overview_table <- DT::renderDataTable({
      data$Speed <- as.integer(data$Speed)
      DT::datatable(data, options = list(pageLength = 5))
    })
    output$before_cleaned <- renderPlot({
      boxplot(outliers())
    })
    output$cleaned <- renderPlot({
      boxplot(data %>% select(where(is.numeric), -OrderID))
    })
  })
  
  # Overall Delivery Time
  observe({
    req(dataset())
    data<-dataset()
    
    output$hist_delivery_time <- renderPlot({
      ggplot(data, aes(DeliveryTime)) +
        geom_histogram(fill = "steelblue", color = "black") +
        labs(x = "Delivery Time (min)", title = "Delivery time frequancy") +
        theme_minimal()
    })
    output$centeral_delivery_time <- renderPlot({
      ggplot(data, aes(y=DeliveryTime)) +
        geom_boxplot(fill = "skyblue") +
        labs(y = "Delivery Time (min)",title="Delivery time centeral tendency") +
        theme_minimal()
    })
    output$mean <- renderText({
      mean(data$DeliveryTime, na.rm = TRUE)
    })
    output$median <- renderText({
      median(data$DeliveryTime, na.rm = TRUE)
    })
    output$min <- renderText({
      min(data$DeliveryTime, na.rm = TRUE)
    })
    output$max <- renderText({
      max(data$DeliveryTime, na.rm = TRUE)
    })
  })
  
  # Delivery by Vehicle Type
  observe({
    req(dataset())
    data<-dataset()
    
    output$perc_setuts_del_vehicle <- renderPlot({
      df <- data %>%
        group_by(Vehicle, DeliveryLate) %>%
        summarize(count = n())
      
      ggplot(df, aes(x = Vehicle, y = count, fill = DeliveryLate)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        labs(
          title = "Delivery Status by Vehicle",
          x = "Vehicle Type",
          y = "Percentage",
          fill = "Delivery Status"
        ) +
        theme_minimal()
    })
    output$perc_orders_vehicle<-renderPlot({
      vehicle_counts <- data %>%
        count(Vehicle) %>%
        mutate(perc = round(n / sum(n) * 100, 1),
               label = paste0(Vehicle, " (", perc, "%)"))
      
      pie(vehicle_counts$perc,
          labels = vehicle_counts$label,
          main = "Percentage of Orders by Vehicle Type")
    })
    output$vehicle_delivery_time <- renderPlot({
      ggplot(data, aes(x = Vehicle, y = DeliveryTime, fill = Vehicle)) +
        geom_boxplot() +
        labs(title = "Delivery Time Distribution by Vehicle", x = "Vehicle", y = "Delivery Time (min)") +
        theme_minimal()
    })
    output$vehicle_experience <- renderPlot({
      ggplot(data, aes(x = Vehicle, y = Experience, fill = Vehicle)) +
        geom_boxplot() +
        labs(title = "Experience by Vehicle", x = "Vehicle", y = "Experience (years)") +
        theme_minimal()
    })
    output$vehicle_distance <- renderPlot({
      ggplot(data, aes(x = Vehicle, y = Distance, fill = Vehicle)) +
        geom_boxplot() +
        labs(title = "Distance by Vehicle", x = "Vehicle", y = "Distance (km)") +
        theme_minimal()
    })
    output$vehicle_speed <- renderPlot({
      ggplot(data, aes(x = Vehicle, y = Speed, fill = Vehicle)) +
        geom_boxplot() +
        labs(title = "Speed by Vehicle", x = "Vehicle", y = "Speed (km/h)") +
        theme_minimal()
    })
  })
  
  # Distribution
  observe({
    req(dataset())
    data<-dataset()
    
    distribution_cols<-c("Distance","PreparationTime","Speed","Experience")
    distribution_by_col<-function(data,column){
      ggplot(data, aes_string(x = column)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
        labs(x = column, y = "Frequency") +
        theme_minimal()
    }
    
    output$num_distrbution<-renderUI({
      plot_list <- lapply(distribution_cols, function(col){
        card(card_body(plotOutput(paste0("num_distrbution_",col))))
      })
      do.call(layout_column_wrap,c(plot_list, list(width = 1/2)))
    })
    for(col in distribution_cols) {
      local({
        mycol <- col
        output[[paste0("num_distrbution_", mycol)]] <- renderPlot({distribution_by_col(data, mycol)})
      })
    }
  })
  
  # Orders by Category
  observe({
    req(dataset())
    data<-dataset()
    
    ordrByctgry<-c("Traffic","Weather","Time","DeliveryLate") # cols want present
    count_by_categoral<-function(data,column) {
      ggplot(data, aes_string(x = column, fill = column)) +
        geom_bar() +
        labs(
          title = paste("Number of orders for",column),
          y = "Count of Orders",
          x = column
        ) +
        theme_minimal()
    }
    
    output$ordrByctgry<-renderUI({
      plot_list <- lapply(ordrByctgry, function(col) {
        card(card_body(plotOutput(paste0("plot_", col))))
      })
      do.call(layout_column_wrap,c(plot_list, list(width = 1/2)))
    })
    for(col in ordrByctgry) {
      local({
        mycol <- col
        output[[paste0("plot_", mycol)]] <- renderPlot({count_by_categoral(data, mycol)})
      })
    }
  })
  
  # Delivery Time Throughout Day
  observe({
    req(dataset())
    data<-dataset()
    
    render_plot<-function(data,column) {
      ggplot(data, aes_string(x = column, y = "DeliveryTime", fill = column)) +
        geom_boxplot() +
        labs(x = column, y = "Delivery time (min)") +
        theme_minimal()
    }
    cols<-c("Time","Weather","Traffic")
    
    output$del_time_day <- renderUI({
      plot_list <- lapply(cols, function(col){
        card(card_body(plotOutput(paste0("del_time_day_",col))))
      })
      do.call(layout_column_wrap,c(plot_list, list(width = 1)))
    })
    
    for(col in cols) {
      local({
        mycol <- col
        output[[paste0("del_time_day_", mycol)]] <- renderPlot({render_plot(data, mycol)})
      })
    }
  })
  
  # Correlation
  observe({
    req(dataset())
    data<-dataset()
    
    output$corr <- renderPlot({
      df <- data %>% select(where(is.numeric),-OrderID)
      cor_matrix <- cor(df)
      ggcorrplot(cor_matrix,
                 hc.order = TRUE, type = "lower",
                 lab = TRUE, lab_size = 3,
                 method = "square", colors = c("blue", "white", "red")
      )
    })
  })
  
  # kmeans
  observe({
    req(dataset())
    data<-dataset()
    
    output$kmeans_plot <- renderPlot({
      fviz_cluster(
        kmeans_result()$km,
        data = kmeans_result()$scaled_df,
        geom = "point",
        ellipse.type = "convex",
        palette = "jco",
        ggtheme = theme_minimal(),
        show.clust.cent = TRUE
      )
    })
    output$kmeans_improve_plot <- renderPlot({
      fviz_nbclust(kmeans_result()$scaled_df, kmeans, method = "silhouette")
    })
    output$kmeans_summary <- renderPrint({
      aggregate(cbind(Distance, DeliveryTime, Speed) ~ Cluster, data=data, FUN=mean)
    })
  })
  
  # assoucation rules
  observe({
    req(dataset())
    data<-dataset()
    
    render_arules<-function(data,flag) {
      df <- data %>% select(Traffic, Vehicle, DeliveryLate, Weather, Time)
      trans <- as(df, "transactions")
      ru <- apriori(
        trans,
        parameter = list(support = input$support, confidence = input$confidence, minlen = 2),
        appearance = list(
          rhs = grep(paste0("DeliveryLate=",flag), itemLabels(trans), value = TRUE)
        )
      )
      ig<-plot(sort(ru,by ="lift"), method = "grouped", control = list(k = 5) ,limit = 20)
      ig$layers[[1]]$aes_params$edge_alpha = 0.8
      ig$layers[[1]]$aes_params$edge_width = 0.8
      plot(ig)
    }
    output$rule_late <- renderPlot({
      render_arules(data,"Late")
    })
    output$rule_ontime <- renderPlot({
      render_arules(data,"Ontime")
    })
  })
  
  observeEvent(dataset(), {
    req(dataset())
    data<-dataset()
    
    if (!is.null(data)) {
      output$download_data <- downloadHandler(
        filename = function() {
          paste("food_delivery_data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(data, file, row.names = FALSE)
        }
      )
    }
  })
}


shinyApp(ui = ui, server = server)
