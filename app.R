library(shiny)
library(bslib)
library(arules)
library(arulesViz)
library(dplyr)
library(ggplot2)
library(factoextra)
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
    sliderInput("clusters", "Number of Clusters:", min = 2, max = 6, value = 3),
    hr(),
    downloadButton("download_data", "Download Data")
  ),
  mainPanel(
    width = 12,
    navset_tab(
      nav_panel(
        "Overview",
        layout_column_wrap(
          card(card_header("Data overview"),card_body(DT::dataTableOutput("overview_table"))),
          card(card_header("Data Clean"),card_body(plotOutput("cleaned"))),
          width = 1
        ),
        hr(),
        verbatimTextOutput("data_summary"),
        hr(),
        verbatimTextOutput("data_structure"),
      ),
      nav_panel(
        "Overall and Spread Delivery Time",
        layout_column_wrap(
          card(card_header("Distance vs Delivery time"), card_body(plotOutput("distance_delivery_time"))),
          card(card_header("Experience vs Delivery time"), card_body(plotOutput("experience_delivery_time"))),
          card(card_header("PreparationTime vs Delivery time"), card_body(plotOutput("prep_delivery_time"))),
          width = 1,
        )
      ),
      nav_panel(
        "Delivery by Vehicle Type",
        layout_column_wrap(
          card(card_header("Vehicle vs delivery time"), card_body(plotOutput("vehicle_delivery_time"))),
          layout_column_wrap(
            card(card_header("Vehicle vs delivery late"), card_body(plotOutput("vehicle_delivery_late"))),
            card(card_header("Orders by vehicle"), card_body(plotOutput("vehicle_orders"))),
            width = 1 / 2
          ),
          width = 1
        ),
      ),
      nav_panel(
        "Numerical Distribution",
        layout_column_wrap(
          card(card_header("Distribution of Distance"), card_body(plotOutput("distribution_distance"))),
          card(card_header("Distribution of Preparation Time"), card_body(plotOutput("distribution_prep"))),
          card(card_header("Distribution of Speed"), card_body(plotOutput("distribution_speed"))),
          card(card_header("Distribution of Delivery Time"), card_body(plotOutput("distribution_delivery_time"))),
          card(card_header("Distribution of Experience"), card_body(plotOutput("distribution_Experience"))),
          width = 1 / 2
        )
      ),
      nav_panel(
        "Orders by Category",
        layout_column_wrap(
          card(card_header("Trafic"), card_body(plotOutput("trafic_orders"))),
          card(card_header("Weather"), card_body(plotOutput("weather_orders"))),
          card(card_header("Time"), card_body(plotOutput("time_orders"))),
          card(card_header("Delivery Late"), card_body(plotOutput("delivery_late_orders"))),
          width = 1 / 2
        ),
      ),
      nav_panel(
        "Correlation",
        plotOutput("corr")
      ),
      nav_panel(
        "Delivery Time Throughout Day",
        layout_column_wrap(
          card(card_header("Delivery Time by Time of Day"), card_body(plotOutput("time_by_time"))),
          card(card_header("Delivery Time by Weather of Day"), card_body(plotOutput("time_by_weather"))),
          card(card_header("Delivery Time by Traffic of Day"), card_body(plotOutput("time_by_traffic"))),
          width = 1
        ),
      ),
      nav_panel(
        "Association Rules",
        plotOutput("rule_late"),
        hr(),
        plotOutput("rule_ontime"),
      ),
      nav_panel(
        "Clusters",
        layout_column_wrap(
          card(card_header("Plot"), card_body(plotOutput("kmeans_plot"))),
          card(card_header("plot"), card_body(plotOutput("kmeans_improve_plot"))),
          card(card_header("Cluster Interpretation"), card_body(verbatimTextOutput("cluster_description")))
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
        selectInput("new_weather", "Weather:", choices = c("Clear", "Foggy", "Rainy","Snowy","Windy")),
        selectInput("new_traffic", "Traffic:", choices = c("Low", "Medium", "High")),
        selectInput("new_time", "Time of Day:", choices = c("Morning", "Afternoon", "Evening", "Night")),
        selectInput("new_vehicle", "Vehicle:", choices = c("Bike", "Car", "Scooter")),
        actionButton("add_record", "Add Record")
      )
    )
  )
)


server <- function(input, output,session) {
  # add data
  dataload <- reactiveVal(NULL)
  outliers <- reactiveVal()
  kmeans_result <- reactiveVal(NULL)

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
    if (!is.null(input$dataset)) {
      user_data <- read.csv(input$dataset$datapath)
      dataload(rbind(dataload(), user_data))
    }
  })

  # data clean
  dataset <- reactive({
    data <- dataload()

    if (is.null(data)) {
      return(NULL)
    }

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

    data_numeric_cols <- names(select(select(data, -OrderID), where(is.numeric)))
    for (col in data_numeric_cols) {
      data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
    }

    get_mod <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    data_factor_cols <- names(select(data, where(is.character), where(is.factor)))

    for (col in data_factor_cols) {
      mode_value <- get_mod(data[[col]])
      data[[col]][data[[col]] == ""] <- NA
      data[[col]][is.na(data[[col]])] <- mode_value
    }
    
    data$Speed <- data$Distance / ((data$DeliveryTime-data$PreparationTime) / 60)
    
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
    
    data <- data %>%
      mutate(
        OrderID = as.integer(OrderID),
        Weather = as.factor(Weather),
        Traffic = as.factor(Traffic),
        Time = as.factor(Time),
        Vehicle = as.factor(Vehicle),
        Experience = as.numeric(Experience),
        Speed = as.numeric(Speed),
        Distance = as.numeric(Distance),
        PreparationTime = as.numeric(PreparationTime),
        DeliveryTime = as.numeric(DeliveryTime)
      )
    
    Q3_time <- quantile(data$DeliveryTime, probs = 0.75)
    data$DeliveryLate <- as.factor(ifelse(data$DeliveryTime >= Q3_time, "Late", "Ontime"))

    df_k <- data %>% select(Distance,DeliveryTime,Speed,Experience,PreparationTime)
    scaled_df <- scale(df_k)
    
    set.seed(123)
    km <- kmeans(scaled_df, centers = input$clusters, nstart = 50)
    kmeans_result(list(km = km, scaled_df = scaled_df))
    
    data$Cluster <- as.factor(km$cluster)
    
    cluster_summary <- data %>%
      group_by(Cluster) %>%
      summarise(
        AvgDistance = mean(Distance),
        AvgDelivery = mean(DeliveryTime),
        AvgSpeed = mean(Speed),
        AvgExp = mean(Experience),
        AvgPrep = mean(PreparationTime)
      ) %>%
      mutate(
        Rating = 
          ifelse(AvgDistance < median(AvgDistance), 1, 0) +
          ifelse(AvgDelivery < median(AvgDelivery), 1, 0) +
          ifelse(AvgSpeed > median(AvgSpeed), 1, 0) +
          ifelse(AvgExp > median(AvgExp), 1, 0) +
          ifelse(AvgPrep < median(AvgPrep), 1, 0)
      )
    
    data <- data %>% left_join(cluster_summary %>% select(Cluster, Rating), by = "Cluster")
    
    data
  })

  # app
  observeEvent(dataset(), {
    data <- dataset()

    if (is.null(data)) {
      return(NULL)
    }

    # data overview
    output$overview_table <- DT::renderDataTable({
      DT::datatable(data, options = list(pageLength = 5))
    })
    output$cleaned <- renderPlot({
      boxplot(data %>% select(where(is.numeric),-OrderID))
    })
    output$data_summary <- renderPrint({
      summary(data)
    })
    output$data_structure <- renderPrint({
      str(data)
    })

    # Overall Delivery Time
    output$distance_delivery_time <- renderPlot({
      ggplot(data, aes(x = Distance, y = DeliveryTime, colour = Vehicle)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, colour = "black") +
        labs(x = "Distance (km)", y = "Delivery Time (min)") +
        theme_minimal()
    })
    output$experience_delivery_time <- renderPlot({
      ggplot(data, aes(x = Experience, y = DeliveryTime)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, colour = "blue") +
        labs(x = "Experience (years)", y = "Delivery Time (min)") +
        theme_minimal()
    })
    output$prep_delivery_time <- renderPlot({
      ggplot(data, aes(x = PreparationTime, y = DeliveryTime)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, colour = "blue") +
        labs(x = "Preparation Time (min)", y = "Delivery Time (min)") +
        theme_minimal()
    })

    # Delivery by Vehicle Type
    output$vehicle_delivery_time <- renderPlot({
      ggplot(data, aes(x = Vehicle, y = DeliveryTime, fill = Vehicle)) +
        geom_boxplot() +
        labs(title = "Delivery Time Distribution by Vehicle", x = "Vehicle", y = "Delivery Time (min)") +
        theme_minimal()
    })
    output$vehicle_delivery_late <- renderPlot({
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
    output$vehicle_orders <- renderPlot({
      ggplot(data, aes(x = Vehicle, fill = Vehicle)) +
        geom_bar() +
        labs(
          title = "Number of orders for Vehicle",
          y = "Count of Orders",
          x = " Vehicle"
        ) +
        theme_minimal()
    })

    # Numerical Feature Distribution
    output$distribution_distance <- renderPlot({
      ggplot(data, aes(x = Distance)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
        labs(x = "Distance (km)", y = "Frequency") +
        theme_minimal()
    })
    output$distribution_prep <- renderPlot({
      ggplot(data, aes(x = PreparationTime)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
        labs(x = "Prepration Time (min)", y = "Frequency") +
        theme_minimal()
    })
    output$distribution_speed <- renderPlot({
      ggplot(data, aes(x = Speed)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
        labs(x = "Speed (km/h)", y = "Frequency") +
        theme_minimal()
    })
    output$distribution_delivery_time <- renderPlot({
      ggplot(data, aes(x = DeliveryTime)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
        labs(x = "Delivery Time (min)", y = "Frequency") +
        theme_minimal()
    })
    output$distribution_Experience <- renderPlot({
      ggplot(data, aes(x = Experience)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
        labs(x = "Experience (years)", y = "Frequency") +
        theme_minimal()
    })

    # Orders by Category
    output$trafic_orders <- renderPlot({
      ggplot(data, aes(x = Traffic, fill = Traffic)) +
        geom_bar() +
        labs(
          title = "Number of orders for traffic",
          y = "Count of Orders",
          x = "Traffic"
        ) +
        theme_minimal()
    })
    output$weather_orders <- renderPlot({
      ggplot(data, aes(x = Weather, fill = Weather)) +
        geom_bar() +
        labs(
          title = "Number of orders for  Weather",
          y = "Count of Orders",
          x = " Weather"
        ) +
        theme_minimal()
    })
    output$time_orders <- renderPlot({
      ggplot(data, aes(x = Time, fill = Time)) +
        geom_bar() +
        labs(
          title = "Number of orders for  Time",
          y = "Count of Orders",
          x = " Time"
        ) +
        theme_minimal()
    })
    output$delivery_late_orders <- renderPlot({
      ggplot(data, aes(x = DeliveryLate, fill = DeliveryLate)) +
        geom_bar() +
        labs(
          title = "Number of orders for  Delivery Late",
          y = "Count of Orders",
          x = " Delivery Late"
        ) +
        theme_minimal()
    })

    # Correlation
    output$corr <- renderPlot({
      df <- data %>% select(Distance, DeliveryTime, Speed, PreparationTime, Experience,Rating)
      cor_matrix <- cor(df)
      ggcorrplot(cor_matrix,
        hc.order = TRUE, type = "lower",
        lab = TRUE, lab_size = 3,
        method = "circle", colors = c("blue", "white", "red")
      )
    })

    # Delivery Time Throughout Day
    output$time_by_time <- renderPlot({
      ggplot(data, aes(x = Time, y = DeliveryTime, fill = Time)) +
        geom_boxplot() +
        labs(x = "Time", y = "Delivery time (min)") +
        theme_minimal()
    })
    output$time_by_weather <- renderPlot({
      ggplot(data, aes(x = Weather, y = DeliveryTime, fill = Weather)) +
        geom_boxplot() +
        labs(x = "Weather", y = "Delivery time (min)") +
        theme_minimal()
    })
    output$time_by_traffic <- renderPlot({
      ggplot(data, aes(x = Traffic, y = DeliveryTime, fill = Traffic)) +
        geom_boxplot() +
        labs(x = "Traffic", y = "Delivery time (min)") +
        theme_minimal()
    })

    # assoucation rules
    output$rule_late <- renderPlot({
      df <- data %>% select(Traffic, Vehicle, DeliveryLate, Weather, Time)
      trans <- as(df, "transactions")
      ru <- apriori(
        trans,
        parameter = list(support = input$support, confidence = input$confidence, minlen = 2),
        appearance = list(
          rhs = grep("DeliveryLate=Late", itemLabels(trans), value = TRUE)
        )
      )
      plot(ru, method = "group", control = list(reorder = TRUE), limit = 10)
    })
    output$rule_ontime <- renderPlot({
      df <- data %>% select(Traffic, Vehicle, DeliveryLate, Weather, Time)
      trans <- as(df, "transactions")
      ru <- apriori(
        trans,
        parameter = list(support = input$support, confidence = input$confidence, minlen = 2),
        appearance = list(
          rhs = grep("DeliveryLate=Ontime", itemLabels(trans), value = TRUE)
        )
      )
      plot(ru, method = "group", control = list(reorder = TRUE), limit = 10)
    })

    # kmeans
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
      kmeans_result()$km
    })
    output$cluster_description <- renderPrint({
      km <- kmeans_result()
      
      df <- data %>% 
        select(Cluster, Distance, DeliveryTime, Speed, Experience,PreparationTime)
      
      summary_df <- df %>%
        group_by(Cluster) %>%
        summarise(
          AvgDistance = mean(Distance),
          AvgDelivery = mean(DeliveryTime),
          AvgSpeed = mean(Speed),
          AvgExp = mean(Experience),
          AvgPrep = mean(PreparationTime)
        )
      
      descriptions <- c()
      
      for (i in 1:nrow(summary_df)) {
        row <- summary_df[i, ]
        
        dist_label <- ifelse(row$AvgDistance < median(summary_df$AvgDistance), "Short Distance", "Long Distance")
        del_label <- ifelse(row$AvgDelivery < median(summary_df$AvgDelivery),"Fast Delivery", "Slow Delivery")
        speed_label <- ifelse(row$AvgSpeed > median(summary_df$AvgSpeed),"High Speed", "Low Speed")
        exp_label <- ifelse(row$AvgExp > median(summary_df$AvgExp),"Experienced Courier", "Low Experience Courier")
        prep_label <- ifelse(row$AvgPrep < median(summary_df$AvgPrep),"Fast Preparation", "Slow Preparation")
        descriptions[i] <- paste0("Cluster ", row$Cluster, ": ",dist_label, " — ",del_label, " — ",speed_label, " — ",exp_label," - ",prep_label)
      }
      
      cat(paste(descriptions, collapse = "\n\n"))
    })
    
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
