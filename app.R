# ------------------------------------------------------------------
# Sales Dashboard - Shiny + shinydashboard + plotly + leaflet
# Everything runs on dummy data generated below. No external files.
#
# Install once:
# install.packages(c("shiny","shinydashboard","plotly","leaflet","dplyr","scales"))
# Run: shiny::runApp("app.R")
# ------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(dplyr)
library(scales)

set.seed(42)

# ------------------------- DUMMY DATA -----------------------------

months <- month.abb
regions <- c("North", "South", "East", "West", "Central")
channels <- c("Online", "Retail", "Partner", "Wholesale")
devices <- c("Mobile", "Desktop", "Tablet")

# Monthly revenue & target, per region, for two years
monthly <- expand.grid(
  year   = c(2024, 2025),
  month  = factor(months, levels = months),
  region = regions,
  stringsAsFactors = FALSE
) |>
  mutate(
    revenue = round(runif(n(), 40000, 220000)),
    target  = round(revenue * runif(n(), 0.85, 1.25)),
    orders  = round(revenue / runif(n(), 700, 1400)),
    customers = round(orders * runif(n(), 0.55, 0.9))
  )

# Channel and device mix
channel_mix <- data.frame(
  channel = channels,
  value   = c(4200, 3100, 1800, 1200)
)

device_mix <- data.frame(
  device = devices,
  value  = c(58, 33, 9)
)

# City level data for the map
cities <- data.frame(
  city = c("Bhubaneswar", "Mumbai", "Delhi", "Bengaluru", "Kolkata",
           "Chennai", "Hyderabad", "Pune", "Ahmedabad", "Jaipur"),
  region = c("East", "West", "North", "South", "East",
             "South", "South", "West", "West", "North"),
  lat = c(20.2961, 19.0760, 28.6139, 12.9716, 22.5726,
          13.0827, 17.3850, 18.5204, 23.0225, 26.9124),
  lng = c(85.8245, 72.8777, 77.2090, 77.5946, 88.3639,
          80.2707, 78.4867, 73.8567, 72.5714, 75.7873),
  stringsAsFactors = FALSE
) |>
  mutate(
    revenue = round(runif(n(), 150000, 900000)),
    stores  = sample(3:25, n(), replace = TRUE)
  )

pal_main <- c("#2C7BE5", "#00B8A9", "#F6C343", "#E63757", "#6C5CE7")

# ---------------------------- UI ----------------------------------

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Sales Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Geography", tabName = "geo", icon = icon("map-marked-alt")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    selectInput("year", "Year", choices = c(2024, 2025), selected = 2025),
    checkboxGroupInput("regions", "Regions",
                       choices = regions, selected = regions)
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-box h3 { font-size: 28px; }
      .box { border-top-color: #2C7BE5; }
    "))),
    
    tabItems(
      # ----------------------- OVERVIEW -----------------------
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("kpi_revenue", width = 3),
          valueBoxOutput("kpi_orders", width = 3),
          valueBoxOutput("kpi_aov", width = 3),
          valueBoxOutput("kpi_attain", width = 3)
        ),
        fluidRow(
          box(title = "Monthly Revenue vs Target", status = "primary",
              solidHeader = TRUE, width = 8,
              plotlyOutput("line_chart", height = 330)),
          box(title = "Revenue by Region", status = "primary",
              solidHeader = TRUE, width = 4,
              plotlyOutput("bar_chart", height = 330))
        ),
        fluidRow(
          box(title = "Revenue Share by Channel", status = "primary",
              solidHeader = TRUE, width = 6,
              plotlyOutput("pie_chart", height = 320)),
          box(title = "Sessions by Device", status = "primary",
              solidHeader = TRUE, width = 6,
              plotlyOutput("donut_chart", height = 320))
        )
      ),
      
      # ----------------------- MAP ----------------------------
      tabItem(
        tabName = "geo",
        fluidRow(
          valueBoxOutput("kpi_cities", width = 4),
          valueBoxOutput("kpi_stores", width = 4),
          valueBoxOutput("kpi_top_city", width = 4)
        ),
        fluidRow(
          box(title = "Revenue by City (bubble size = revenue)",
              status = "primary", solidHeader = TRUE, width = 12,
              leafletOutput("map", height = 520))
        )
      ),
      
      # ----------------------- DATA ---------------------------
      tabItem(
        tabName = "data",
        box(title = "Filtered Records", status = "primary",
            solidHeader = TRUE, width = 12,
            tableOutput("table"))
      )
    )
  )
)

# -------------------------- SERVER --------------------------------

server <- function(input, output, session) {
  
  filtered <- reactive({
    req(input$regions)
    monthly |>
      filter(year == as.numeric(input$year), region %in% input$regions)
  })
  
  city_filtered <- reactive({
    req(input$regions)
    cities |> filter(region %in% input$regions)
  })
  
  # ------------------------ SCORE CARDS ------------------------
  output$kpi_revenue <- renderValueBox({
    valueBox(
      dollar(sum(filtered()$revenue), prefix = "$", big.mark = ","),
      "Total Revenue", icon = icon("sack-dollar"), color = "blue"
    )
  })
  
  output$kpi_orders <- renderValueBox({
    valueBox(
      comma(sum(filtered()$orders)),
      "Orders", icon = icon("cart-shopping"), color = "teal"
    )
  })
  
  output$kpi_aov <- renderValueBox({
    aov_val <- sum(filtered()$revenue) / max(sum(filtered()$orders), 1)
    valueBox(
      dollar(round(aov_val), prefix = "$", big.mark = ","),
      "Avg Order Value", icon = icon("receipt"), color = "yellow"
    )
  })
  
  output$kpi_attain <- renderValueBox({
    att <- sum(filtered()$revenue) / max(sum(filtered()$target), 1)
    valueBox(
      percent(att, accuracy = 0.1),
      "Target Attainment", icon = icon("bullseye"),
      color = if (att >= 1) "green" else "red"
    )
  })
  
  output$kpi_cities <- renderValueBox({
    valueBox(nrow(city_filtered()), "Cities Covered",
             icon = icon("city"), color = "blue")
  })
  
  output$kpi_stores <- renderValueBox({
    valueBox(sum(city_filtered()$stores), "Total Stores",
             icon = icon("store"), color = "teal")
  })
  
  output$kpi_top_city <- renderValueBox({
    d <- city_filtered()
    top <- if (nrow(d)) d$city[which.max(d$revenue)] else "-"
    valueBox(top, "Top City by Revenue",
             icon = icon("trophy"), color = "yellow")
  })
  
  # ------------------------- LINE CHART ------------------------
  output$line_chart <- renderPlotly({
    d <- filtered() |>
      group_by(month) |>
      summarise(revenue = sum(revenue), target = sum(target), .groups = "drop")
    
    plot_ly(d, x = ~month) |>
      add_trace(y = ~revenue, name = "Revenue", type = "scatter",
                mode = "lines+markers",
                line = list(color = pal_main[1], width = 3),
                marker = list(size = 7)) |>
      add_trace(y = ~target, name = "Target", type = "scatter",
                mode = "lines",
                line = list(color = pal_main[4], width = 2, dash = "dash")) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Revenue ($)"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.15)
      )
  })
  
  # -------------------------- BAR CHART ------------------------
  output$bar_chart <- renderPlotly({
    d <- filtered() |>
      group_by(region) |>
      summarise(revenue = sum(revenue), .groups = "drop") |>
      arrange(revenue)
    
    plot_ly(d, x = ~revenue, y = ~reorder(region, revenue),
            type = "bar", orientation = "h",
            marker = list(color = pal_main[seq_len(nrow(d))]),
            hovertemplate = "%{y}: $%{x:,.0f}<extra></extra>") |>
      layout(xaxis = list(title = "Revenue ($)"),
             yaxis = list(title = ""))
  })
  
  # -------------------------- PIE CHART ------------------------
  output$pie_chart <- renderPlotly({
    plot_ly(channel_mix, labels = ~channel, values = ~value,
            type = "pie",
            textinfo = "label+percent",
            marker = list(colors = pal_main,
                          line = list(color = "#fff", width = 2))) |>
      layout(showlegend = TRUE)
  })
  
  # ------------------------- DONUT CHART -----------------------
  output$donut_chart <- renderPlotly({
    plot_ly(device_mix, labels = ~device, values = ~value,
            type = "pie", hole = 0.6,
            textinfo = "label+percent",
            marker = list(colors = pal_main[c(2, 1, 3)],
                          line = list(color = "#fff", width = 2))) |>
      layout(
        showlegend = TRUE,
        annotations = list(
          text = "Devices", showarrow = FALSE,
          font = list(size = 16), x = 0.5, y = 0.5
        )
      )
  })
  
  # ----------------------------- MAP ---------------------------
  output$map <- renderLeaflet({
    d <- city_filtered()
    validate(need(nrow(d) > 0, "No cities for the selected regions."))
    
    pal <- colorNumeric("Blues", domain = d$revenue)
    
    leaflet(d) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~scales::rescale(revenue, to = c(8, 28)),
        color = "#ffffff", weight = 1,
        fillColor = ~pal(revenue), fillOpacity = 0.85,
        label = ~city,
        popup = ~paste0("<b>", city, "</b><br>",
                        "Region: ", region, "<br>",
                        "Revenue: $", format(revenue, big.mark = ","), "<br>",
                        "Stores: ", stores)
      ) |>
      addLegend("bottomright", pal = pal, values = ~revenue,
                title = "Revenue ($)", opacity = 0.9)
  })
  
  # ---------------------------- TABLE --------------------------
  output$table <- renderTable({
    filtered() |>
      group_by(region, month) |>
      summarise(revenue = sum(revenue), orders = sum(orders),
                .groups = "drop") |>
      head(30)
  })
}

shinyApp(ui, server)