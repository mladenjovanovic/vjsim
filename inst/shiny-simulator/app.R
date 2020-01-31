library(shiny)
library(plotly)
library(tidyverse)
library(vjsim)

# -----------------------------------------------
# User Interface
ui <- navbarPage(
  "Vertical Jump Simulator",

  # ----------------------------
  tabPanel("About"),

  # ----------------------------
  tabPanel(
    "Simulator",
    sidebarPanel(actionButton("calculate", "Calculate"),
      h4("Bodyweight"),
      sliderInput("athlete1_BW", "Athlete 1", value = 75, min = 65, max = 100, ticks = FALSE, step = 1),
      sliderInput("athlete2_BW", "Athlete 2", value = 75, min = 65, max = 100, ticks = FALSE, step = 1),
      hr(),
      h4("Push-off Distance"),
      sliderInput("athlete1_push_off_distance", "Athlete 1", value = 0.4, min = 0.3, max = 0.6, ticks = FALSE, step = 0.01),
      sliderInput("athlete2_push_off_distance", "Athlete 2", value = 0.4, min = 0.3, max = 0.6, ticks = FALSE, step = 0.01),
      hr(),
      h4("Maximal Force"),
      sliderInput("athlete1_max_force", "Athlete 1", value = 3000, min = 2000, max = 5000, ticks = FALSE, step = 100),
      sliderInput("athlete2_max_force", "Athlete 2", value = 3000, min = 2000, max = 5000, ticks = FALSE, step = 100),
      hr(),
      h4("Maximal Velocity"),
      sliderInput("athlete1_max_velocity", "Athlete 1", value = 4, min = 3, max = 8, ticks = FALSE, step = 0.1),
      sliderInput("athlete2_max_velocity", "Athlete 2", value = 4, min = 3, max = 8, ticks = FALSE, step = 0.1),
      hr(),
      h4("Force Length Characteristic (Decline Rate)"),
      sliderInput("athlete1_decline_rate", "Athlete 1", value = 1, min = 0, max = 1.5, ticks = FALSE, step = 0.01),
      sliderInput("athlete2_decline_rate", "Athlete 2", value = 1, min = 0, max = 1.5, ticks = FALSE, step = 0.01),
      hr(),
      h4("Force Length Characteristic (Peak Location)"),
      sliderInput("athlete1_peak_location", "Athlete 1", value = -0.05, max = -0.01, min = -0.2, ticks = FALSE, step = 0.01),
      sliderInput("athlete2_peak_location", "Athlete 2", value = -0.05, max = -0.01, min = -0.2, ticks = FALSE, step = 0.01),
      hr(),
      h4("Time to Max Activation"),
      sliderInput("athlete1_time_to_max_activation", "Athlete 1", value = 0.2, min = 0.1, max = 0.5, ticks = FALSE, step = 0.01),
      sliderInput("athlete2_time_to_max_activation", "Athlete 2", value = 0.2, min = 0.1, max = 0.5, ticks = FALSE, step = 0.01),
      width = 3
    ),

    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel(
        "Force Generator",
        br(),
        h4("Force-Length Characteristic"),
        checkboxInput("use_distance_to_take_off", "Use distance to take off?", value = FALSE),
        plotlyOutput("force_length_characteristic"),
        br(),
        h4("Force-Time Characteristic"),
        plotlyOutput("force_time_characteristic"),
        br(),
        h4("Force-Velocity Characteristic"),
        plotlyOutput("force_velocity_characteristic")
      )
    ))
  ),
  # ----------------------------
  tabPanel("Explorer")
)

# -----------------------------------------------
# Server
server <- function(input, output) {
   # ---------------------------------------------
   # Reactive parameters
   # These response when CALCULATE is pushed

    # Bodyweight
    athlete1_BW <- eventReactive(input$calculate, {
        return(input$athlete1_BW)
    }, ignoreNULL = FALSE)

    athlete2_BW <- eventReactive(input$calculate, {
        return(input$athlete2_BW)
    }, ignoreNULL = FALSE)

    # Push-off distance
    athlete1_push_off_distance <- eventReactive(input$calculate, {
        return(input$athlete1_push_off_distance )
    }, ignoreNULL = FALSE)

    athlete2_push_off_distance  <- eventReactive(input$calculate, {
        return(input$athlete2_push_off_distance )
    }, ignoreNULL = FALSE)

    # Max-Force
    athlete1_max_force <- eventReactive(input$calculate, {
        return(input$athlete1_max_force)
    }, ignoreNULL = FALSE)

    athlete2_max_force <- eventReactive(input$calculate, {
        return(input$athlete2_max_force)
    }, ignoreNULL = FALSE)

    # Max Velocity
    athlete1_max_velocity <- eventReactive(input$calculate, {
        return(input$athlete1_max_velocity)
    }, ignoreNULL = FALSE)

    athlete2_max_velocity  <- eventReactive(input$calculate, {
        return(input$athlete2_max_velocity)
    }, ignoreNULL = FALSE)

    # Decline Rate
    athlete1_decline_rate <- eventReactive(input$calculate, {
        return(input$athlete1_decline_rate)
    }, ignoreNULL = FALSE)

    athlete2_decline_rate  <- eventReactive(input$calculate, {
        return(input$athlete2_decline_rate)
    }, ignoreNULL = FALSE)


    # Peak Location
    athlete1_peak_location <- eventReactive(input$calculate, {
        return(input$athlete1_peak_location)
    }, ignoreNULL = FALSE)

    athlete2_peak_location  <- eventReactive(input$calculate, {
        return(input$athlete2_peak_location)
    }, ignoreNULL = FALSE)

    # Time to max activation
    athlete1_time_to_max_activation <- eventReactive(input$calculate, {
        return(input$athlete1_time_to_max_activation)
    }, ignoreNULL = FALSE)

    athlete2_time_to_max_activation  <- eventReactive(input$calculate, {
        return(input$athlete2_time_to_max_activation)
    }, ignoreNULL = FALSE)

    # -----------------------------------------------------
    # Simulator

    # -----------------------------
    # Force Generator panel

    # ---------
    output$force_length_characteristic <- renderPlotly({
        current_distance <- seq(0, 0.6, length.out = 1000)

        athlete1_DF <- data.frame(
            current_distance = current_distance,
            distance_perc = current_distance / input$athlete1_push_off_distance,
            distance_to_take_off = current_distance - input$athlete1_push_off_distance,
            force_perc = vjsim::fgen_get_force_percentage(
                current_distance = current_distance,
                push_off_distance = input$athlete1_push_off_distance,
                decline_rate = input$athlete1_decline_rate,
                peak_location = input$athlete1_peak_location
                )
        )
        athlete1_DF$force <- athlete1_DF$force_perc * input$athlete1_max_force

        athlete2_DF <- data.frame(
          current_distance = current_distance,
          distance_perc = current_distance / input$athlete2_push_off_distance,
          distance_to_take_off = current_distance - input$athlete2_push_off_distance,
          force_perc = vjsim::fgen_get_force_percentage(
            current_distance = current_distance,
            push_off_distance = input$athlete2_push_off_distance,
            decline_rate = input$athlete2_decline_rate,
            peak_location = input$athlete2_peak_location
          )
        )
        athlete2_DF$force <- athlete2_DF$force_perc * input$athlete2_max_force



      if (input$use_distance_to_take_off == TRUE) {
        x_label <-  "Distance to take-off (m)"

        athlete1_DF$x <- athlete1_DF$distance_to_take_off

        athlete2_DF$x <- athlete2_DF$distance_to_take_off
      } else {
        x_label <-  "Distance (m)"

        athlete1_DF$x <- athlete1_DF$current_distance

        athlete2_DF$x <- athlete2_DF$current_distance
      }

      gg <- plot_ly() %>%
            add_lines(data = athlete1_DF, x = ~x, y = ~force,
                      name = "Athlete 1", line = list(color = '#5DA5DA'),
                      hoverinfo = "text",
                      text = ~paste("Athlete 1\n",
                                    "Distance =", round(current_distance, 2), "m\n",
                                    "Distance to take-off =", round(distance_to_take_off, 2), "m\n",
                                    "Distance =", round(distance_perc * 100, 0), "%\n",
                                    "Force =", round(force_perc * 100, 0), "%\n",
                                    "Force =", round(force, 0), "N\n")) %>%
            add_lines(data = athlete2_DF, x = ~x, y = ~force,
                      name = "Athlete 2", line = list(color = '#FAA43A'),
                      hoverinfo = "text",
                      text = ~paste("Athlete 2\n",
                                    "Distance =", round(current_distance, 2), "m\n",
                                    "Distance to take-off =", round(distance_to_take_off, 2), "m\n",
                                    "Distance =", round(distance_perc * 100, 0), "%\n",
                                    "Force =", round(force_perc * 100, 0), "%\n",
                                    "Force =", round(force, 0), "N\n")) %>%
            layout(showlegend = FALSE,
                   yaxis = list(side = 'left', title = "Potential Force (N)",
                                showgrid = TRUE, zeroline = TRUE),
                   xaxis = list(side = 'left', title = x_label,
                                showgrid = TRUE, zeroline = TRUE))

        return(gg)
    })
    # -------------
    output$force_time_characteristic <- renderPlotly({
        current_time = seq(0, 0.5, length.out = 10000)

        athlete1_DF <- data.frame(
            current_time = current_time,
            activation = vjsim::fgen_get_activation(
                current_time = current_time,
                initial_activation = (input$athlete1_BW * 9.81) / input$athlete1_max_force,
                time_to_max_activation = input$athlete1_time_to_max_activation
            )
        )
        athlete1_DF$force <- athlete1_DF$activation * input$athlete1_max_force

        athlete2_DF <- data.frame(
            current_time = current_time,
            activation = vjsim::fgen_get_activation(
                current_time = current_time,
                initial_activation = (input$athlete2_BW * 9.81) / input$athlete2_max_force,
                time_to_max_activation = input$athlete2_time_to_max_activation
            )
        )
        athlete2_DF$force <- athlete2_DF$activation * input$athlete2_max_force

        gg <- plot_ly() %>%
            add_lines(data = athlete1_DF, x = ~current_time, y = ~force,
                      name = "Athlete 1", line = list(color = '#5DA5DA'),
                      hoverinfo = "text",
                      text = ~paste("Athlete 1\n",
                                    "Activation =", round(activation * 100, 0), "%\n",
                                    "Time =", round(current_time, 2), "s\n",
                                    "Force =", round(force, 0), "N\n")) %>%
            add_lines(data = athlete2_DF, x = ~current_time, y = ~force,
                      name = "Athlete 2", line = list(color = '#FAA43A'),
                      hoverinfo = "text",
                      text = ~paste("Athlete 2\n",
                                    "Activation =", round(activation * 100, 0), "%\n",
                                    "Time =", round(current_time, 2), "s\n",
                                    "Force =", round(force, 0), "N\n")) %>%
            layout(showlegend = FALSE,
                   yaxis = list(side = 'left', title = "Generated Force (N)",
                                showgrid = TRUE, zeroline = TRUE),
                   xaxis = list(side = 'left', title = "Time (s)",
                                showgrid = TRUE, zeroline = TRUE))

        return(gg)
    })

    # -------------
    output$force_velocity_characteristic <- renderPlotly({
        external_force = seq(0, input$athlete1_max_force, length.out = 1000)

        athlete1_DF <- data.frame(
            external_force = external_force,
            velocity = vjsim::fgen_get_velocity(
                external_force = external_force,
                max_force = input$athlete1_max_force,
                max_velocity = input$athlete1_max_velocity
            )
        )

        external_force = seq(0, input$athlete2_max_force, length.out = 1000)

        athlete2_DF <- data.frame(
            external_force = external_force,
            velocity = vjsim::fgen_get_velocity(
                external_force = external_force,
                max_force = input$athlete2_max_force,
                max_velocity = input$athlete2_max_velocity
            )
        )

        gg <- plot_ly() %>%
            add_lines(data = athlete1_DF, x = ~external_force, y = ~velocity,
                      name = "Athlete 1", line = list(color = '#5DA5DA'),
                      hoverinfo = "text",
                      text = ~paste("Athlete 1\n",
                                    "External Force =", round(external_force, 0), "N\n",
                                    "Max Velocity =", round(velocity, 2), "m/s\n")) %>%
            add_lines(data = athlete2_DF, x = ~external_force, y = ~velocity,
                      name = "Athlete 2", line = list(color = '#FAA43A'),
                      hoverinfo = "text",
                      text = ~paste("Athlete 2\n",
                                    "External Force =", round(external_force, 0), "N\n",
                                    "Max Velocity =", round(velocity, 2), "m/s\n")) %>%
            layout(showlegend = FALSE,
                   yaxis = list(side = 'left', title = "Max Velocity (m/s)",
                                showgrid = TRUE, zeroline = TRUE),
                   xaxis = list(side = 'left', title = "External Force (s)",
                                showgrid = TRUE, zeroline = TRUE))

        return(gg)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
