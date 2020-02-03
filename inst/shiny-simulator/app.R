library(shiny)
library(plotly)
library(tidyverse)
library(vjsim)
library(DT)

# ----------------------------------------------
# Constants
gravity_const <- 9.81
time_step <- 0.001
fgen_length_out <- 1000

trace_variables <- c(
  "time",
  "distance",
  "velocity",
  "distance_to_take_off",
  "push_off_perc",
  "force_percentage",
  "potential_force",
  "activation",
  "generated_force",
  "viscous_force",
  "ground_reaction_force",
  "propulsive_force",
  "acceleration",
  "power",
  "RFD",
  "RPD"
)

summary_variables <- c(
  "take_off_time",
  "take_off_velocity",
  "height",
  "mean_GRF_over_distance",
  "mean_GRF_over_time",
  "peak_GRF",
  "mean_velocity",
  "peak_velocity",
  "mean_power",
  "peak_power",
  "peak_RFD",
  "peak_RPD",
  "work_done",
  "impulse"
)

parameter_variables <- c(
  "mass",
  "push_off_distance",
  "max_force",
  "max_velocity",
  "time_to_max_activation"
)

vj_probing_change <- seq(0.9, 1.1, length.out = 7)

# Function to extract one column from the probing data
get_metric_sensitivity <- function(probing_data, variable, invert = FALSE) {
  df <- data.frame(
    change_ratio = probing_data$change_ratio,
    probing = probing_data$probing,
    variable = probing_data[[variable]]
  )

  if (invert) {
    df$change_ratio <- 1 / df$change_ratio
  }

  return(df)
}

# Function to extract one parameter
get_parameter_sensitivity <- function(probing_data, parameter, invert = FALSE) {
  probing_data <- probing_data[probing_data$probing == parameter, ]

  if (invert) {
    probing_data$change_ratio <- 1 / df$change_ratio
  }

  df <- gather(probing_data, key = "variable", value = "value", -(1:13)) %>%
    filter(
      variable %in% summary_variables
    )

  return(df)
}

# -----------------------------------------------
# User Interface
ui <- navbarPage(
  "Vertical Jump Simulator",

  # ----------------------------
  # tabPanel("About"),

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
      ),
      tabPanel(
        "Jump Analysis",
        br(),
        fixedRow(
          column(
            4,
            selectInput(
              inputId = "jump_kinetics_x_var",
              label = "X axis",
              choices = trace_variables,
              selected = "time"
            )
          ),
          column(
            4,
            selectInput(
              inputId = "jump_kinetics_y_var",
              label = "Y axis",
              choices = trace_variables,
              selected = "velocity"
            )
          )
        ),
        br(),
        plotlyOutput("jump_trace"),
        br(),
        h4("Jump summary"),
        dataTableOutput("jump_summary_table"),
        br(),
        h4("Raw trace"),
        br(),
        h5("Athlete 1"),
        dataTableOutput("athlete1_jump_trace"),
        br(),
        h5("Athlete 2"),
        dataTableOutput("athlete2_jump_trace")
      ),
      tabPanel(
        "Jump Sensitivity",
        br(),
        selectInput(
          inputId = "probing_variable",
          label = "Probing summary variable",
          choices = summary_variables,
          selected = "height"
        ),
        fixedRow(
          column(
            6,
            h4("Athlete 1"),
            plotlyOutput("athlete1_jump_probing")
          ),
          column(
            6,
            h4("Athlete 2"),
            plotlyOutput("athlete2_jump_probing")
          )
        ),
        br(),
        selectInput(
          inputId = "parameter_variable",
          label = "Probing Force Generator parameter",
          choices = parameter_variables,
          selected = "push_off_distance"
        ),
        fixedRow(
          column(
            6,
            h4("Athlete 1"),
            plotlyOutput("athlete1_parameter_probing")
          ),
          column(
            6,
            h4("Athlete 2"),
            plotlyOutput("athlete2_parameter_probing")
          )
        )
      )
    )),
    # ----------------------------
    tabPanel("Explorer")
  )
)

# -----------------------------------------------
# Server
server <- function(input, output) {
  # ---------------------------------------------
  # Reactive parameters
  # These response when CALCULATE is pushed

  # Bodyweight
  athlete1_BW <- eventReactive(input$calculate,
    {
      return(input$athlete1_BW)
    },
    ignoreNULL = FALSE
  )

  athlete2_BW <- eventReactive(input$calculate,
    {
      return(input$athlete2_BW)
    },
    ignoreNULL = FALSE
  )

  # Push-off distance
  athlete1_push_off_distance <- eventReactive(input$calculate,
    {
      return(input$athlete1_push_off_distance)
    },
    ignoreNULL = FALSE
  )

  athlete2_push_off_distance <- eventReactive(input$calculate,
    {
      return(input$athlete2_push_off_distance)
    },
    ignoreNULL = FALSE
  )

  # Max-Force
  athlete1_max_force <- eventReactive(input$calculate,
    {
      return(input$athlete1_max_force)
    },
    ignoreNULL = FALSE
  )

  athlete2_max_force <- eventReactive(input$calculate,
    {
      return(input$athlete2_max_force)
    },
    ignoreNULL = FALSE
  )

  # Max Velocity
  athlete1_max_velocity <- eventReactive(input$calculate,
    {
      return(input$athlete1_max_velocity)
    },
    ignoreNULL = FALSE
  )

  athlete2_max_velocity <- eventReactive(input$calculate,
    {
      return(input$athlete2_max_velocity)
    },
    ignoreNULL = FALSE
  )

  # Decline Rate
  athlete1_decline_rate <- eventReactive(input$calculate,
    {
      return(input$athlete1_decline_rate)
    },
    ignoreNULL = FALSE
  )

  athlete2_decline_rate <- eventReactive(input$calculate,
    {
      return(input$athlete2_decline_rate)
    },
    ignoreNULL = FALSE
  )


  # Peak Location
  athlete1_peak_location <- eventReactive(input$calculate,
    {
      return(input$athlete1_peak_location)
    },
    ignoreNULL = FALSE
  )

  athlete2_peak_location <- eventReactive(input$calculate,
    {
      return(input$athlete2_peak_location)
    },
    ignoreNULL = FALSE
  )

  # Time to max activation
  athlete1_time_to_max_activation <- eventReactive(input$calculate,
    {
      return(input$athlete1_time_to_max_activation)
    },
    ignoreNULL = FALSE
  )

  athlete2_time_to_max_activation <- eventReactive(input$calculate,
    {
      return(input$athlete2_time_to_max_activation)
    },
    ignoreNULL = FALSE
  )

  # --------------
  # Jump trace reactive element
  athlete1_jump_trace <- eventReactive(input$calculate,
    {
      jump_trace <- vj_simulate(
        mass = athlete1_BW(),
        weight = athlete1_BW() * gravity_const,
        push_off_distance = athlete1_push_off_distance(),
        gravity_const = gravity_const,
        time_step = time_step,
        save_trace = TRUE,
        max_force = athlete1_max_force(),
        max_velocity = athlete1_max_velocity(),
        decline_rate = athlete1_decline_rate(),
        peak_location = athlete1_peak_location(),
        time_to_max_activation = athlete1_time_to_max_activation()
      )
      return(jump_trace)
    },
    ignoreNULL = FALSE
  )

  athlete2_jump_trace <- eventReactive(input$calculate,
    {
      jump_trace <- vj_simulate(
        mass = athlete2_BW(),
        weight = athlete2_BW() * gravity_const,
        push_off_distance = athlete2_push_off_distance(),
        gravity_const = gravity_const,
        time_step = time_step,
        save_trace = TRUE,
        max_force = athlete2_max_force(),
        max_velocity = athlete2_max_velocity(),
        decline_rate = athlete2_decline_rate(),
        peak_location = athlete2_peak_location(),
        time_to_max_activation = athlete2_time_to_max_activation()
      )
      return(jump_trace)
    },
    ignoreNULL = FALSE
  )

  # Jump probe reactive element
  athlete1_jump_probe <- eventReactive(input$calculate,
    {
      jump_probe_raw <- probe_vj(
        mass = athlete1_BW(),
        push_off_distance = athlete1_push_off_distance(),
        max_force = athlete1_max_force(),
        max_velocity = athlete1_max_velocity(),
        time_to_max_activation = athlete1_time_to_max_activation(),
        change_ratio = vj_probing_change,
        aggregate = "raw",

        # Extra params
        weight = athlete1_BW() * gravity_const,
        gravity_const = gravity_const,
        time_step = time_step,
        decline_rate = athlete1_decline_rate(),
        peak_location = athlete1_peak_location()
      )

      jump_probe_ratio <- probe_vj(
        mass = athlete1_BW(),
        push_off_distance = athlete1_push_off_distance(),
        max_force = athlete1_max_force(),
        max_velocity = athlete1_max_velocity(),
        time_to_max_activation = athlete1_time_to_max_activation(),
        change_ratio = vj_probing_change,
        aggregate = "ratio",

        # Extra params
        weight = athlete1_BW() * gravity_const,
        gravity_const = gravity_const,
        time_step = time_step,
        decline_rate = athlete1_decline_rate(),
        peak_location = athlete1_peak_location()
      )

      return(list(raw = jump_probe_raw, ratio = jump_probe_ratio))
    },
    ignoreNULL = FALSE
  )

  athlete2_jump_probe <- eventReactive(input$calculate,
    {
      jump_probe_raw <- probe_vj(
        mass = athlete2_BW(),
        push_off_distance = athlete2_push_off_distance(),
        max_force = athlete2_max_force(),
        max_velocity = athlete2_max_velocity(),
        time_to_max_activation = athlete2_time_to_max_activation(),
        change_ratio = vj_probing_change,
        aggregate = "raw",

        # Extra params
        weight = athlete2_BW() * gravity_const,
        gravity_const = gravity_const,
        time_step = time_step,
        decline_rate = athlete2_decline_rate(),
        peak_location = athlete2_peak_location()
      )

      jump_probe_ratio <- probe_vj(
        mass = athlete2_BW(),
        push_off_distance = athlete2_push_off_distance(),
        max_force = athlete2_max_force(),
        max_velocity = athlete2_max_velocity(),
        time_to_max_activation = athlete2_time_to_max_activation(),
        change_ratio = vj_probing_change,
        aggregate = "ratio",

        # Extra params
        weight = athlete2_BW() * gravity_const,
        gravity_const = gravity_const,
        time_step = time_step,
        decline_rate = athlete2_decline_rate(),
        peak_location = athlete2_peak_location()
      )

      return(list(raw = jump_probe_raw, ratio = jump_probe_ratio))
    },
    ignoreNULL = FALSE
  )

  # -----------------------------------------------------
  # Simulator

  # -----------------------------
  # Force Generator panel

  # ---------
  output$force_length_characteristic <- renderPlotly({
    current_distance <- seq(0, 0.6, length.out = fgen_length_out)

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
      x_label <- "Distance to take-off (m)"

      athlete1_DF$x <- athlete1_DF$distance_to_take_off

      athlete2_DF$x <- athlete2_DF$distance_to_take_off
    } else {
      x_label <- "Distance (m)"

      athlete1_DF$x <- athlete1_DF$current_distance

      athlete2_DF$x <- athlete2_DF$current_distance
    }

    gg <- plot_ly() %>%
      add_lines(
        data = athlete1_DF, x = ~x, y = ~force,
        name = "Athlete 1", line = list(color = "#5DA5DA"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 1\n",
          "Distance =", round(current_distance, 2), "m\n",
          "Distance to take-off =", round(distance_to_take_off, 2), "m\n",
          "Distance =", round(distance_perc * 100, 0), "%\n",
          "Force =", round(force_perc * 100, 0), "%\n",
          "Force =", round(force, 0), "N\n"
        )
      ) %>%
      add_lines(
        data = athlete2_DF, x = ~x, y = ~force,
        name = "Athlete 2", line = list(color = "#FAA43A"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 2\n",
          "Distance =", round(current_distance, 2), "m\n",
          "Distance to take-off =", round(distance_to_take_off, 2), "m\n",
          "Distance =", round(distance_perc * 100, 0), "%\n",
          "Force =", round(force_perc * 100, 0), "%\n",
          "Force =", round(force, 0), "N\n"
        )
      ) %>%
      layout(
        showlegend = FALSE,
        yaxis = list(
          side = "left", title = "Potential Force (N)",
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = x_label,
          showgrid = TRUE, zeroline = TRUE
        )
      )

    return(gg)
  })
  # -------------
  output$force_time_characteristic <- renderPlotly({
    current_time <- seq(0, 0.5, length.out = fgen_length_out)

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
      add_lines(
        data = athlete1_DF, x = ~current_time, y = ~force,
        name = "Athlete 1", line = list(color = "#5DA5DA"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 1\n",
          "Activation =", round(activation * 100, 0), "%\n",
          "Time =", round(current_time, 2), "s\n",
          "Force =", round(force, 0), "N\n"
        )
      ) %>%
      add_lines(
        data = athlete2_DF, x = ~current_time, y = ~force,
        name = "Athlete 2", line = list(color = "#FAA43A"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 2\n",
          "Activation =", round(activation * 100, 0), "%\n",
          "Time =", round(current_time, 2), "s\n",
          "Force =", round(force, 0), "N\n"
        )
      ) %>%
      layout(
        showlegend = FALSE,
        yaxis = list(
          side = "left", title = "Generated Force (N)",
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = "Time (s)",
          showgrid = TRUE, zeroline = TRUE
        )
      )

    return(gg)
  })

  # -------------
  output$force_velocity_characteristic <- renderPlotly({
    external_force <- seq(0, input$athlete1_max_force, length.out = fgen_length_out)

    athlete1_DF <- data.frame(
      external_force = external_force,
      velocity = vjsim::fgen_get_velocity(
        external_force = external_force,
        max_force = input$athlete1_max_force,
        max_velocity = input$athlete1_max_velocity
      )
    )

    external_force <- seq(0, input$athlete2_max_force, length.out = fgen_length_out)

    athlete2_DF <- data.frame(
      external_force = external_force,
      velocity = vjsim::fgen_get_velocity(
        external_force = external_force,
        max_force = input$athlete2_max_force,
        max_velocity = input$athlete2_max_velocity
      )
    )

    gg <- plot_ly() %>%
      add_lines(
        data = athlete1_DF, x = ~external_force, y = ~velocity,
        name = "Athlete 1", line = list(color = "#5DA5DA"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 1\n",
          "External Force =", round(external_force, 0), "N\n",
          "Max Velocity =", round(velocity, 2), "m/s\n"
        )
      ) %>%
      add_lines(
        data = athlete2_DF, x = ~external_force, y = ~velocity,
        name = "Athlete 2", line = list(color = "#FAA43A"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 2\n",
          "External Force =", round(external_force, 0), "N\n",
          "Max Velocity =", round(velocity, 2), "m/s\n"
        )
      ) %>%
      layout(
        showlegend = FALSE,
        yaxis = list(
          side = "left", title = "Max Velocity (m/s)",
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = "External Force (s)",
          showgrid = TRUE, zeroline = TRUE
        )
      )

    return(gg)
  })


  # ----------------------------------------------
  # Jump trace
  output$jump_trace <- renderPlotly({
    withProgress(message = "Jump trace", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_trace <- athlete1_jump_trace()$trace

      incProgress(1, detail = "Athlete 2")
      athlete2_trace <- athlete2_jump_trace()$trace
    })


    athlete1_trace$x_var <- athlete1_trace[, input$jump_kinetics_x_var]
    athlete1_trace$y_var <- athlete1_trace[, input$jump_kinetics_y_var]

    athlete2_trace$x_var <- athlete2_trace[, input$jump_kinetics_x_var]
    athlete2_trace$y_var <- athlete2_trace[, input$jump_kinetics_y_var]


    gg <- plot_ly() %>%
      add_lines(
        data = athlete1_trace, x = ~x_var, y = ~y_var,
        name = "Athlete 1", line = list(color = "#5DA5DA"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 1\n",
          "Mass =", round(mass, 0), "kg\n",
          "Weight =", round(weight, 0), "N\n",
          "Time =", round(time, 2), "s\n",
          "Distance =", round(distance, 2), "m\n",
          "Velocity =", round(velocity, 2), "m/s\n",
          "Distance to take-off =", round(distance_to_take_off, 2), "m\n",
          "Distance =", round(push_off_perc * 100, 0), "%\n",
          "Force percentage =", round(force_percentage * 100, 0), "%\n",
          "Potential force =", round(potential_force, 0), "N\n",
          "Activation =", round(activation * 100, 0), "%\n",
          "Generated force =", round(generated_force, 0), "N\n",
          "Viscous force =", round(viscous_force, 0), "N\n",
          "Ground reaction force =", round(ground_reaction_force, 0), "N\n",
          "Propulsive force =", round(propulsive_force, 0), "N\n",
          "Acceleration =", round(acceleration, 2), "m/s^2\n",
          "Power =", round(power, 0), "W\n",
          "RFD =", round(RFD, 0), "N/s\n",
          "RPD =", round(RPD, 0), "W/s\n"
        )
      ) %>%
      add_lines(
        data = athlete2_trace, x = ~x_var, y = ~y_var,
        name = "Athlete 2", line = list(color = "#FAA43A"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 2\n",
          "Mass =", round(mass, 0), "kg\n",
          "Weight =", round(weight, 0), "N\n",
          "Time =", round(time, 2), "s\n",
          "Distance =", round(distance, 2), "m\n",
          "Velocity =", round(velocity, 2), "m/s\n",
          "Distance to take-off =", round(distance_to_take_off, 2), "m\n",
          "Distance =", round(push_off_perc * 100, 0), "%\n",
          "Force percentage =", round(force_percentage * 100, 0), "%\n",
          "Potential force =", round(potential_force, 0), "N\n",
          "Activation =", round(activation * 100, 0), "%\n",
          "Generated force =", round(generated_force, 0), "N\n",
          "Viscous force =", round(viscous_force, 0), "N\n",
          "Ground reaction force =", round(ground_reaction_force, 0), "N\n",
          "Propulsive force =", round(propulsive_force, 0), "N\n",
          "Acceleration =", round(acceleration, 2), "m/s^2\n",
          "Power =", round(power, 0), "W\n",
          "RFD =", round(RFD, 0), "N/s\n",
          "RPD =", round(RPD, 0), "W/s\n"
        )
      ) %>%
      layout(
        showlegend = FALSE,
        yaxis = list(
          side = "left", title = input$jump_kinetics_y_var,
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = input$jump_kinetics_x_var,
          showgrid = TRUE, zeroline = TRUE
        )
      )

    return(gg)
  })

  # Summary table
  output$jump_summary_table <- renderDataTable({
    withProgress(message = "Jump summary", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_summary <- athlete1_jump_trace()$summary

      incProgress(1, detail = "Athlete 2")
      athlete2_summary <- athlete2_jump_trace()$summary
    })

    df <- rbind(
      data.frame(round(athlete1_summary, 2)),
      data.frame(round(athlete2_summary, 2))
    )
    rownames(df) <- c("Athlete 1", "Athlete 2")
    df <- t(df)
    df <- datatable(df)
    return(df)
  })

  # Trace table
  output$athlete1_jump_trace <- renderDataTable({
    withProgress(message = "Jump trace table", value = 0, {
      incProgress(1, detail = "Athlete 1")
      athlete1_trace <- athlete1_jump_trace()$trace
    })

    df <- datatable(athlete1_trace, rownames = FALSE) %>%
      formatRound(columns = 1:ncol(athlete1_trace), digits = 3)
    return(df)
  })

  output$athlete2_jump_trace <- renderDataTable({
    withProgress(message = "Jump trace table", value = 0, {
      incProgress(1, detail = "Athlete 2")
      athlete2_trace <- athlete2_jump_trace()$trace
    })

    df <- datatable(athlete2_trace, rownames = FALSE, options = list(digits = 2)) %>%
      formatRound(columns = 1:ncol(athlete2_trace), digits = 3)
    return(df)
  })


  # Jump probing

  output$athlete1_jump_probing <- renderPlotly({
    withProgress(message = "Jump probing", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_probing <- athlete1_jump_probe()$raw
      incProgress(1, detail = "Athlete 1")
    })

    # Convert
    plot_data <- get_metric_sensitivity(athlete1_probing, input$probing_variable)

    gg <- plot_ly() %>%
      add_lines(
        data = plot_data, x = ~change_ratio, y = ~variable,
        name = ~probing, color = ~probing,
        line = list(
          color = c(
            "mass" = "#4D4D4D",
            "max_force" = "#5DA5DA",
            "max_velocity" =  "#FAA43A",
            "push_off_distance" = "#60BD68",
            "time_to_max_activation" = "#B276B2"
          )
        ),
        hoverinfo = "text",
        text = ~ paste(
          probing, "\n",
          "Normalized change =", round(change_ratio, 2), "\n",
          input$probing_variable, "=", round(variable, 2), "\n"
        )
      ) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(
          side = "left", title = input$probing_variable,
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = "Normalized parameter change",
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })


  output$athlete2_jump_probing <- renderPlotly({
    withProgress(message = "Jump probing", value = 0, {
      incProgress(0.5, detail = "Athlete 2")
      athlete2_probing <- athlete2_jump_probe()$raw
      incProgress(1, detail = "Athlete 2")
    })


    # Convert
    plot_data <- get_metric_sensitivity(athlete2_probing, input$probing_variable)

    gg <- plot_ly() %>%
      add_lines(
        data = plot_data, x = ~change_ratio, y = ~variable,
        name = ~probing, color = ~probing,
        line = list(
          color = c(
            "mass" = "#4D4D4D",
            "max_force" = "#5DA5DA",
            "max_velocity" =  "#FAA43A",
            "push_off_distance" = "#60BD68",
            "time_to_max_activation" = "#B276B2"
          )
        ),
        hoverinfo = "text",
        text = ~ paste(
          probing, "\n",
          "Normalized change =", round(change_ratio, 2), "\n",
          input$probing_variable, "=", round(variable, 2), "\n"
        )
      ) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(
          side = "left", title = input$probing_variable,
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = "Normalized parameter change",
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })

  # -----------
  # Parameter probing
  output$athlete1_parameter_probing <- renderPlotly({
    withProgress(message = "Jump probing", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_probing <- athlete1_jump_probe()$ratio
      incProgress(1, detail = "Athlete 1")
    })

    # Convert
    plot_data <- get_parameter_sensitivity(athlete1_probing, input$parameter_variable)

    gg <- plot_ly() %>%
      add_lines(
        data = plot_data, x = ~change_ratio, y = ~value,
        name = ~variable, color = ~variable,
        hoverinfo = "text",
        text = ~ paste(
          variable, "\n",
          input$parameter_variable, " change =", round(change_ratio, 2), "\n",
          variable, "change =", round(value, 2), "\n"
        )
      ) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(
          side = "left", title = "Normalized metric change",
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = paste("Normalized", input$parameter_variable, "change"),
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })

  output$athlete2_parameter_probing <- renderPlotly({
    withProgress(message = "Jump probing", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete2_probing <- athlete2_jump_probe()$ratio
      incProgress(1, detail = "Athlete 2")
    })

    # Convert
    plot_data <- get_parameter_sensitivity(athlete2_probing, input$parameter_variable)

    gg <- plot_ly() %>%
      add_lines(
        data = plot_data, x = ~change_ratio, y = ~value,
        name = ~variable, color = ~variable,
        hoverinfo = "text",
        text = ~ paste(
          variable, "\n",
          input$parameter_variable, " change =", round(change_ratio, 2), "\n",
          variable, "change =", round(value, 2), "\n"
        )
      ) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(
          side = "left", title = "Normalized metric change",
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = paste("Normalized", input$parameter_variable, "change"),
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
