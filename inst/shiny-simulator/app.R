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

profiling_variables <- c(
  "external_load",
  "mass",
  "weight",
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
  "impulse",
  "ratio_peak_to_take_off_velocity",
  "ratio_mean_to_take_off_velocity",
  "ratio_mean_to_peak_velocity",
  "ratio_mean_to_peak_power",
  "ratio_mean_GRF_over_distance_to_time"
)

profile_summary_variables <- c(
  "profile_mean_FV.F0",
  "profile_mean_FV.V0",
  "profile_mean_FV.Pmax",
  "profile_mean_FV.Sfv",
  "profile_mean_power.Pmax",
  "profile_mean_power.Pmax_location",
  "profile_mean_power.F0_perc",
  "profile_peak_FV.F0",
  "profile_peak_FV.V0",
  "profile_peak_FV.Pmax",
  "profile_peak_FV.Sfv",
  "profile_peak_power.Pmax",
  "profile_peak_power.Pmax_location",
  "profile_peak_power.F0_perc",
  "profile_load_take_off_velocity.V0",
  "profile_load_take_off_velocity.L0",
  "profile_load_take_off_velocity.Imax",
  "profile_load_take_off_velocity.Slv",
  "profile_load_impulse.Imax",
  "profile_load_impulse.Imax_location",
  "profile_load_impulse.L0_perc"
)


profile_type <- c(
  "profile_mean_FV",
  "profile_mean_power",
  "profile_peak_FV",
  "profile_peak_power",
  "profile_load_take_off_velocity",
  "profile_load_impulse"
)

vj_probing_change <- seq(0.9, 1.1, length.out = 7)

# For profiling
external_load_options <- seq(-40, 120, 10)
external_load_options_selected <- seq(0, 80, 20)

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
get_parameter_sensitivity <- function(probing_data, parameter, invert = FALSE, summary_variables = summary_variables, key_columns = 13) {
  probing_data <- probing_data[probing_data$probing == parameter, ]

  if (invert) {
    probing_data$change_ratio <- 1 / df$change_ratio
  }

  df <- gather(probing_data, key = "variable", value = "value", -(1:key_columns)) %>%
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
            6,
            selectInput(
              inputId = "jump_kinetics_x_var",
              label = "X axis",
              choices = trace_variables,
              selected = "time"
            )
          ),
          column(
            6,
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
      ),
      tabPanel(
        "Profile Analysis",
        br(),
        selectInput(
          inputId = "selected_external_load",
          label = "External load",
          choices = external_load_options,
          multiple = TRUE,
          selected = external_load_options_selected
        ),
        br(),
        fixedRow(
          column(
            6,
            selectInput(
              inputId = "jump_profile_x_var",
              label = "X axis",
              choices = profiling_variables,
              selected = "mean_GRF_over_distance"
            )
          ),
          column(
            6,
            selectInput(
              inputId = "jump_profile_y_var",
              label = "Y axis",
              choices = profiling_variables,
              selected = "mean_velocity"
            )
          )
        ),
        br(),
        plotlyOutput("jump_profile"),
        br(),
        h4("Profile summaries"),
        fixedRow(
          column(
            6,
            h5("Athlete 1"),
            dataTableOutput("athlete1_jump_profile_summary")
          ),
          column(
            6,
            h5("Athlete 2"),
            dataTableOutput("athlete2_jump_profile_summary")
          )
        ),
        h4("Profile data"),
        br(),
        h5("Athlete 1"),
        dataTableOutput("athlete1_jump_profile_table"),
        br(),
        h5("Athlete 2"),
        dataTableOutput("athlete2_jump_profile_table")
      ),

      tabPanel(
        "Profile Sensitivity",
        br(),
        selectInput(
          inputId = "profile_variable",
          label = "Probing profile variable",
          choices = profile_summary_variables,
          selected = "profile_mean_FV.Pmax"
        ),
        fixedRow(
          column(
            6,
            h4("Athlete 1"),
            plotlyOutput("athlete1_profile_probing")
          ),
          column(
            6,
            h4("Athlete 2"),
            plotlyOutput("athlete2_profile_probing")
          )
        ),
        br(),
        fixedRow(
          column(
            6,
            selectInput(
              inputId = "profile_parameter_variable",
              label = "Probing Force Generator parameter",
              choices = parameter_variables,
              selected = "push_off_distance"
            ),
            br(),
            h4("Athlete 1"),
            plotlyOutput("athlete1_profile_parameter_probing")
          ),
          column(
            6,
            selectInput(
              inputId = "profile_type",
              label = "Profile type",
              choices = profile_type,
              selected = "profile_mean_FV"
            ),
            br(),
            h4("Athlete 2"),
            plotlyOutput("athlete2_profile_parameter_probing")
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
  # -------
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

  # --------
  # Profile reactive element
  athlete1_get_jump_profile <- eventReactive(input$calculate,
    {
      jump_profile <- vj_profile(
        external_load = as.numeric(input$selected_external_load),
        mass = athlete1_BW(),
        # weight = athlete1_BW() * gravity_const,
        push_off_distance = athlete1_push_off_distance(),
        gravity_const = gravity_const,
        time_step = time_step,
        max_force = athlete1_max_force(),
        max_velocity = athlete1_max_velocity(),
        decline_rate = athlete1_decline_rate(),
        peak_location = athlete1_peak_location(),
        time_to_max_activation = athlete1_time_to_max_activation()
      )
      return(jump_profile)
    },
    ignoreNULL = FALSE
  )

  athlete2_get_jump_profile <- eventReactive(input$calculate,
    {
      jump_profile <- vj_profile(
        external_load = as.numeric(input$selected_external_load),
        mass = athlete2_BW(),
        # weight = athlete2_BW() * gravity_const,
        push_off_distance = athlete2_push_off_distance(),
        gravity_const = gravity_const,
        time_step = time_step,
        max_force = athlete2_max_force(),
        max_velocity = athlete2_max_velocity(),
        decline_rate = athlete2_decline_rate(),
        peak_location = athlete2_peak_location(),
        time_to_max_activation = athlete2_time_to_max_activation()
      )
      return(jump_profile)
    },
    ignoreNULL = FALSE
  )

  # --------
  # Profile probing reactive elements
  # Jump probe reactive element
  athlete1_profile_probe <- eventReactive(input$calculate,
    {
      profile_probe_ratio <- probe_profile(
        mass = athlete1_BW(),
        external_load =  as.numeric(input$selected_external_load),
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

      return(list(ratio = profile_probe_ratio))
    },
    ignoreNULL = FALSE
  )

athlete2_profile_probe <- eventReactive(input$calculate,
  {
    profile_probe_ratio <- probe_profile(
      mass = athlete2_BW(),
      external_load = as.numeric(input$selected_external_load),
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

    return(list(ratio = profile_probe_ratio))
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
      data.frame(Athlete = "Athlete 1", round(athlete1_summary, 2)),
      data.frame(Athlete = "Athlete 2", round(athlete2_summary, 2))
    )
    # rownames(df) <- c("Athlete 1", "Athlete 2")
    # df <- t(df)
    df <- datatable(df, rownames = FALSE)
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

    df <- datatable(athlete2_trace, rownames = FALSE) %>%
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
    plot_data <- get_parameter_sensitivity(
      athlete1_probing,
      input$parameter_variable,
      summary_variables = summary_variables,
      key_columns = 13)

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
      incProgress(0.5, detail = "Athlete 2")
      athlete2_probing <- athlete2_jump_probe()$ratio
      incProgress(1, detail = "Athlete 2")
    })

    # Convert
    plot_data <- get_parameter_sensitivity(
      athlete2_probing,
      input$parameter_variable,
      summary_variables = summary_variables,
      key_columns = 13)

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

  # ---------------------------------------------
  # Profiling
  output$jump_profile <- renderPlotly({
    withProgress(message = "Jump profiling", value = 0, {
      incProgress(0, detail = "Athlete 1")
      athlete1_jump_profile_data <- athlete1_get_jump_profile()
      incProgress(0.5, detail = "Athlete 2")
      athlete2_jump_profile_data <- athlete2_get_jump_profile()
      incProgress(1, detail = "Athlete 2")
    })


    # bind together
    athlete1_plot_data <- data.frame(
      x_var = athlete1_jump_profile_data[, input$jump_profile_x_var],
      y_var = athlete1_jump_profile_data[, input$jump_profile_y_var],
      athlete1_jump_profile_data
    )

    athlete2_plot_data <- data.frame(
      x_var = athlete2_jump_profile_data[, input$jump_profile_x_var],
      y_var = athlete2_jump_profile_data[, input$jump_profile_y_var],
      athlete2_jump_profile_data
    )

    gg <- plot_ly() %>%
      add_lines(
        data = athlete1_plot_data, x = ~x_var, y = ~y_var,
        name = "Athlete 1", line = list(color = "#5DA5DA")
      ) %>%
      add_markers(
        data = athlete1_plot_data, x = ~x_var, y = ~y_var,
        name = "Athlete 1", marker = list(color = "#5DA5DA"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 1", "\n",
          "external_load =", round(external_load, 2), "kg\n",
          "mass =", round(mass, 2), "kg\n",
          "weight =", round(weight, 2), "N\n",
          "take_off_time =", round(take_off_time, 2), "s\n",
          "take_off_velocity =", round(take_off_velocity, 2), "m/s\n",
          "height =", round(height, 2), "m\n",
          "mean_GRF_over_distance =", round(mean_GRF_over_distance, 0), "N\n",
          "mean_GRF_over_time =", round(mean_GRF_over_time, 0), "N\n",
          "peak_GRF =", round(peak_GRF, 0), "N\n",
          "mean_velocity =", round(mean_velocity, 2), "m/s\n",
          "peak_velocity =", round(peak_velocity, 2), "m/s\n",
          "mean_power =", round(mean_power, 0), "W\n",
          "peak_power =", round(peak_power, 0), "W\n",
          "peak_RFD =", round(peak_RFD, 0), "N/s\n",
          "peak_RPD =", round(peak_RPD, 0), "W/s\n",
          "work_done =", round(work_done, 2), "J\n",
          "impulse =", round(impulse, 2), "Ns\n"
        )
      ) %>%
      add_lines(
        data = athlete2_plot_data, x = ~x_var, y = ~y_var,
        name = "Athlete 2", line = list(color = "#FAA43A")
      ) %>%
      add_markers(
        data = athlete2_plot_data, x = ~x_var, y = ~y_var,
        name = "Athlete 2", marker = list(color = "#FAA43A"),
        hoverinfo = "text",
        text = ~ paste(
          "Athlete 2", "\n",
          "external_load =", round(external_load, 2), "kg\n",
          "mass =", round(mass, 2), "kg\n",
          "weight =", round(weight, 2), "N\n",
          "take_off_time =", round(take_off_time, 2), "s\n",
          "take_off_velocity =", round(take_off_velocity, 2), "m/s\n",
          "height =", round(height, 2), "m\n",
          "mean_GRF_over_distance =", round(mean_GRF_over_distance, 0), "N\n",
          "mean_GRF_over_time =", round(mean_GRF_over_time, 0), "N\n",
          "peak_GRF =", round(peak_GRF, 0), "N\n",
          "mean_velocity =", round(mean_velocity, 2), "m/s\n",
          "peak_velocity =", round(peak_velocity, 2), "m/s\n",
          "mean_power =", round(mean_power, 0), "W\n",
          "peak_power =", round(peak_power, 0), "W\n",
          "peak_RFD =", round(peak_RFD, 0), "N/s\n",
          "peak_RPD =", round(peak_RPD, 0), "W/s\n",
          "work_done =", round(work_done, 2), "J\n",
          "impulse =", round(impulse, 2), "Ns\n"
        )
      ) %>%
      layout(
        showlegend = FALSE,
        yaxis = list(
          side = "left", title = input$jump_profile_y_var,
          showgrid = TRUE, zeroline = TRUE
        ),
        xaxis = list(
          side = "left", title = input$jump_profile_x_var,
          showgrid = TRUE, zeroline = TRUE
        )
      )

    return(gg)
  })


  output$athlete1_jump_profile_table <- renderDataTable({
    withProgress(message = "Jump profiling", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_jump_profile_data <- athlete1_get_jump_profile()
      incProgress(1, detail = "Athlete 1")
    })

    df <- datatable(athlete1_jump_profile_data, rownames = FALSE) %>%
      formatRound(columns = 1:ncol(athlete1_jump_profile_data), digits = 2)
    return(df)
  })

  output$athlete2_jump_profile_table <- renderDataTable({
    withProgress(message = "Jump profiling", value = 0, {
      incProgress(0.5, detail = "Athlete 2")
      athlete2_jump_profile_data <- athlete2_get_jump_profile()
      incProgress(1, detail = "Athlete 2")
    })

    df <- datatable(athlete2_jump_profile_data, rownames = FALSE) %>%
      formatRound(columns = 1:ncol(athlete2_jump_profile_data), digits = 2)
    return(df)
  })

  # -----
  # Summary profile tables
  output$athlete1_jump_profile_summary <- renderDataTable({
    withProgress(message = "Jump profiling", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_jump_profile_data <- athlete1_get_jump_profile()
      incProgress(1, detail = "Athlete 1")
    })

    all_profiles <- get_all_profiles(athlete1_jump_profile_data)$data_frame

    df <- datatable(all_profiles, rownames = FALSE) %>%
      formatRound(columns = 3, digits = 2)
    return(df)
  })

  output$athlete2_jump_profile_summary <- renderDataTable({
    withProgress(message = "Jump profiling", value = 0, {
      incProgress(0.5, detail = "Athlete 2")
      athlete2_jump_profile_data <- athlete2_get_jump_profile()
      incProgress(1, detail = "Athlete 2")
    })

    all_profiles <- get_all_profiles(athlete2_jump_profile_data)$data_frame

    df <- datatable(all_profiles, rownames = FALSE) %>%
      formatRound(columns = 3, digits = 2)
    return(df)
  })

  # -----------------------------------
  # Profile Probing
  output$athlete1_profile_probing <- renderPlotly({
    withProgress(message = "Profile probing", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_probing <- athlete1_profile_probe()$ratio
      incProgress(1, detail = "Athlete 1")
    })

    # Convert
    plot_data <- get_metric_sensitivity(athlete1_probing, input$profile_variable)

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
          "Normalized",input$profile_variable, "chage", "=", round(variable, 2), "\n"
        )
      ) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(
          side = "left", title = paste("Normalized", input$profile_variable, "chage"),
          showgrid = TRUE, zeroline = FALSE
        ),
        xaxis = list(
          side = "left", title = "Normalized parameter change",
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })

  output$athlete2_profile_probing <- renderPlotly({
    withProgress(message = "Profile probing", value = 0, {
      incProgress(0.5, detail = "Athlete 2")
      athlete2_probing <- athlete2_profile_probe()$ratio
      incProgress(1, detail = "Athlete 2")
    })

    # Convert
    plot_data <- get_metric_sensitivity(athlete2_probing, input$profile_variable)

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
          "Normalized",input$profile_variable, "chage", "=", round(variable, 2), "\n"
        )
      ) %>%
      layout(
        showlegend = TRUE,
        yaxis = list(
          side = "left", title = paste("Normalized", input$profile_variable, "chage"),
          showgrid = TRUE, zeroline = FALSE
        ),
        xaxis = list(
          side = "left", title = "Normalized parameter change",
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })

  output$athlete1_profile_parameter_probing <- renderPlotly({
    withProgress(message = "Profile probing", value = 0, {
      incProgress(0.5, detail = "Athlete 1")
      athlete1_probing <- athlete1_profile_probe()$ratio
      incProgress(1, detail = "Athlete 1")
    })
    # Convert
    plot_data <- get_parameter_sensitivity(
      athlete1_probing,
      input$profile_parameter_variable,
      summary_variables = profile_summary_variables,
      key_columns = 12
      ) %>%
      filter(grepl(input$profile_type, variable)) %>%
      mutate(variable = str_remove(variable, paste(input$profile_type, ".", sep="")))


    gg <- plot_ly() %>%
      add_lines(
        data = plot_data, x = ~change_ratio, y = ~value,
        name = ~variable, color = ~variable,
        hoverinfo = "text",
        text = ~ paste(
          variable, "\n",
          input$profile_parameter_variable, " change =", round(change_ratio, 2), "\n",
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
          side = "left", title = paste("Normalized", input$profile_parameter_variable, "change"),
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })

  output$athlete2_profile_parameter_probing <- renderPlotly({
    withProgress(message = "Profile probing", value = 0, {
      incProgress(0.5, detail = "Athlete 2")
      athlete2_probing <- athlete2_profile_probe()$ratio
      incProgress(1, detail = "Athlete 2")
    })
    # Convert
    plot_data <- get_parameter_sensitivity(
      athlete2_probing,
      input$profile_parameter_variable,
      summary_variables = profile_summary_variables,
      key_columns = 12
    ) %>%
      filter(grepl(input$profile_type, variable)) %>%
      mutate(variable = str_remove(variable, paste(input$profile_type, ".", sep="")))


    gg <- plot_ly() %>%
      add_lines(
        data = plot_data, x = ~change_ratio, y = ~value,
        name = ~variable, color = ~variable,
        hoverinfo = "text",
        text = ~ paste(
          variable, "\n",
          input$profile_parameter_variable, " change =", round(change_ratio, 2), "\n",
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
          side = "left", title = paste("Normalized", input$profile_parameter_variable, "change"),
          showgrid = TRUE, zeroline = FALSE
        )
      )

    return(gg)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
