get_adjusted_threshold <- function(push_off_distance, push_off_distance_0, threshold_0) {
  1 + (((threshold_0 - 1) * push_off_distance_0) / push_off_distance)
}

get_adjusted_start_perc <- function(push_off_distance, push_off_distance_0, start_perc_0, threshold_0) {
  threshold_adj <- get_adjusted_threshold(push_off_distance, push_off_distance_0, threshold_0)

  ((push_off_distance * start_perc_0 * threshold_adj) + (push_off_distance_0 * threshold_0) - (push_off_distance * threshold_adj))/
 (push_off_distance_0 * threshold_0) * (1.5 + -0.5 * start_perc_0)
}



parameters <- expand.grid(
  push_off_perc = seq(0, 1, length.out = 100),
  start_perc = c(0.6, 0.8),
  threshold = c(0.8, 0.9),
  push_off_distance = c(0.3, 0.4, 0.5),
  adjusted = c(FALSE, TRUE)
)

# Make adjustments
parameters <- parameters %>%
  mutate(
    current_distance = push_off_perc * push_off_distance,
    threshold_label = factor(paste("Threshold = ", threshold)),
    start_perc_label = factor(paste("Start perc =", start_perc)),
    push_off_distance_label = factor(paste("Push-off =", push_off_distance)),
    adjusted_label = factor(paste("Adjusted =", adjusted)),
    remaining_distance = push_off_distance - current_distance,

    # Adjustments
    start_perc = ifelse(
      adjusted,
      get_adjusted_start_perc(push_off_distance, 0.4, start_perc, threshold),
      start_perc
    ),

    threshold = ifelse(
      adjusted,
      get_adjusted_threshold(push_off_distance, 0.4, threshold),
      threshold
    ),
    )


force_length <- parameters %>%
  mutate(
    force_perc = vjsim::fgen_get_force_percentage(
      push_off_perc = current_distance / push_off_distance,
      start_perc = start_perc,
      threshold = threshold
    )
  )

# Unadjusted
ggplot(
  filter(force_length, adjusted == FALSE),
  aes(
    x = remaining_distance,
    y = force_perc,
    color = push_off_distance_label
  )
) +
  theme_cowplot(12) +
  geom_line(alpha = 0.6) +
  facet_grid(threshold_label~start_perc_label) +
  xlab("Distance to take-off") +
  ylab("Force percentage") +
  labs(color = "Push-off distance") +
  scale_x_reverse()

# Adjusted
ggplot(
  filter(force_length, adjusted == TRUE),
  aes(
    x = remaining_distance,
    y = force_perc,
    color = push_off_distance_label
  )
) +
  theme_cowplot(12) +
  geom_line(alpha = 0.6) +
  facet_grid(threshold_label~start_perc_label) +
  xlab("Distance to take-off") +
  ylab("Force percentage") +
  labs(color = "Push-off distance") +
  scale_x_reverse()
