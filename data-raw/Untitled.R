ggplot(
  filter(force_length, push_off_distance == 0.4, decline_rate == 1.5, peak_location == -0.1),
  aes(
    x = current_distance,
    y = force_perc
  )
) +
  theme_cowplot(18) +
  geom_line(color = "blue", size = 2) +
  #facet_grid(decline_rate_label~peak_location_label) +
  xlab("Distance (m)") +
  ylab("Force percentage") +
  labs(color = "Push-off distance") +
  theme(legend.position =  "none")

# ---------------------------

fgen_get_fp <- function(current_distance,
                        push_off_distance = 0.4,
                        decline_rate = 1.05,
                        peak_location = -push_off_distance * 0.2) {

  peak_location <- push_off_distance + peak_location

  y1 <- sin((decline_rate * (peak_location - current_distance) + 1) * pi/2)
  y2 <- sin(((current_distance - peak_location) / (push_off_distance - peak_location)) * pi/2 + pi/2)

  force_percentage <- ifelse(current_distance < peak_location, y1, y2)
  force_percentage <- ifelse(current_distance > push_off_distance, 0, force_percentage)

}

parameters <- expand.grid(
  current_distance = seq(0, 0.5, length.out = 1000),
  decline_rate = c(1, 1.05, 0.8, 2),
  peak_location = c(-0.05, -0.1),
  push_off_distance = c(0.4, 0.45)
)

force_length <- parameters %>%
  mutate(
    force_perc = fgen_get_fp(current_distance, push_off_distance, decline_rate, peak_location),
    remaining_distance = push_off_distance - current_distance,
    peak_location_label = factor(paste("peak_location = ", peak_location)),
    decline_rate_label = factor(paste("decline_rate =", decline_rate)),
    push_off_distance_label = factor(paste("Push-off =", push_off_distance))
         )

ggplot(
  force_length,
  aes(
    x = current_distance,#remaining_distance,
    y = force_perc,
    color = decline_rate_label
  )
) +
  theme_cowplot(12) +
  geom_line(alpha = 0.6) +
  geom_vline(aes(xintercept = peak_location + push_off_distance)) +
  facet_grid(peak_location_label~push_off_distance_label) +
  xlab("Distance to take-off") +
  ylab("Force percentage") +
  labs(color = "Push-off distance") #+
  #scale_x_reverse()


