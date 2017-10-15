
# 4. Nigel's animated plot
# Lofi version:
# Need gganimate
total_egm_clean <-
 total_egm_df %>%
  separate(month_year,
           into = c("month", "year"),
           sep = "\\s",
           convert = FALSE) %>%
  mutate(month_idx = match(month, month.name),
         taking_per_machine = metered_win/operational_egms) %>%
  filter(!(year %in% c("2004","2017")))

line_plot <-
    ggplot(total_egm_clean, aes(x = fct_reorder(month, month_idx) , y = taking_per_machine, group = year)) +
    geom_line(aes(frame = year)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(labels = month.abb) +
    ylab("Monthly Takings Per Poker Machine") +
    xlab("Month of the Year")

gganimate(line_plot)

# To have transition animations we're going to need to use
# tweenr: https://github.com/thomasp85/tweenr
# Should be fun!
# test tweenr

egm_animation <-
  total_egm_clean %>%
  select(month, year, taking_per_machine) %>%
  spread(key = month, value = taking_per_machine) %>%
  arrange(year)

test_trans <- transition_row(df = egm_animation[1:2,],
                             tween_cols = month.name,
                             nframes = 40)
test_trans2 <- transition_row(df = egm_animation[2:3,],
                             tween_cols = month.name,
                             nframes = 40,
                             frame_offset = max(test_trans$.frame))
test_df <-
  rbind(
  test_trans,
  start_row_shadow(test_trans, "year", nrow(test_trans)+
                     nrow(test_trans2)),
  test_trans2,
  start_row_shadow(test_trans2, "year", nrow(test_trans2))
  )

test_df %>%
   gather(key = "month",
         value = "taking_per_machine",
         -.frame, -year
         ) %>%
    mutate(month_idx = match(month, month.name)) %>%
    ggplot(aes(x = fct_reorder(month, month_idx),
               y = taking_per_machine,
               frame = .frame)) +
    geom_line(aes(group = year, colour = year)) +
    scale_y_continuous(labels = dollar) +
    scale_x_discrete(labels = month.abb) +
    ylab("Monthly Takings Per Poker Machine") +
    xlab("Month of the Year") +
    scale_color_manual(values = c("grey","grey",
                                  "black","black", "black"),
                       guide = "none")


gganimate()
