# helpers.R

find_range_for_similar_applicants <- 
  function(data, 
           .term, 
           .all_util, 
           .bc_util, 
           .bc_open_to_buy, 
           .percent_bc_gt_75) {  
    data |>
      mutate(distance = sqrt((term - as.numeric(.term))^2 +
                               (all_util - .all_util)^2 +
                               (bc_util - .bc_util)^2 +
                               (bc_open_to_buy - .bc_open_to_buy)^2 +
                               (percent_bc_gt_75 - .percent_bc_gt_75)^2),
             rank = min_rank(distance)) |> 
      filter(rank <= 50) |> 
      summarise(min_rate = min(int_rate), max_rate = max(int_rate))
  }

plot_interest_rate_range <- function(min_rate, max_rate, predicted_rate) {
  
  tibble(min_rate = min_rate, max_rate = max_rate) |> 
  ggplot() +
    geom_segment(aes(x = min_rate, xend = max_rate, y = 1.25, yend = 1.25),
                 color = "#4682b4", lineend = "round", linewidth = 2) +
    geom_point(aes(x = predicted_rate, y = 1.25), color = "#082D46", size = 10) +
    ylim(c(0, 2.5)) +
    theme_minimal() +
    labs(y = "", x = "Interest Rate (%)") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    scale_x_continuous(limits = c(0, 35),
                       breaks = seq(from = 0, to = 35, by = 5)) +
    annotate("text", x = min_rate, y = 1.25, label = paste0(min_rate, " "),
             hjust = "right", color = "#4682b4", size = 7) +
    annotate("text", x = max_rate,  y = 1.25, label = paste0(" ", max_rate),
             hjust = "left", color = "#4682b4", size = 7)
  
}