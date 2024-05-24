# helpers.R

find_100_most_similar <- 
  function(data, 
           .term, 
           .all_util, 
           .bc_util, 
           .bc_open_to_buy, 
           .percent_bc_gt_75) {  
    data |>
      mutate(term = as.numeric(SUBSTRING(term, 2,2)),
             distance = sqrt((term - .term)^2 +
                               (all_util - .all_util)^2 +
                               (bc_util - .bc_util)^2 +
                               (bc_open_to_buy - .bc_open_to_buy)^2 +
                               (percent_bc_gt_75 - .percent_bc_gt_75)^2),
             rank = min_rank(distance)) |> 
      filter(rank <= 100) |> 
      summarise(min_rate = min(int_rate), max_rate = max(int_rate))
  }