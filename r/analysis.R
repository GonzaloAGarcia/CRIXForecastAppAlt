source("r/model_functions.R")
source("r/analysis_functions.R")
source("r/get_data.R")

interactive_graph <- function(
                              periods_to_forecast = 0,
                              for_model) {
  
  
  if (periods_to_forecast <= 0) {
    log_ret_without_forecast()
  } else {
    if (for_model == 'ets_lr') {
      ets_lr(periods_to_forecast)
    } else if (for_model == 'naive_lr') {
      naive_lr(periods_to_forecast)
    } else if (for_model == 'mean_lr') {
      mean_lr(periods_to_forecast)
    } else if (for_model == 'lstm') {
      lstm(periods_to_forecast)
    }
    
  }
}



analysis <- function(for_analysis, part = "") {
  if (for_analysis == "acf") {
    myacf()
  } else if (for_analysis == "pacf") {
    mypacf()
  } else if (for_analysis == "mdecomp" & part == "seasonal") {
    mdecomp(part="seasonal")
  } else if (for_analysis == "mdecomp" & part == "trend") {
    mdecomp(part="trend")
  } else if (for_analysis == "mdecomp" & part == "remainder") {
    mdecomp(part="remainder")
  }
}
