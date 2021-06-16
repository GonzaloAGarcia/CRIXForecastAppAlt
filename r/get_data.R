get_crypto <- function(coin = 'crix') {
  if (coin == 'crix') {
    read.csv(url("http://data.thecrix.de/data/new_crix.csv")) 
  } 
  
  # This was only used to generate graphs for the paper itself. Can still be used to expand the App so I
  # left it as a placeholder.
  else if (coin == 'btc') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\btc.csv") 
  } else if (coin == 'eth') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\eth.csv") 
  } else if (coin == 'xrp') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\xrp.csv") 
  } else if (coin == 'usdt') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\usdt.csv") 
  } else if (coin == 'link') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\link.csv") 
  }  else if (coin == 'ltc') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\ltc.csv") 
  } else if (coin == 'ada') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\ada.csv") 
  } else if (coin == 'dot') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\dot.csv") 
  } else if (coin == 'bnb') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\bnb.csv") 
  } else if (coin == 'doge') {
    read.csv("C:\\Users\\gonza\\Documents\\MasterArbeit\\marbeit\\plots\\doge.csv") 
  } 
  
}

