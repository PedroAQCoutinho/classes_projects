library(rmarkdown)

#render wwwusage
rmarkdown::render('time_series/WWWusage_arima_fit.R')

#render anfavea
rmarkdown::render('time_series/ANFAVEA_arima_fit.R')
rmarkdown::render('time_series/ANFAVEA_arima_fit.R', output_format = 'github_document')

