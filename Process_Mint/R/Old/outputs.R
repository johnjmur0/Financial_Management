createOutputs = function(finalDf)
{
  finalDf_Summary = finalDf %>% select(Timestamp, NetIncome, AvgSpend, NetWorth,
                                       Total_Savings, Investments, BaseSalary,
                                       Public_Loans, Private_Loans,
                                       Total_Assets, Total_Loans)
  print(finalDf_Summary %>% tail()) 
  
  graphDf = finalDf_Summary %>% gather(key=Key, value=Value, -Timestamp) %>%  
    dplyr::filter(!(Key %in% c("Total_Loans", "Total_Assets", "AvgSpend", "BaseSalary"))) %>% 
    mutate(Year = lubridate::year(Timestamp), Month = lubridate::month(Timestamp))
  
  finalPlot = ggplot(graphDf) + 
    
    geom_point(aes(x=Month, y=Value, color=Key)) + geom_line(aes(x=Month, y=Value, color=Key)) +
    
    scale_y_continuous(labels=dollar_format(prefix="$"), 
                       limits = signif(c(min(graphDf[["Value"]]), max(graphDf[["Value"]])), 2), 
                       breaks = signif(seq(min(graphDf[["Value"]]), max(graphDf[["Value"]]), by = 20000), 2)) +
    
    facet_wrap(~Year) + scale_x_continuous(breaks = seq(1, 12), labels = seq(1, 12))
  
  plot(finalPlot)
  
}