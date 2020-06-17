
```{r, message = FALSE, warning = FALSE, fig.cap = "\\label{oos2} Residuals by region.", echo = FALSE, fig.height = 8}
oos_df(depvar = "deaths_cum_log", controls = controls) %>%
  mutate(geoid2 = cases) %>%
  left_join(select(df_today, geoid2, region, gdp_pc)) %>%
  mutate(region = ifelse(region == "Europe & Central Asia" | region== "North America", "Europe, N America", region))   %>%
  ggplot(aes(gdp_pc, actual - pred)) +   
  geom_point(alpha = 0.5, size = 2) +
  ylab("Residuals")+
  xlab("GDP per capita ('000 USD)") +
  scale_x_continuous(trans='log10') +
  geom_abline(slope = 0, intercept = 0, linetype = 'dotted') +
  theme_bw() +  
  facet_wrap(~region) +
  theme(panel.grid.minor = element_blank()) +
  ggrepel::geom_text_repel(data = . %>% filter(abs(actual - pred)> 1.5),
                           aes(label = cases))

```
