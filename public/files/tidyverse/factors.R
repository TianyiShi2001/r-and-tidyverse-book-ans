ggplot(gss_cat, aes(rincome))+
  geom_bar()

count(gss_cat, relig)

count(gss_cat, rincome)

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

relig_summary

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point(aes(size = n))
