GDP = NULL
for (i in unique(da$year)) {
  te = da %>% filter(year == i) %>% group_by(continent) %>% summarise(tol_POP = sum(as.numeric(pop)),GDP_perCap = sum(pop*gdpPercap)/tol_POP) %>% arrange(continent)
  GDP = cbind(GDP,as.matrix(te)) 
}

kable(GDP[,1:12], caption = "Total GDP for each continents 1952-1967", align = "c", booktab =T, format = "latex",longtable = F) %>% kable_styling(latex_options = c("HOLD_position")) %>% add_header_above(c("1952"=3,"1957"=3,
                                                                                                                                                                                                             "1962"=3,"1967"=3))
kable(GDP[,13:24], caption = "Total GDP for each continents 1972-1987", align = "c", booktab =T, format = "latex",longtable = F) %>% kable_styling(latex_options = c("HOLD_position")) %>% add_header_above(c("1972"=3,"1977"=3,
                                                                                                                                                                                                              "1982"=3,"1987"=3))
kable(GDP[,25:36], caption = "Total GDP for each continents 1992-2007", align = "c", booktab =T, format = "latex",longtable = F) %>% kable_styling(latex_options = c("HOLD_position")) %>% add_header_above(c("1992"=3,"1997"=3,
                                                                                                                                                                                                              "2002"=3,"2007"=3))