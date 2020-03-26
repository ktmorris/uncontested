theme_bc <- function(){theme(plot.title = element_text(family = "Roboto Black", size = 23,
                                                       color = "black", hjust = 0.5),
                             axis.title = element_text(family = "Roboto", 
                                                       color = "#525353", size = 23),
                             axis.line = element_line(colour = "black"), 
                             text = element_text(family = "Roboto", color = "#525353"), 
                             plot.caption = element_text(family = "Roboto", size = 8), 
                             panel.border = element_blank(), panel.grid.major = element_blank(), 
                             panel.background = element_rect(fill = "#dddddb"), plot.background = element_rect(fill = "#dddddb"), 
                             panel.grid.minor = element_blank(), legend.key = element_blank(), 
                             legend.background = element_rect(color = "black", fill = "#dddddb"), 
                             plot.subtitle = element_text(family = "Roboto Black", size = 15,
                                                          color = "black", hjust = 0.5), 
                             plot.margin = margin(10, 10, 10, 10),
                             axis.text = element_text(family = "Roboto", color = "#525353", size = 23))}



table <- fread("./work_product/changes.csv")

table <- melt(table, id.vars = c("State", "factor"), value.vars = c("Treated", "Untreated", "Control"))

table$variable <- factor(table$variable, levels = c("Untreated", "Treated", "Control"))

table$value <- as.numeric(gsub(",", "", table$value))

ggplot(filter(table, factor == "Non-Hispanic White"), aes(x = variable, y = value, label = percent(value))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Share Non-Hispanic White", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Share Non-Hispanic White", subtitle = "New York")

ggsave("./temp/perc_white_spsa.png", width = 11, height = 7.25, units = "in")

ggplot(filter(table, factor == "Non-Hispanic Black"), aes(x = variable, y = value, label = percent(value))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Share Non-Hispanic Black", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Share Non-Hispanic Black", subtitle = "New York")

ggsave("./temp/perc_black_spsa.png", width = 11, height = 7.25, units = "in")

ggplot(filter(table, factor == "Median Income"), aes(x = variable, y = value, label = dollar(value, accuracy = 1))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc() +
  scale_y_continuous(labels = dollar_format(accuracy = 1000)) + labs(y = "Median Income", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Census Tract Median Income", subtitle = "Texas")

ggsave("./temp/income_spsa.png", width = 11, height = 7.25, units = "in")

ggplot(filter(table, factor == "Percent Democrats"), aes(x = variable, y = value, label = percent(value))) +
  geom_col(color = "black", fill = "#ED1C24") +
  theme_bc() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + labs(y = "Share Democrats", x = NULL) +
  geom_text(size = 6, position = position_dodge(width = 1), vjust = -0.25,
            family = "Roboto", color = "#525353") +
  ggtitle("Percent Democrats", subtitle = "Florida")

ggsave("./temp/perc_dems_spsa.png", width = 11, height = 7.25, units = "in")