names <- fread("./raw_data/model_names.csv")


lapply(c("FL", "MA", "NY", "TX", "GA"), function(state){
  tidies <- rbindlist(lapply(c("", "_white", "_black", "_latinx",
                               "_dem", "_rep", "_lt40", "_40_65", "_gt65",
                               "_men", "_women"),
                             function(g){
                               print(paste(state, g))
                               load(paste0("./temp/", state, g, "_match_reg.rdata"))
                               
                               model1 <- as.data.frame(c1) %>% 
                                 mutate(vars = rownames(c1),
                                        model = "No Controls")
                               
                               colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                               
                               model2 <- as.data.frame(c2) %>% 
                                 mutate(vars = rownames(c2),
                                        model = "With Controls")
                               
                               colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                               
                               
                               tidies <- bind_rows(model1, model2) %>% 
                                 mutate(estimate = (conf.low + conf.high) / 2) %>% 
                                 filter(term == "uncontestedTRUE") %>% 
                                 mutate(term = g)
                             }))
  
  tidies <- left_join(tidies, names, by = c("term" = "g"))
  
  tidies <- mutate(tidies, term = group) %>% 
    select(-group)
  
  tidies$term <- factor(tidies$term,
                        levels = rev(c("Overall",
                                       "White",
                                       "Black",
                                       "Latinx",
                                       "Democrats",
                                       "Republicans",
                                       "Women",
                                       "Men",
                                       "Younger than 40",
                                       "40 - 65 Years Old",
                                       "Older than 65"
                        )))
  
  #############
  ci_level = 0.95
  legend.title = "Model"
  facet.label.pos = "top"
  n_models <- length(unique(tidies$model))
  oshapes <- c(21:25, 15:18, 3, 4, 8)
  shapes <- oshapes[seq_len(n_models)]
  colors <- get_colors("CUD Bright", n_models)
  ################
  
  p <- ggplot(data = tidies)
  
  p <- p + ggstance::geom_pointrangeh(aes(y = term, x = estimate, 
                                          xmin = conf.low, xmax = conf.high, colour = model, 
                                          shape = model), position = ggstance::position_dodgev(height = -.5), 
                                      fill = "white", fatten = 3, size = 0.8, show.legend = T)
  
  p <- p + geom_vline(xintercept = 1 - !exp, linetype = 2, 
                      size = 0.25) + scale_colour_manual(values = colors, name = legend.title) + 
    scale_shape_manual(values = shapes, name = legend.title) + theme_nice(legend.pos = "right") + 
    drop_y_gridlines() + theme(axis.title.y = element_blank(), 
                               axis.text.y = element_text(size = 10),
                               panel.grid.major.x = element_line(linetype = "solid")) + 
    xlab("exp(Estimate)") +
    ggtitle(paste0("Turnout Effect of Living in Uncontested House District in ", state), "Subpopulations")
  
  ggsave(p, file = paste0("./temp/", state, "_reg_plots.png"))
})
 


lapply(c("WI"), function(state){
  tidies <- rbindlist(lapply(c("", "_white", "_black", "_latinx",
                               "_dem", "_rep",
                               "_men", "_women"),
                             function(g){
                               print(paste(state, g))
                               load(paste0("./temp/", state, g, "_match_reg.rdata"))
                               
                               model1 <- as.data.frame(c1) %>% 
                                 mutate(vars = rownames(c1),
                                        model = "No Controls")
                               
                               colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                               
                               model2 <- as.data.frame(c2) %>% 
                                 mutate(vars = rownames(c2),
                                        model = "With Controls")
                               
                               colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                               
                               
                               tidies <- bind_rows(model1, model2) %>% 
                                 mutate(estimate = (conf.low + conf.high) / 2) %>% 
                                 filter(term == "uncontested") %>% 
                                 mutate(term = g)
                             }))
  
  tidies <- left_join(tidies, names, by = c("term" = "g"))
  
  tidies <- mutate(tidies, term = group) %>% 
    select(-group)
  
  tidies$term <- factor(tidies$term,
                        levels = rev(c("Overall",
                                       "White",
                                       "Black",
                                       "Latinx",
                                       "Democrats",
                                       "Republicans",
                                       "Women",
                                       "Men",
                                       "Younger than 40",
                                       "40 - 65 Years Old",
                                       "Older than 65"
                        )))
  
  #############
  ci_level = 0.95
  legend.title = "Model"
  facet.label.pos = "top"
  n_models <- length(unique(tidies$model))
  oshapes <- c(21:25, 15:18, 3, 4, 8)
  shapes <- oshapes[seq_len(n_models)]
  colors <- get_colors("CUD Bright", n_models)
  ################
  
  p <- ggplot(data = tidies)
  
  p <- p + ggstance::geom_pointrangeh(aes(y = term, x = estimate, 
                                          xmin = conf.low, xmax = conf.high, colour = model, 
                                          shape = model), position = ggstance::position_dodgev(height = -.5), 
                                      fill = "white", fatten = 3, size = 0.8, show.legend = T)
  
  p <- p + geom_vline(xintercept = 1 - !exp, linetype = 2, 
                      size = 0.25) + scale_colour_manual(values = colors, name = legend.title) + 
    scale_shape_manual(values = shapes, name = legend.title) + theme_nice(legend.pos = "right") + 
    drop_y_gridlines() + theme(axis.title.y = element_blank(), 
                               axis.text.y = element_text(size = 10),
                               panel.grid.major.x = element_line(linetype = "solid")) + 
    xlab("exp(Estimate)") +
    ggtitle(paste0("Turnout Effect of Living in Uncontested House District in ", state), "Subpopulations")
  
  ggsave(p, file = paste0("./temp/", state, "_reg_plots.png"))
})
