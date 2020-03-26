library(jtools)
library(data.table)
library(tidyverse)

names <- fread("./raw_data/model_names.csv")



lapply(c("", "_white", "_black", "_latinx",
         "_dem", "_rep",
         "_men", "_women"), function(g){
  tidies <- rbindlist(lapply(c("FL", "MA", "NY", "TX", "GA", "WI", "CA"),
                             function(state){
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
                                 filter(term %in% c("uncontestedTRUE", "uncontested")) %>% 
                                 mutate(term = state)
                             }))
  
  
  tidies$term <- factor(tidies$term,
                        levels = rev(c("CA",
                                       "FL",
                                       "GA",
                                       "MA",
                                       "NY",
                                       "TX",
                                       "WI"
                        )))
  
  #############
  ci_level = 0.95
  legend.title = "Model"
  facet.label.pos = "top"
  n_models <- length(unique(tidies$model))
  oshapes <- c(21:25, 15:18, 3, 4, 8)
  shapes <- oshapes[seq_len(n_models)]
  colors <- get_colors("CUD Bright", n_models)
  exp <- T
  g2 <- g
  t <- names %>% 
    filter(g == g2) %>% 
    select(group) %>% 
    pull()
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
    theme(text = element_text(family = "LM Roman 10"))
  
  save(p, file = paste0("./temp/", g, "_reg_plots.rdata"))
})



lapply(c("_lt40", "_40_65", "_gt65"), function(g){
           tidies <- rbindlist(lapply(c("FL", "MA", "NY", "TX", "GA", "CA"),
                                      function(state){
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
                                          filter(term %in% c("uncontestedTRUE", "uncontested")) %>% 
                                          mutate(term = state)
                                      }))
           
           
           tidies$term <- factor(tidies$term,
                                 levels = rev(c("CA",
                                                "FL",
                                                "GA",
                                                "MA",
                                                "NY",
                                                "TX",
                                                "WI"
                                 )))
           
           #############
           ci_level = 0.95
           legend.title = "Model"
           facet.label.pos = "top"
           n_models <- length(unique(tidies$model))
           oshapes <- c(21:25, 15:18, 3, 4, 8)
           shapes <- oshapes[seq_len(n_models)]
           colors <- get_colors("CUD Bright", n_models)
           exp <- T
           g2 <- g
           t <- names %>% 
             filter(g == g2) %>% 
             select(group) %>% 
             pull()
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
             theme(text = element_text(family = "LM Roman 10"))
           
           save(p, file = paste0("./temp/", g, "_reg_plots.rdata"))
         })


#### compare men women

tidies <- rbindlist(lapply(c("FL", "MA", "NY", "TX", "GA", "WI", "CA"),
                          function(state){
                            load(paste0("./temp/", state, "_men_match_reg.rdata"))
                            
                            model1 <- as.data.frame(c2) %>% 
                              mutate(vars = rownames(c2),
                                     model = "Men")
                            
                            colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                            
                            load(paste0("./temp/", state, "_women_match_reg.rdata"))
                            
                            model2 <- as.data.frame(c2) %>% 
                              mutate(vars = rownames(c2),
                                     model = "Women")
                            
                            colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                            
                            
                            tidies <- bind_rows(model1, model2) %>% 
                              mutate(estimate = (conf.low + conf.high) / 2) %>% 
                              filter(term %in% c("uncontestedTRUE", "uncontested")) %>% 
                              mutate(term = state)
                          }))


tidies$term <- factor(tidies$term,
                     levels = rev(c("CA",
                                    "FL",
                                    "GA",
                                    "MA",
                                    "NY",
                                    "TX",
                                    "WI"
                     )))

#############
ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
colors <- get_colors("CUD Bright", n_models)
exp <- T
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
  theme(text = element_text(family = "LM Roman 10"))

save(p, file = paste0("./temp/compare_men_women_reg_plots.rdata"))


#####
#### compare ages

tidies <- rbindlist(lapply(c("FL", "MA", "NY", "TX", "GA", "CA"),
                           function(state){
                             load(paste0("./temp/", state, "_lt40_match_reg.rdata"))
                             
                             model1 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "Younger than 40")
                             
                             colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                             
                             load(paste0("./temp/", state, "_40_65_match_reg.rdata"))
                             
                             model2 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "40 to 65")
                             
                             colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                             
                             load(paste0("./temp/", state, "_gt65_match_reg.rdata"))
                             
                             model3 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "65 or Older")
                             
                             colnames(model3) <- c("conf.low", "conf.high", "term", "model")
                             
                             
                             tidies <- bind_rows(model1, model2, model3) %>% 
                               mutate(estimate = (conf.low + conf.high) / 2) %>% 
                               filter(term %in% c("uncontestedTRUE", "uncontested")) %>% 
                               mutate(term = state)
                           }))


tidies$term <- factor(tidies$term,
                      levels = rev(c("CA",
                                     "FL",
                                     "GA",
                                     "MA",
                                     "NY",
                                     "TX",
                                     "WI"
                      )))

tidies$model <- factor(tidies$model,
                      levels = c("Younger than 40",
                                     "40 to 65",
                                     "65 or Older"
                      ))

#############
ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
colors <- get_colors("CUD Bright", n_models)
exp <- T
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
  theme(text = element_text(family = "LM Roman 10"))

save(p, file = paste0("./temp/compare_ages_reg_plots.rdata"))

##### black white latinx


tidies <- rbindlist(lapply(c("FL", "MA", "NY", "TX", "GA", "WI", "CA"),
                           function(state){
                             load(paste0("./temp/", state, "_black_match_reg.rdata"))
                             
                             model1 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "Black Voters")
                             
                             colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                             
                             load(paste0("./temp/", state, "_latinx_match_reg.rdata"))
                             
                             model2 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "Latinx Voters")
                             
                             colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                             
                             load(paste0("./temp/", state, "_white_match_reg.rdata"))
                             
                             model3 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "White Voters")
                             
                             colnames(model3) <- c("conf.low", "conf.high", "term", "model")
                             
                             
                             tidies <- bind_rows(model1, model2, model3) %>% 
                               mutate(estimate = (conf.low + conf.high) / 2) %>% 
                               filter(term %in% c("uncontestedTRUE", "uncontested")) %>% 
                               mutate(term = state)
                           }))


tidies$term <- factor(tidies$term,
                      levels = rev(c("CA",
                                     "FL",
                                     "GA",
                                     "MA",
                                     "NY",
                                     "TX",
                                     "WI"
                      )))

tidies$model <- factor(tidies$model,
                       levels = c("Black Voters",
                                  "Latinx Voters",
                                  "White Voters"
                       ))

#############
ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
colors <- get_colors("CUD Bright", n_models)
exp <- T

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
  theme(text = element_text(family = "LM Roman 10"))

save(p, file = paste0("./temp/compare_race_reg_plots.rdata"))

##### dem rep


tidies <- rbindlist(lapply(c("FL", "MA", "NY", "TX", "GA", "WI", "CA"),
                           function(state){
                             load(paste0("./temp/", state, "_dem_match_reg.rdata"))
                             
                             model1 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "Represented Voter")
                             
                             colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                             
                             load(paste0("./temp/", state, "_rep_match_reg.rdata"))
                             
                             model2 <- as.data.frame(c2) %>% 
                               mutate(vars = rownames(c2),
                                      model = "Unrepresented Voter")
                             
                             colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                             
                             
                             tidies <- bind_rows(model1, model2) %>% 
                               mutate(estimate = (conf.low + conf.high) / 2) %>% 
                               filter(term %in% c("uncontestedTRUE", "uncontested")) %>% 
                               mutate(term = state)
                           }))


tidies$term <- factor(tidies$term,
                      levels = rev(c("CA",
                                     "FL",
                                     "GA",
                                     "MA",
                                     "NY",
                                     "TX",
                                     "WI"
                      )))

tidies$model <- factor(tidies$model,
                       levels = c("Represented Voter",
                                  "Unrepresented Voter"
                       ))

#############
ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
shapes <- oshapes[seq_len(n_models)]
colors <- get_colors("CUD Bright", n_models)
exp <- T

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
  theme(text = element_text(family = "LM Roman 10"))

save(p, file = paste0("./temp/compare_dem_rep_reg_plots.rdata"))
