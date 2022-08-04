library(ggplot2)
library(sf)

source.cols <- c('Dolphins' = 'tomato',#'grey60', 
                 'Pelicans' = 'firebrick2', #'#1D8D2A', 
                 'Gulls & Terns' = 'firebrick', #'#AA7601',
                 'Fishing' = 'royalblue')

nbins <- 30

rename_prey <- function(prey.vec) {
  temp <- stringr::str_replace_all(string = prey.vec, pattern = 'croaker.spot.perch',
                               replacement = 'sm.sciaenids') %>%
    stringr::str_replace_all(pattern = '\\.', replacement = '. ') %>%
    stringr::str_replace_all(pattern = 'blue.', replacement = 'blue') %>%
    stringr::str_replace_all(pattern = 'panaeid', replacement = 'penaeid') %>%
    stringr::str_to_title()
  
  # order levels so that adults and juveniles of a species are next to each other
  if(any(stringr::str_detect(prey.vec, 'juv'))) {
    factor(temp, 
           levels = unique(temp)[stringr::str_order(
             stringr::str_sub(unique(temp), start = 6))]) %>%
      return()
  } else {
    return(temp)
  }
}

summary_func <- function(vec) {
  data.frame(ymin = quantile(vec, probs = 0.25),
             y = median(vec),
             ymax = quantile(vec, probs = 0.75))
}

get_indirect_res <- function(predation.arr, fishing.arr) {
  fishing <- reshape2::melt(-fishing.arr*100) %>%
    as_tibble() %>%
    rename(Prey = Var1, iter = Var2, change = value) %>%
    mutate(Source = 'Fishing') 
  indirect.res <- reshape2::melt(10*predation.arr) %>%
    as_tibble() %>%
    rename(Prey = biomass.i, Source = r.j, iter = Var3, change = value) %>%
    mutate(Source = case_when(Source == 'diving.birds' ~ 'Gulls & Terns',
                              TRUE ~ stringr::str_to_title(Source))) %>% 
    bind_rows(fishing) %>%
    mutate(Source = factor(Source, levels = c('Dolphins', 'Pelicans', 
                                              'Gulls & Terns', 'Fishing')),
           Prey = rename_prey(Prey))
  return(indirect.res)
}

round_median <- function(x) {
  round(median(x), 2) 
}
make_indirect_figure <- function(indirect.res) {
  plot.obj <- indirect.res %>%
    ggplot(aes(x = Prey, fill = Source, y = change)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = 'summary', fun.data = summary_func, 
             position = position_dodge(width = 1)) +
    # stat_summary(aes(label=..y..), geom = 'text', fun = round_median, 
    #              position = position_dodge(width = 1)) +
    geom_linerange(stat = 'summary', fun.data = summary_func, 
                   position = position_dodge(width = 1)) +
    scale_fill_manual(values = source.cols) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = .5), 
          plot.margin = unit(c(5.5, 5.5, 7, 5.5), units = 'points')) +
    ylab('% change in biomasss per 10%\n decrease in predator survival or fishing') +
    NULL
  return(plot.obj)
}


# make rpath table by rendering rmarkdown document
rmarkdown::render(input = here('R', 'rpath_table.Rmd'), 
                  output_dir = here('Figs'), 
                  output_file = 'rpath_table.docx')

sum(test$Biomass[test$type==0]) # total living biomass
sum(test$PB[test$type==0]*test$Biomass[test$type==0]) # total living biomass

mean(test$TL[test$type <= 1]) # mean trophic level
sum(test$TL[test$type <= 1] * test$Biomass[test$type <= 1]) / 
  sum(test$Biomass[test$type <= 1]) # biomass-weighted MTL

png('Figs/direct-mortality.png', res=1000, height=7, width=9, units='in')
as_tibble(mort.mat) %>%
  bind_cols(Prey = rownames(mort.mat)) %>%
  rename(Dolphins = dolphins,
         Pelicans = pelicans,
         `Gulls & Terns` = diving.birds,
         Fishing = V4) %>%
  tidyr::pivot_longer(cols = Dolphins:Fishing, names_to = 'Source', 
                      values_to = 'mort.pct') %>%
  mutate(Source = factor(Source, levels = c('Dolphins', 'Pelicans', 
                                            'Gulls & Terns', 'Fishing')),
         Prey =  rename_prey(Prey)) %>%
  ggplot() +
  geom_col(aes(x = Prey, fill = Source, y = mort.pct)) +
  scale_fill_manual(values = source.cols) +
  labs(x = '', y = 'Fraction of total mortality from source') +
  ylim(0, 1)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5), 
        plot.margin = unit(c(5.5, 5.5, 7, 5.5), units = 'points')) +
  NULL
dev.off()

png('Figs/indirect.png', res=1000, height=8, width=9, units='in')
purrr::map2(.x = list(Jr.inv.std, Jr.inv.std.combined),
            .y = list(fishing.res, fishing.res.combined), 
            .f = ~ get_indirect_res(.x, .y)) %>%
  purrr::map(.f = make_indirect_figure) %>%
  cowplot::plot_grid(plotlist = ., ncol = 1, labels = list('a)', 'b)'))
dev.off()

png('Figs/indirect_comparisons95.png', width = 7, height = 10, units = 'in', res = 500)
get_indirect_res(Jr.inv.std, fishing.res) %>%
  tidyr::pivot_wider(id_cols = c(Prey, iter), names_from = Source, 
                     values_from = change) %>%
  group_by(Prey) %>% 
  mutate(across(Dolphins:Fishing, percent_rank, .names = '{.col}_rank')) %>%
  filter(if_all(.cols = Dolphins_rank:Fishing_rank, ~ .x > 0.025 & .x < 0.975)) %>%
#  select(c(Prey, Dolphins_rank:Fishing_rank)) %>%
#  group_walk(~ pairs(.x, main = .y$Prey))
  group_by(iter, .add = FALSE) %>%
  mutate(n_remaining = n()) %>% 
  filter(n_remaining ==10) %>% # nrow()/10
  select(Prey:Fishing) %>%
  tidyr::pivot_longer(cols = Dolphins:`Gulls & Terns`, 
                      names_to = 'Source', values_to = 'change') %>%
  group_by(Prey, .add = FALSE) %>%
  group_map(function(.x, .y)
    ggplot(.x, aes(x = Fishing, y = change)) +
      geom_density2d_filled(contour_var = 'ndensity', bins = nbins,
                           show.legend = FALSE) +
      geom_hline(yintercept = 0, col = 'gray70', lwd = rel(0.5)) +
      geom_vline(xintercept = 0, col = 'gray70', lwd = rel(0.5)) +
      geom_abline(slope = 1, intercept = 0, col = 'red', lwd = rel(0.5)) +
      geom_point(pch = '.', alpha = 0.25) +
      facet_wrap(~ Source) +
      scale_fill_manual(values = c('white', rev(PNWColors::pnw_palette('Winter', n = nbins-1)))) +
      theme_classic() +
      theme(strip.background = element_rect(linetype = 0)) +
      labs(y = '', title = .y$Prey) +
      NULL
  ) %>%
cowplot::plot_grid(plotlist = ., nrow = 5, ncol = 2, 
                   labels = as.list(paste0(letters[1:10], ')')))
dev.off() 

png('Figs/indirect_comparisons_combined.png', width = 7, height = 7, units = 'in', res = 500)
get_indirect_res(Jr.inv.std.combined, fishing.res.combined) %>%
  tidyr::pivot_wider(id_cols = c(Prey, iter), names_from = Source, 
                     values_from = change) %>%
  group_by(Prey) %>% 
  mutate(across(Dolphins:Fishing, percent_rank, .names = '{.col}_rank')) %>% 
  filter(if_all(.cols = Dolphins_rank:Fishing_rank, ~ .x > 0.025 & .x < 0.975)) %>%
  group_by(iter, .add = FALSE) %>%
  mutate(n_remaining = n()) %>% 
  filter(n_remaining == 5) %>% # nrow()/5
  select(Prey:Fishing) %>%
  tidyr::pivot_longer(cols = Dolphins:`Gulls & Terns`, 
                      names_to = 'Source', values_to = 'change') %>%
  group_by(Prey, .add = FALSE) %>%
  group_map(function(.x, .y)
    ggplot(.x, aes(x = Fishing, y = change)) +
      geom_density2d_filled(contour_var = 'ndensity', bins = nbins,
                            show.legend = FALSE) +
      geom_hline(yintercept = 0, col = 'gray70', lwd = rel(0.5)) +
      geom_vline(xintercept = 0, col = 'gray70', lwd = rel(0.5)) +
      geom_abline(slope = 1, intercept = 0, col = 'red', lwd = rel(0.5)) +
      geom_point(pch = '.', alpha = 0.25) +
      facet_wrap(~ Source) +
      scale_fill_manual(values = c('white', rev(PNWColors::pnw_palette('Winter', n = nbins-1)))) +
      theme_classic() +
      theme(strip.background = element_rect(linetype = 0)) +
      labs(y = '', title = .y$Prey) +
      NULL
  ) %>%
  cowplot::plot_grid(plotlist = ., nrow = 3, ncol = 2,
                     labels = as.list(paste0(letters[1:5], ')')))  
dev.off() 

## Sloppy way to explore quantitative output for results sec. in ref to figs. 4 and 5
xx <- purrr::map2(.x = list(Jr.inv.std, Jr.inv.std.combined),
                  .y = list(fishing.res, fishing.res.combined), 
                  .f = ~ get_indirect_res(.x, .y))[[1]]
yy <- purrr::map2(.x = list(Jr.inv.std, Jr.inv.std.combined),
              .y = list(fishing.res, fishing.res.combined), 
              .f = ~ get_indirect_res(.x, .y))[[2]]

group_by(xx, Prey, Source) %>%
  summarize(median(change), low = quantile(change, 0.25), high = quantile(change, 0.75)) %>% 
  filter(low > 0 | high < 0)

group_by(yy, Prey, Source) %>%
  summarize(median(change), low = quantile(change, 0.25), high = quantile(change, 0.75)) %>% 
  filter(low > 0 | high < 0)

png('Figs/foodweb.png', width = 7, height = 7, units = 'in', res = 500)
test$Group <- gsub('croaker.spot.perch', 'sm.sciaenid', test$Group) %>%
  gsub('diving.birds', 'gulls.terns', .) %>%
  gsub('marsh.birds', 'wading.birds', .) %>%
  gsub('panaeid', 'penaeid', .)
ggwebplot(test, labels = TRUE, point.size = 2, text.size = 4, max.overlaps = 15) +
  theme_classic() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
dev.off()


# Map ---------------------------------------------------------------------

coarse.coastline.us <- rnaturalearth::ne_states(returnclass = "sf", 
                              country = 'United States of America', )
coarse.coastline.mx <- rnaturalearth::ne_countries(returnclass = 'sf',
                                                   country = 'Mexico', scale = 10)
detailed.coastline <- st_read('Data/United_States_Coastline.shp')
st_crs(detailed.coastline) <- st_crs(coarse.coastline.us)


big.area.map <- ggplot() +
  geom_sf(data = coarse.coastline.us) +
  geom_sf(data = coarse.coastline.mx) +
  coord_sf(xlim = c(-100, -80), ylim = c(22.5, 35)) +
  annotate(geom = 'rect', xmin = -90.2, xmax = -89, ymin = 28.95, ymax = 30, 
           fill = NA, col = 'red') +
  theme_void() +
  theme(panel.border = element_rect(fill = NA, color = 'gray40'))


small.area.map <- ggplot(detailed.coastline) + 
  geom_sf() +
  coord_sf(xlim = c(-90.2, -89), ylim = c(28.95,30)) +
  theme_classic() +
  labs(x = '', y = '') +
  annotate(geom = 'segment', xend = -89.95, x = -89.7, yend = 29.38, y = 29.15,
           arrow = arrow(length = unit(0.15, 'inches'))) +
  annotate(geom = 'text', label = 'Barataria\nBay', x = -89.7, y = 29.15, 
           vjust = 1) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.3)

png('Figs/map.png', width = 7, height = 7, units = 'in', res = 500)
cowplot::ggdraw(small.area.map) +
  cowplot::draw_plot(big.area.map, x = .73, y = .73, width = 0.26, height = 0.26)
dev.off()