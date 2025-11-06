library(tidyverse)
library(ggsci)
library(pins)
library(patchwork)

# Load all simulation results
data_board <- pins::board_folder(path = 'data', versioned = TRUE)

all_simulations <- map(list.files('data/'), ~pin_read(data_board, .x) |>  mutate(id = .x))

# label the data frames within the list with their simulation type
names(all_simulations) <- list.files('data/')

# plot base theming
my_plot_theme <- function(base_family="Karla", 
                          base_size =12,
                          plot_title_family='Karla',
                          plot_title_size = 16,
                          grid_col='#dadada') { 
  aplot <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size) #piggyback on theme_minimal 
  aplot <- aplot + theme(panel.grid=element_line(color=grid_col))
  aplot <- aplot + theme(plot.title=element_text(size=plot_title_size, 
                                                 family=plot_title_family))
  aplot <- aplot + theme(axis.ticks = element_blank())
  aplot
}


####### Racing Plots
plot_racing_graph <- function(df, title = NULL,
                              subtitle = NULL,
                              .x_label = NULL,
                              .y_label = NULL){
  
  # Identify the 5 sims that returned the highest total dollar amount
  highlighted_values <- df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |> 
    slice_max(total_realized_profit + current_bankroll, n = 5) |> 
    pull(sim_id)
  
  # TODO document functionality
  df |> 
    ggplot()+
    geom_line(aes(x = round, y = total_realized_profit+current_bankroll, group = sim_id, color = sim_id, alpha = .25))+
    gghighlight::gghighlight(sim_id %in% highlighted_values, max_highlight = 5,
                             use_group_by = FALSE,
                             unhighlighted_params = list(colour = alpha('grey', .25)),
                             use_direct_label = FALSE)+
    geom_hline(yintercept = 100000000, linetype = 'dotted', color = 'darkred')+
    geom_hline(yintercept = 1000000000, linetype = 'dashed')+
    scale_y_continuous(labels = scales::number_format(big.mark = ","))+
    scale_x_continuous(expand = expansion(mult = c(0, .05)))+
    theme_minimal()+
    labs(
      x = .x_label,
      y = .y_label,
      title = title,
      subtitle = subtitle
    )+
    my_plot_theme() + 
    theme(
      legend.position = 'None'
    )
}


build_stacked_racing_plot <- function(list_dfs, plot_title = NULL,
                                      p1_subtitle = NULL,
                                      p2_subtitle = NULL,
                                      p3_subtitle = NULL,
                                      y_label = NULL,
                                      x_label = NULL){
  dfs <- all_simulations |> 
    purrr::keep(names(all_simulations) %in% list_dfs) 
  
  p1 <- dfs[list_dfs][[1]] |> 
    plot_racing_graph(title = plot_title, subtitle = p1_subtitle)
  p2 <- dfs[list_dfs][[2]] |> 
    plot_racing_graph(subtitle = p2_subtitle, .y_label = y_label)
  p3 <- dfs[list_dfs][[3]] |> 
    plot_racing_graph(subtitle = p3_subtitle, .x_label = x_label)
  
  p1/p2/p3

  
}

# starting capital racing plots
starting_capital_racing_plot <- 
  build_stacked_racing_plot(c("single_investment_small_reinvest", 'multiple_investment_small_reinvest',
                            "big_multiple_investment_realistic_investment_table"),
                          plot_title = 'Investment results of 1x, 10x, and 20x starting capital(realistic investment table)',
                          p1_subtitle = 'Starting capital equal to 1 investment',
                          p2_subtitle = 'Starting capital equal to 10 investments',
                          p3_subtitle = 'Starting capital equal to 20 investments',
                          y_label = 'Total Investment Income($)',
                          x_label = 'Investment Rounds')
ggsave('starting_capital_racing_plot.png', plot = starting_capital_racing_plot)

# different investment tables 1x investment
investment_tables_racing_plot <- 
  build_stacked_racing_plot(c("single_investment_small_reinvest", 'single_investment_stingy_investment_table',
                              "single_investment_win_or_lose"),
                            plot_title = 'Investment results of 1x starting capital with \n realistic, stingy, and boom-bust investment tables',
                            p1_subtitle = 'Realistic investment table',
                            p2_subtitle = 'Stingy investment table',
                            p3_subtitle = 'Boom-Bust investment table',
                            y_label = 'Total Investment Income($)',
                            x_label = 'Investment Rounds')
ggsave('investment_tables_racing_plot.png', investment_tables_racing_plot)


# different investment tables 10x investment
investment_tables_multiple_racing_plot <- 
  build_stacked_racing_plot(c("multiple_investment_small_reinvest", 'multiple_investment_stingy_investment_table',
                              "multiple_investment_win_or_lose"),
                            plot_title = 'Investment results of 10x starting capital with \n realistic, stingy, and boom-bust investment tables',
                            p1_subtitle = 'Realistic investment table',
                            p2_subtitle = 'Stingy investment table',
                            p3_subtitle = 'Boom-Bust investment table',
                            y_label = 'Total Investment Income($)',
                            x_label = 'Investment Rounds')
ggsave('investment_tables_multiple_racing_plot.png', investment_tables_multiple_racing_plot)

# single investment normal v good
srn <- all_simulations$single_investment_small_reinvest |> 
  plot_racing_graph(title = 'Investment results of 1x starting capital with realistic investment table',
                    subtitle = 'Normal investor')
srg <- all_simulations$single_investment_realistic_investment_table_good_investor |> 
  plot_racing_graph(subtitle = 'Good investor',
                    .x_label = "Investment Rounds",
                    .y_label = 'Total Investment Income($)')

single_normal_v_good <- srn/srg
ggsave('single_normal_v_good.png', single_normal_v_good)

# multiple investment normal v good
mrn <- all_simulations$multiple_investment_small_reinvest |> 
  plot_racing_graph(title = 'Investment results of 10x starting capital with realistic investment table',
                    subtitle = 'Normal investor')
mrg <- all_simulations$multiple_investment_realistic_investment_table_good_investor |> 
  plot_racing_graph(subtitle = 'Good investor',
                    .x_label = "Investment Rounds",
                    .y_label = 'Total Investment Income($)')

multiple_normal_v_good <- mrn/mrg
ggsave('multiple_normal_v_good.png', multiple_normal_v_good)

# single investment, different reinvestment percentages
single_small_medium_large_reinvestment <- 
  build_stacked_racing_plot(c("single_investment_small_reinvest", 'single_investment_medium_reinvest',
                              "single_investment_large_reinvest"),
                            plot_title = 'Investment results of 1x starting capital with realistic investment tables',
                            p1_subtitle = 'Small reinvestment',
                            p2_subtitle = 'Medium reinvestment',
                            p3_subtitle = 'Large reinvestment',
                            y_label = 'Total Investment Income($)',
                            x_label = 'Investment Rounds')
ggsave('single_small_medium_large_reinvestment.png', single_small_medium_large_reinvestment)


################ Histograms
plot_histogram <- function(df, title = NULL, subtitle = NULL,
                           .x_label = NULL, .y_label = NULL, ...){
  df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |> 
    ggplot()+
    geom_histogram(aes((total_realized_profit + current_bankroll)-start_capital),
                   color = 'white', alpha = .8, ...)+
    scale_x_continuous(labels = scales::number_format(big.mark = ','))+
    scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','))+
    labs(
      x = .x_label,
      y = .y_label,
      title = title,
      subtitle = subtitle
    )+
    theme_minimal()+
    my_plot_theme()
}
  

# 1x Investment Realistic Table
h1 <- plot_histogram(all_simulations$single_investment_small_reinvest, bins = 100, fill = "#0072B5FF",
               title = 'Investment simulations show extreme bimodality with a pronounced right skew',
               subtitle = '1x starting capital with realistic investment table')+
  scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','),
                limits = c(1, 100000))

#10x Investment Realistic Table
h2 <- plot_histogram(all_simulations$multiple_investment_small_reinvest, bins = 100, fill = "#20854EFF",
               subtitle = '10x starting capital with realistic investment table',
               .y_label = 'Total Investors(Log Scale)')+
  scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','),
                limits = c(1, 100000))


#20x Investment Realistic Table
h3 <- plot_histogram(all_simulations$big_multiple_investment_realistic_investment_table, bins = 100, fill = "#BC3C29FF",
               subtitle = '20x starting capital with realistic investment table',
               .x_label = 'Total Investment Profit($)')+
  scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','),
                limits = c(1, 100000))

starting_capital_histogram <- h1/h2/h3
ggsave('starting_capital_histogram.png', starting_capital_histogram)


# 1x Good Investor Realistic Table
h1g <- plot_histogram(all_simulations$single_investment_realistic_investment_table_good_investor, bins = 100, fill = "#0072B5FF",
               title = 'Good investors shift the bimodal distributions rightward \n but otherwise do not change the results',
               subtitle = '1x starting capital with realistic investment table, good investor')+
  scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','),
                limits = c(1, 100000))

# 10x Good Investor Realistic Table
h2g <- plot_histogram(all_simulations$multiple_investment_realistic_investment_table_good_investor, bins = 100, fill = "#20854EFF",
                     subtitle = '10x starting capital with realistic investment table, good investor',
                     .y_label = 'Total Investors(Log Scale)')+
  scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','),
                limits = c(1, 100000))

#20x Good Investor  Realistic Table
h3g <- plot_histogram(all_simulations$big_multiple_investment_realistic_investment_table_good_investor, bins = 100, fill = "#BC3C29FF",
                     subtitle = '20x starting capital with realistic investment table, good investor',
                     .x_label = 'Total Investment Profit($)')+
  scale_y_log10(breaks = scales::breaks_log(base = 10, n = 10),labels = scales::number_format(big.mark = ','),
                limits = c(1, 100000))

starting_capital_good_investor_histogram <- h1g/h2g/h3g
ggsave('starting_capital_good_investor_histogram.png', starting_capital_good_investor_histogram)


############## Bar Graphs
plot_returns_bar_graph <- function(df, x_variable, title = NULL, subtitle = NULL, add_legend = FALSE){
  
  plot <- df|> 
    select({{x_variable}}, where(is.numeric)) |> 
    pivot_longer(-{{x_variable}}, names_to = 'return_amount', values_to = 'value') |>
    mutate(return_amount = case_when(
      return_amount == 'perc_lost_money' ~ '<0%',
      return_amount == 'zero_to_ten' ~ '0-10%',
      return_amount == 'ten_to_one_hundred' ~ '10-100%',
      return_amount == 'one_hundred_to_one_thousand' ~ '100-1,000%',
      return_amount == 'one_thousand_to_ten_thousand' ~ '1,000-10,000%',
      return_amount == 'greater_than_ten_thousand' ~ '>10,000%'
    )) |> 
    mutate(return_amount = factor(return_amount, ordered = TRUE, 
                                  levels = c("<0%", "0-10%",
                                             "10-100%", "100-1,000%",
                                             "1,000-10,000%", ">10,000%"))) |> 
    ggplot(aes(x = {{x_variable}}, y = value, fill = fct_rev(return_amount)))+
    geom_bar(position = 'fill', stat = 'identity', alpha = .9)+
    scale_fill_nejm()+
    theme_minimal()+
    scale_y_continuous(expand = expansion(add = c(0, 0)),
      labels = scales::percent_format())+
    scale_x_discrete(expand = expansion(add = c(0.5,0)))+
    labs(
      x = "Starting Capital",
      y = "",
      title = title,
      subtitle = subtitle,
      fill = '% Return'
    )+
    my_plot_theme()+
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank())
  
  if(add_legend){
    plot <- plot + theme(legend.position = 'bottom',
                 legend.box = 'horizontal',
                 legend.title.position = 'left')+
      guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  }
  plot
}


# load bar graph data from pins
returns_data <- pins::pin_read(data_board, 'returns_data')
millionaire_data <- pins::pin_read(data_board, 'millionaire_data')
winner_loser_data <- pins::pin_read(data_board, 'winner_loser_data')

######### realistic table, 1x, 10x, 20x capital; Return Rate plot
realistic_bar_plot <- returns_data |> 
  filter(id %in% c('single_investment_small_reinvest', 
                   'multiple_investment_small_reinvest', 
                   'big_multiple_investment_realistic_investment_table')) |>
  plot_returns_bar_graph(x_variable = investment_capital_multiple, title = 'What rate of return do investors earn?',
                         subtitle = 'Realistic Table')

stingy_bar_plot <- returns_data |> 
  filter(id %in% c('single_investment_stingy_investment_table', 
                   'multiple_investment_stingy_investment_table', 
                   'big_multiple_investment_stingy_investment_table')) |>
  plot_returns_bar_graph(x_variable = investment_capital_multiple, subtitle = 'Stingy Table',
                         add_legend = TRUE)

boom_bust_bar_plot <- returns_data |> 
  filter(id %in% c('single_investment_win_or_lose',
                   'multiple_investment_win_or_lose', 
                   'big_multiple_investment_win_or_lose')) |>
  plot_returns_bar_graph(x_variable = investment_capital_multiple, subtitle = 'Boom-Bust Table')

realistic_returns_plot <- realistic_bar_plot+stingy_bar_plot+boom_bust_bar_plot
ggsave("realistic_returns_plot.png", realistic_returns_plot, height = 10, width = 10)

########## realistic table, good v normal investor return rate plot
single_good_v_normal_investor <- returns_data |> 
  filter(id %in% c('single_investment_small_reinvest', 
                   'single_investment_realistic_investment_table_good_investor')) |>
  plot_returns_bar_graph(x_variable = investor_type, subtitle = '1x Starting Capital')

multiple_good_v_normal_investor <- returns_data |> 
  filter(id %in% c('multiple_investment_small_reinvest', 
                   'multiple_investment_realistic_investment_table_good_investor')) |>
  plot_returns_bar_graph(x_variable = investor_type, subtitle = '10x Starting Capital',
                         add_legend = TRUE)

big_multiple_good_v_normal_investor <- returns_data |> 
  filter(id %in% c('big_multiple_investment_realistic_investment_table', 
                   'big_multiple_investment_realistic_investment_table_good_investor')) |>
  plot_returns_bar_graph(x_variable = investor_type, subtitle = '20x Starting Capital',
                         title = 'Do Good Investors Make Up for Lower Starting Capital')

good_v_normal_return_rate_plot <- big_multiple_good_v_normal_investor + multiple_good_v_normal_investor+single_good_v_normal_investor
ggsave("good_v_normal_return_rate_plot.png", good_v_normal_return_rate_plot)


########## millionaire bar graphs
plot_millionaire_bar_graph <- function(df, x_variable, title = NULL, subtitle = NULL, add_legend = FALSE){
  
  plot <- df|> 
    select({{x_variable}}, where(is.numeric)) |> 
    pivot_longer(-{{x_variable}}, names_to = 'return_amount', values_to = 'value') |>
    mutate(return_amount = case_when(
      return_amount == "less_than_million" ~ "< Millionaire",
      .default = return_amount
    )) |> 
    mutate(return_amount = factor(return_amount, ordered = TRUE,
                                  levels = c("< Millionaire", "Millionaire", "Decamillionaire", "Centimillionaire",
                                             "Billionaire", "Decabillionaire"))) |> 
    ggplot(aes(x = {{x_variable}}, y = value, fill = fct_rev(return_amount)))+
    geom_bar(position = 'fill', stat = 'identity', alpha = .9)+
    scale_fill_nejm()+
    theme_minimal()+
    scale_y_continuous(expand = expansion(add = c(0,0)),
                       labels = scales::percent_format())+
    scale_x_discrete(expand = expansion(add = c(0.5, 0)))+
    labs(
      x = "Starting Capital",
      y = "",
      title = title,
      subtitle = subtitle,
      fill = 'Final Wealth'
    )+
    my_plot_theme()+
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank())
  
  if(add_legend){
    plot <- plot + theme(legend.position = 'bottom',
                         legend.box = 'horizontal',
                         legend.title.position = 'left')+
      guides(fill = guide_legend(nrow = 1, reverse = TRUE))
  }
  plot
}

millionaire_bar_plot <- millionaire_data |> 
  filter(id %in% c('single_investment_small_reinvest', 
                   'multiple_investment_small_reinvest', 
                   'big_multiple_investment_realistic_investment_table')) |> 
  select(investment_capital_multiple, where(is.numeric)) |>
  # pivot_longer(-investment_capital_multiple, names_to = 'return_amount', values_to = 'value' ) |> 
  plot_millionaire_bar_graph(x_variable = investment_capital_multiple, add_legend = TRUE,
                             title = "What proportion of investors finish with total wealth in the millions, billions, etc?",
                             subtitle = "Investors with more starting capital finish with higher wealth")
ggsave("millionaire_bar_plot.png", millionaire_bar_plot)


######## All simulations together millionaire bar graph
total_millionaire_bar_plot <- millionaire_data |> 
  filter(id != 'multiple_investment_large_reinvest', id != 'multiple_investment_medium_reinvest', 
         id != 'single_investment_large_reinvest', id != 'single_investment_medium_reinvest') |> 
  select(-Decabillionaire) |> 
  select(investment_capital_multiple, where(is.numeric)) |> 
  pivot_longer(-investment_capital_multiple, names_to = 'return_amount', values_to = 'value') |> 
  mutate(return_amount = case_when(
    return_amount == "less_than_million" ~ "< Millionaire",
    .default = return_amount),
    return_amount = factor(return_amount, ordered = TRUE,
                                levels = c("< Millionaire", "Millionaire", "Decamillionaire", "Centimillionaire",
                                           "Billionaire"))) |>
  ggplot(aes(x = return_amount, y = value, fill = investment_capital_multiple))+
  geom_bar(position = 'fill', stat = 'identity', alpha = .8)+
  scale_fill_nejm()+
  theme_minimal()+
  scale_y_continuous(expand = expansion(add = c(0,0)),
                     labels = scales::percent_format())+
  scale_x_discrete(expand = expansion(add = c(0.5, 0)))+
  labs(
    x = "",
    y = "",
    title = "Proportion of millionaires, billionaires, etc produced in all simulations \n by starting capital amount",
    subtitle = "~90% of billionaires came from simulation scenarios with 10x or 20x starting capital",
    fill = 'Starting Capital'
  )+
  my_plot_theme()+
  theme(legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.title.position = 'left', 
        panel.grid.major.x = element_blank())+
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

ggsave("total_millionaire_bar_plot.png", total_millionaire_bar_plot)


######### made/lost money bar graph
winner_loser_bar_plot <- winner_loser_data |> 
  filter(id %in% c('single_investment_small_reinvest', 
                   'multiple_investment_small_reinvest', 
                   'big_multiple_investment_realistic_investment_table')) |> 
  select(investment_capital_multiple, where(is.numeric)) |> 
  pivot_longer(-investment_capital_multiple, names_to = "return_amount", values_to = "value") |> 
  mutate(return_amount = case_when(
    return_amount == "lost_money" ~ "Lost Money",
    return_amount == "made_money" ~ "Made Money"
  )) |> 
  ggplot(aes(investment_capital_multiple, y = value, fill = fct_rev(return_amount)))+
  geom_bar(position = 'fill', stat = 'identity', alpha = .75)+
  scale_fill_nejm()+
  theme_minimal()+
  scale_y_continuous(expand = expansion(add = c(0,0)),
                     labels = scales::percent_format())+
  scale_x_discrete(expand = expansion(add = c(0.5, 0)))+
  labs(
    x = "",
    y = "",
    title = "What proportion of investors made money versus lost money?",
    subtitle = 'Realistic Table',
    fill = "Investment Returns"
  )+
  my_plot_theme()+
  theme(legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.title.position = 'left',
        panel.grid.major.x = element_blank())+
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))
ggsave("winner_loser_bar_plot.png", winner_loser_bar_plot)  


########## 1x first round winners millionaire plot
won_first_round <- all_simulations$single_investment_small_reinvest |> 
  filter(round == 1 & revenue_for_round == 20000000) |> 
  pull(sim_id)

first_round_winners_1x <- all_simulations$single_investment_small_reinvest |> 
  filter(sim_id %in% won_first_round) |> 
  bin_dollar_value_returns() |> 
  pivot_longer(-id, names_to = 'return_amount', values_to = 'value') |> 
  mutate(id = '1x', 
         return_amount = case_when(
    return_amount == "less_than_million" ~ "< Millionaire",
    .default = return_amount
  )) |> 
  mutate(return_amount = factor(return_amount, ordered = TRUE,
                                levels = c("< Millionaire", "Millionaire", "Decamillionaire", "Centimillionaire",
                                           "Billionaire", "Decabillionaire"))) |> 
  ggplot(aes(x = id, y = value, fill = fct_rev(return_amount)))+
  geom_bar(stat = 'identity', position = 'fill', alpha = .9)+
  scale_fill_nejm()+
  theme_minimal()+
  scale_y_continuous(expand = expansion(add = c(0,0)),
                     labels = scales::percent_format())+
  scale_x_discrete(expand = expansion(add = c(0.5, 0)))+
  labs(
    x = "",
    y = "",
    title = "Investment results for 1x starting capital investors who earn a 20x \n return on their first investment?",
    subtitle = 'Realistic Table',
    fill = "Final Wealth"
  )+
  my_plot_theme()+
  theme(legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.title.position = 'left',
        panel.grid.major.x = element_blank())+
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))
ggsave("first_round_winners_1x.png", first_round_winners_1x, height = 10, width = 10)

