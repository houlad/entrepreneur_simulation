library(tidyverse)
library(ggsci)
library(pins)
library(patchwork)
library(gt)

# Read in the saved data from pins. It will be in a list of dataframes that can
# be accessed by the investment structure; generally this is defined as so:
# single investment vs multiple investment
# small reinvestment, medium reinvestment, large reinvestment
# realistic investment table(default in all reinvestment dfs), stingy investment table, win or lose investment table
# normal investor (default) vs good investor(return percentages for all values except 0 return are 20% higher)
data_board <- pins::board_folder(path = 'data', versioned = TRUE)

all_simulations <- map(list.files('data/'), ~pin_read(data_board, .x) |>  mutate(id = .x))

# label the data frames within the list with their simulation type
names(all_simulations) <- list.files('data/')

################## all functions needed to build return metrics tables

calculate_return_rate_metrics <- function(df){
  # params: df - a tibble containing round by round simulation results
  # returns: a single row tibble containing binned return percentages
  
  df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |>  
    mutate(end_wealth = total_realized_profit + current_bankroll,
           end_profit_or_loss = end_wealth - start_capital,
           perc_return = (end_profit_or_loss/start_capital) *100) |> 
    group_by(id) |> 
    summarise(perc_lost_money = mean(perc_return < 0),
              zero_to_ten = mean(perc_return >= 0 & perc_return <= 10),
              ten_to_one_hundred = mean(perc_return > 10 & perc_return <= 100),
              one_hundred_to_one_thousand = mean(perc_return > 100 & perc_return <= 1000),
              one_thousand_to_ten_thousand = mean(perc_return > 1000 & perc_return <= 10000),
              greater_than_ten_thousand = mean(perc_return > 10000)) |> 
    mutate_if(is.numeric, ~round(.x * 100, 4))
}


calculate_return_metrics_investors <- function(df){
  # params: df - a tibble containing round by round simulation results
  # returns: a single row tibble containing the number of investors that fall in each
  #          return rate bin
  
  
  df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |>  
    mutate(end_wealth = total_realized_profit + current_bankroll,
           end_profit_or_loss = end_wealth - start_capital,
           perc_return = end_profit_or_loss/start_capital *100) |> 
    group_by(id) |> 
    summarise(perc_lost_money = sum(perc_return < 0),
              zero_to_ten = sum(perc_return >= 0 & perc_return <= 10),
              ten_to_one_hundred = sum(perc_return > 10 & perc_return <= 100),
              one_hundred_to_one_thousand = sum(perc_return > 100 & perc_return <= 1000),
              one_thousand_to_ten_thousand = sum(perc_return > 1000 & perc_return <= 10000),
              greater_than_ten_thousand = sum(perc_return > 10000)) 
}


calculate_profit_metrics<- function(df){
  # params: df - a tibble containing round by round simulation results
  # returns: a single row tibble containing various summary measures of wealth
  
  df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |>  
    mutate(end_wealth = total_realized_profit + current_bankroll,
           end_profit_or_loss = end_wealth - start_capital,
           perc_return = end_profit_or_loss/start_capital *100) |> 
    group_by(id) |> 
    summarise(median_profit = median(end_profit_or_loss),
              mean_profit = mean(end_profit_or_loss),
              sd_profit = sd(end_profit_or_loss),
              max_profit = max(end_profit_or_loss),
              median_end_wealth = median(end_wealth),
              mean_end_wealth = mean(end_wealth),
              max_end_wealth = max(end_wealth),
              median_perc_return = median(perc_return),
              mean_perc_return = mean(perc_return),
              sd_perc_return = sd(perc_return)) 
}


bin_dollar_value_returns <- function(df){
  # params: df - a tibble containing round by round simulation results
  # returns: a single row tibble containing counts of the number of investors
  #          with wealth falling in each bin
  
  df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |> 
    mutate(end_wealth = (total_realized_profit + current_bankroll)) |> 
    group_by(id) |> 
    summarise(less_than_million = sum(end_wealth < 1000000),
              Millionaire = sum(end_wealth >= 1000000 & end_wealth < 10000000),
              Decamillionaire = sum(end_wealth >= 10000000 & end_wealth < 100000000),
              Centimillionaire = sum(end_wealth >=100000000 & end_wealth < 1000000000),
              Billionaire = sum(end_wealth >= 1000000000 & end_wealth < 10000000000),
              Decabillionaire = sum(end_wealth >= 10000000000))
}

lost_made_money <- function(df){
  # params: df - a tibble containing round by round simulation results
  # returns: a single row tibble containing the count of investors who either made 
  #          or lost money in their simulation
  
  df |> 
    group_by(sim_id) |> 
    filter(round == max(round)) |> 
    ungroup() |> 
    mutate(end_wealth = (total_realized_profit + current_bankroll) - start_capital) |> 
    group_by(id) |> 
    summarise(lost_money = sum(end_wealth < start_capital),
              made_money = sum(end_wealth > start_capital))
}


extract_sim_identifiers <- function(df){
  # creates identifier columns describing the type of simulation; comprised of:
  # investment_capital: small, medium, large
  #   - whether 'investor' starts with 1, 10 or 20 investments worth of capital
  #
  # investment_table: realistic, stingy, win_lose
  #   - describes which returns table (returns, probabilities) the simulation was run under
  #   - realistic: (0, 1, 3, 5, 10, 20), (.64, .18, .06, .07, .03, .02)
  #   - stingy: (0, 1, 10),(.8, .15, .05)
  #   - win_lose: (0, 50), (.99, .01)
  #
  # investor_type: good, normal
  #   - good: investor has 20% higher probability to return a positive amount.
  #        - eg probabilities for stingy investment table become (.76, .18, .06)
  #   - normal: base probabilities from the return tables are active
  #
  # sim_label: uses the first letter of each identifier to succinctly identify a simulation
  #   - eg a small investment, with a realistic table with normal investor is srn
  df |> 
    mutate(
      investment_capital = case_when(
        grepl("^single", id) ~ 'small',
        grepl("^multiple", id) ~ 'medium',
        grepl("^big", id) ~ 'large'
      ),
      investment_table = case_when(
        grepl("realistic|reinvest", id) ~ 'realistic',
        grepl("stingy", id) ~ 'stingy',
        grepl("win", id) ~ 'boom-bust'
      ),
      investment_capital_multiple = case_when(
        grepl("^single", id) ~ "1x",
        grepl("^multiple", id) ~ "10x",
        grepl("^big", id) ~ "20x"
      ),
      investor_type = if_else(grepl("good", id), 'Good', 'Normal'),
      sim_label = paste0(toupper(str_extract(investment_capital, "")), toupper(str_extract(investment_table, "")), 
                         toupper(str_extract(investor_type, ""))),
      investment_capital_multiple = factor(investment_capital_multiple, ordered = TRUE,
                                           levels = c('20x', '10x', '1x'))
    )
}

############## building return metrics dataframes 

# loop through each simulation and calculate the specified metric set. Bind them into one df
returns_data <- map(all_simulations, ~calculate_return_rate_metrics(.x)) |> list_rbind()
profits_data <- map(all_simulations, ~calculate_profit_metrics(.x)) |>  list_rbind()
investor_data <- map(all_simulations, ~calculate_return_metrics_investors(.x)) |> list_rbind()
millionaire_data <- map(all_simulations, ~ bin_dollar_value_returns(.x)) |> list_rbind()
winner_loser_data <- map(all_simulations, ~lost_made_money(.x)) |> list_rbind()


# Relabel various features
profits_data <- profits_data |> extract_sim_identifiers()
returns_data <- returns_data |>  extract_sim_identifiers()
millionaire_data <- millionaire_data |>  extract_sim_identifiers()
investor_data <- investor_data |> extract_sim_identifiers()
winner_loser_data <- winner_loser_data |> extract_sim_identifiers()

############ build return metric tables (all simulations except those with different reinvestment percentages)
table_styling <- function(tbl, percent_format = FALSE){
  # lightly styles a gt object
  final_tbl <- tbl |> 
    tab_style(
      style = list(
        cell_text(weight = 'bold',
                  transform = 'capitalize')
      ),
      locations = cells_column_labels(everything())
    )
  
  if(percent_format){
    final_tbl |> 
      fmt_percent(columns = where(is.numeric),
                  decimals = 2, scale_values = FALSE)
  } else{
    final_tbl |> 
      fmt_number(columns = where(is.numeric),
                 decimals = 0)
  }
}


# table of return percentages 
returns_table <- returns_data |> 
  select(-sim_label, -investment_capital) |> 
  filter(id != 'single_investment_medium_reinvest',
         id != 'single_investment_large_reinvest', id != 'multiple_investment_medium_reinvest',
         id != 'multiple_investment_large_reinvest', investor_type != 'Good') |> 
  janitor::clean_names() |> 
  select(-id, -investor_type) |> 
  relocate(investment_table, investment_capital_multiple, perc_lost_money, zero_to_ten, ten_to_one_hundred,
           one_hundred_to_one_thousand, one_thousand_to_ten_thousand, greater_than_ten_thousand) |> 
  rename(investment_capital = investment_capital_multiple,
         '<0%' = perc_lost_money,
         '0-10%' = zero_to_ten,
         '10-100%' = ten_to_one_hundred,
         '100-1,000%' = one_hundred_to_one_thousand,
         '1,000-10,000%' = one_thousand_to_ten_thousand,
         '>10,000%' = greater_than_ten_thousand 
  ) |> 
  gt() |> 
  table_styling(percent_format = TRUE)
gtsave(returns_table, filename = 'returns_table.png')



# table of investor counts at binned wealth levels
millionaire_table <- millionaire_data |> 
  select(-sim_label, -Decabillionaire, -investment_capital) |> 
  filter(investment_table == 'realistic', id != 'single_investment_medium_reinvest',
         id != 'single_investment_large_reinvest', id != 'multiple_investment_medium_reinvest',
         id != 'multiple_investment_large_reinvest', investor_type == 'Normal') |> 
  janitor::clean_names() |> 
  select(-id, -investor_type) |> 
  relocate(investment_table, investment_capital_multiple, less_than_million, millionaire,
           decamillionaire, centimillionaire, billionaire) |> 
  rename(investment_capital = investment_capital_multiple,
         '<Millionaire' = less_than_million) |> 
  gt() |> 
  table_styling(percent_format = FALSE)
gtsave(millionaire_table, filename = 'millionaire_table.png')


# table of investor counts for those that made v lost money
winner_loser_table <- winner_loser_data |> 
  select(-sim_label, -investment_capital) |> 
  filter(investment_table == 'realistic', id != 'single_investment_medium_reinvest',
         id != 'single_investment_large_reinvest', id != 'multiple_investment_medium_reinvest',
         id != 'multiple_investment_large_reinvest', investor_type == 'Normal') |> 
  janitor::clean_names() |> 
  select(-id, -investor_type) |> 
  relocate(investment_table, investment_capital_multiple, lost_money, made_money) |> 
  rename(invesment_capital = investment_capital_multiple) |> 
  gt() |> 
  table_styling(percent_format = FALSE)
gtsave(winner_loser_table, filename = 'winner_loser_table.png')