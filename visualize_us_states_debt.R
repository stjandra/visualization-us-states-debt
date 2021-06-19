#!/usr/bin/env Rscript

library(robotstxt)
library(rvest)
library(svglite)
library(tidyverse)

#################################################################
##                         Scrape Data                         ##
#################################################################

url <- "https://www.gobankingrates.com/making-money/states-least-amount-debt/"

# Respect robots.txt.
if (!paths_allowed(url)) {
  write("Error, robots.txt disallow", stderr())
  quit(status=1)
}

page <- read_html(url)

# Each state is in ".listicle--slide".
# State name in "<h2>".
# Liabilities, assets, debt ratio in "<li>".
df <- html_nodes(page, ".listicle--slide") %>%
  map_df(~{
    tibble(
      State = html_node(.x, "h2") %>% html_text(trim = TRUE),
      Data = html_nodes(.x, "ul > li") %>% html_text(trim = TRUE)
    )
  })

##################################################################
##                          Preprocess                          ##
##################################################################

# Remove non breaking space.
df$Data <- df$Data %>% gsub(pattern = "\u00A0", replacement = "", fixed = TRUE)

# Filter rows. Separate variable names and value.
df <- df %>%
  filter(grepl("Total assets:|Total liabilities:|Debt ratio:", Data)) %>%
  separate(Data, into = c("Variable", "Value"), sep = ":")

# State column: remove number.
df$State <- gsub("[0-9]*\\. ", "", df$State)

# Value column:
# - Remove "percent", "$", and "billion".
# - Convert to numeric.
# - Round to two decimal places.
df$Value <- df$Value %>%
  gsub(pattern = "percent", replacement = "") %>%
  gsub(pattern = "billion", replacement = "") %>%
  gsub(pattern = "\\$", replacement = "") %>%
  as.numeric() %>%
  round(2)

# Add a new column to label a state's rank on each variable.
df <- df %>%
  group_by(Variable) %>%
  mutate(Rank = rank(-Value, ties.method = "min"))

##################################################################
##                             Plot                             ##
##################################################################

# Add an asterisk to Alabama because they use numbers from an older report.
df$State[df$State == "Alabama"] <- "*Alabama"

# Get the order of state by debt ratio.
stateOrder <- df %>% filter(Variable == "Debt ratio") %>% arrange(Value) %>% .$State

# Order the variables by plot appearance.
variableLevels <- c("Debt ratio", "Total liabilities" ,"Total assets")
variableLabels <- c("Debt ratio (percent)",
                    "Total liabilities (USD billion)",
                    "Total assets (USD billion)")
df$Variable <- factor(df$Variable,
                      levels = variableLevels,
                      labels = variableLabels)

# Dummy data frame containing 2 rows: an asset and a liability.
# The asset row will have the value of the largest liability.
# The liability row will have the value of the largest asset.
# This dummy data is used to make the axis limit of assets and liabilities the same.
# So that assets and liabilities have the same scale.
dummy <- data.frame(State = c("California", "California"), # Pick any state.
                    Variable = factor(c("Total assets", "Total liabilities"),
                                      levels = variableLevels,
                                      labels = variableLabels),
                    Value = c(max(df %>% filter(Variable == "Total liabilities (USD billion)") %>% .$Value),
                              max(df %>% filter(Variable == "Total assets (USD billion)") %>% .$Value)))

# Assets and liabilities have the same color (because they are the same unit and have same scale).
# But debt ratio has a different color.
pal <- c("#E7298A", "#29E786", "#29E786")

# Create the plot.
p <- ggplot(data = df,
            # Order the state.
            aes(x = factor(State, level = stateOrder), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() +
  geom_blank(data=dummy) +
  facet_grid(.~Variable, scales = "free") +
  # Add rank.
  geom_text(aes(label = Rank, y = 0), hjust = 1, size = 2.9) +
  scale_fill_manual(values = pal) +
  labs(title = "Debt of U.S. States Ranked - 2017",
       subtitle  = "debt ratio = (total liabilities + deferred inflows) / (total assets + deferred outflows)",
       caption = "Data source: Andrew DePietro (2018) - https://www.gobankingrates.com/making-money/states-least-amount-debt/
          The numbers next to the bars indicate the states rank for that variable.
          *Alabama data are from fiscal year 2016.") +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

filename = "us-states-debt.svg"
ggsave(filename, width=11, height=7)
write(sprintf("Created file %s", filename), stdout())
