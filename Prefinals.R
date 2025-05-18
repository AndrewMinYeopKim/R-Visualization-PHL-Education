library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(viridis)

OECD2018AverageScore <- data.frame(
  Subject = c("Reading", "Mathematics", "Science"),
  Score = c(487, 489, 489)
)
OECD2022AverageScore <- data.frame(
  Subject = c("Reading", "Mathematics", "Science"),
  Score = c(476, 472, 485)
)
DepEd_Budget.data <- data.frame(
  Year = c(2018,2019,2020,2021,2022,2023,2024,2025),
  Budget = c(580.6,528.8,500.0, 606.6, 631.8, 710.6, 717.0,737.1)
)

descriptive_stats_function <- function(){
desc2018 <- as.data.frame(PISA_2018_scores)
desc2022 <- as.data.frame(PISA_2022_scores)
desc2018$Year <- 2018
desc2022$Year <- 2022


# Bind rows together
combined_PISA <- bind_rows(desc2018, desc2022)

# Pivot longer for subject-wise analysis
PISA_long <- combined_PISA %>%
  pivot_longer(cols = c(Reading, Mathematics, Science), 
               names_to = "Subject", values_to = "Score") %>%
  filter(!is.na(Score))  # Remove NA scores

# Compute stats grouped by year and subject
descriptive_stats <- PISA_long %>%
  group_by(Year, Subject) %>%
  summarise(
    Min = min(Score),
    Q1 = quantile(Score, 0.25),
    Median = median(Score),
    Q3 = quantile(Score, 0.75),
    Max = max(Score),
    Mean = mean(Score),
    SD = sd(Score),
    Variance = var(Score),
    Range = diff(range(Score)),
    IQR = IQR(Score),
    .groups = "drop"
  )
return(descriptive_stats)}
descriptive_stats_function()

violin_plot_2018_function <-function(){
# Reshape PISA 2018
PISA_long <- PISA_2018_scores %>%
  pivot_longer(cols = c(Reading, Mathematics, Science),
               names_to = "Subject", values_to = "Score")

# Sample size
sample_size <- PISA_long %>%
  group_by(Subject) %>%
  summarise(n = n(), .groups = "drop")

# Add label and order
PISA_long <- PISA_long %>%
  left_join(sample_size, by = "Subject") %>%
  mutate(SubjectLabel = paste0(Subject, "\n", "n=", n))

subject_order <- PISA_long %>%
  group_by(SubjectLabel) %>%
  summarise(median_score = median(Score, na.rm = TRUE)) %>%
  arrange(desc(median_score)) %>%
  pull(SubjectLabel)

PISA_long <- PISA_long %>%
  mutate(SubjectLabel = factor(SubjectLabel, levels = subject_order))

# Min/max
extremes <- PISA_long %>%
  group_by(SubjectLabel) %>%
  slice_min(Score, n = 1) %>%
  rename(MinCountry = Country, MinScore = Score) %>%
  left_join(
    PISA_long %>%
      group_by(SubjectLabel) %>%
      slice_max(Score, n = 1) %>%
      rename(MaxCountry = Country, MaxScore = Score),
    by = "SubjectLabel"
  )

# Quantiles
quantiles <- PISA_long %>%
  group_by(SubjectLabel) %>%
  summarise(
    Q1 = quantile(Score, 0.25, na.rm = TRUE),
    Median = quantile(Score, 0.5, na.rm = TRUE),
    Q3 = quantile(Score, 0.75, na.rm = TRUE)
  )

# Violin plot
return(ggplot(PISA_long, aes(x = Score, y = SubjectLabel, fill = SubjectLabel)) +
  geom_violin(scale = "width", trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.1, color = "grey20", alpha = 0.2) +
  geom_point(data = extremes, aes(x = MinScore, y = SubjectLabel), 
             shape = 21, fill = "red", size = 3, inherit.aes = FALSE) +
  geom_text(data = extremes, aes(x = MinScore, y = SubjectLabel, 
                                 label = paste0(MinCountry, "\n", round(MinScore))), 
            hjust = 1.1, vjust = -0.3, size = 3, color = "red", inherit.aes = FALSE) +
  geom_point(data = extremes, aes(x = MaxScore, y = SubjectLabel), 
             shape = 21, fill = "blue", size = 3, inherit.aes = FALSE) +
  geom_text(data = extremes, aes(x = MaxScore, y = SubjectLabel, 
                                 label = paste0(MaxCountry, "\n", round(MaxScore))), 
            hjust = -0.1, vjust = -0.3, size = 3, color = "blue", inherit.aes = FALSE) +
  geom_text(data = quantiles,
            aes(x = Median, y = SubjectLabel,
                label = paste("Median =", round(Median,1))),
            vjust = 2, size = 3, color = "black") +
  geom_text(data = quantiles, 
            aes(x = (Q1 + Q3)/2, y = SubjectLabel, 
                label = paste("IQR =", round(Q3 - Q1, 1))),
            vjust = -1.8, size = 3, color = "darkgreen") +
  
  scale_fill_manual(values = c("#AEC6CF", "#FFDAB9", "#B0E0E6")) +
  theme_ipsum(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  ggtitle("PISA 2018 Score Distribution with Min/Max and IQR Labels") +
  xlab("PISA Average Score"))}
violin_plot_2022_function <-function(){
  # Reshape PISA 2018
  PISA_long <- PISA_2022_scores %>%
    pivot_longer(cols = c(Reading, Mathematics, Science),
                 names_to = "Subject", values_to = "Score")
  
  # Sample size
  sample_size <- PISA_long %>%
    group_by(Subject) %>%
    summarise(n = n(), .groups = "drop")
  
  # Add label and order
  PISA_long <- PISA_long %>%
    left_join(sample_size, by = "Subject") %>%
    mutate(SubjectLabel = paste0(Subject, "\n", "n=", n))
  
  subject_order <- PISA_long %>%
    group_by(SubjectLabel) %>%
    summarise(median_score = median(Score, na.rm = TRUE)) %>%
    arrange(desc(median_score)) %>%
    pull(SubjectLabel)
  
  PISA_long <- PISA_long %>%
    mutate(SubjectLabel = factor(SubjectLabel, levels = subject_order))
  
  # Min/max
  extremes <- PISA_long %>%
    group_by(SubjectLabel) %>%
    slice_min(Score, n = 1) %>%
    rename(MinCountry = Country, MinScore = Score) %>%
    left_join(
      PISA_long %>%
        group_by(SubjectLabel) %>%
        slice_max(Score, n = 1) %>%
        rename(MaxCountry = Country, MaxScore = Score),
      by = "SubjectLabel"
    )
  
  # Quantiles
  quantiles <- PISA_long %>%
    group_by(SubjectLabel) %>%
    summarise(
      Q1 = quantile(Score, 0.25, na.rm = TRUE),
      Median = quantile(Score, 0.5, na.rm = TRUE),
      Q3 = quantile(Score, 0.75, na.rm = TRUE)
    )
  
  # Violin plot
 return(ggplot(PISA_long, aes(x = Score, y = SubjectLabel, fill = SubjectLabel)) +
    geom_violin(scale = "width", trim = FALSE, alpha = 0.4) +
    geom_boxplot(width = 0.1, color = "grey20", alpha = 0.2) +
    geom_point(data = extremes, aes(x = MinScore, y = SubjectLabel), 
               shape = 21, fill = "red", size = 3, inherit.aes = FALSE) +
    geom_text(data = extremes, aes(x = MinScore, y = SubjectLabel, 
                                   label = paste0(MinCountry, "\n", round(MinScore))), 
              hjust = 1.1, vjust = -0.3, size = 3, color = "red", inherit.aes = FALSE) +
    geom_point(data = extremes, aes(x = MaxScore, y = SubjectLabel), 
               shape = 21, fill = "blue", size = 3, inherit.aes = FALSE) +
    geom_text(data = extremes, aes(x = MaxScore, y = SubjectLabel, 
                                   label = paste0(MaxCountry, "\n", round(MaxScore))), 
              hjust = -0.1, vjust = -0.3, size = 3, color = "blue", inherit.aes = FALSE) +
    geom_text(data = quantiles,
              aes(x = Median, y = SubjectLabel,
                  label = paste("Median =", round(Median,1))),
              vjust = 2, size = 3, color = "black") +
    geom_text(data = quantiles, 
              aes(x = (Q1 + Q3)/2, y = SubjectLabel, 
                  label = paste("IQR =", round(Q3 - Q1, 1))),
              vjust = -1.8, size = 3, color = "darkgreen") +
    
    scale_fill_manual(values = c("#AEC6CF", "#FFDAB9", "#B0E0E6")) +
    theme_ipsum(base_size = 12) +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    ggtitle("PISA 2022 Score Distribution with Min/Max and IQR Labels") +
    xlab("PISA Average Score"))}
violin_plot_2018_function()
violin_plot_2022_function()

time_series_deped_budget <- function(){
#DepEd Budget
return(ggplot(DepEd_Budget.data, aes(x = Year, y = Budget)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE, linetype = "dashed") +
  geom_text(aes(label = Budget), vjust = -0.5, size = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "DepEd Budget Over the Years (2018–2025)",
    x = "Year",
    y = "Budget (Billion PHP)"))}
time_series_deped_budget()

cor_ph_vs_deped <- function(){
# Philippines trend over time
phil_2018 <- PISA_2018_scores %>% filter(Country == "Philippines") %>% mutate(Year = 2018)
phil_2022 <- PISA_2022_scores %>% filter(Country == "Philippines") %>% mutate(Year = 2022)
phil_scores <- bind_rows(phil_2018, phil_2022) %>%
  left_join(DepEd_Budget.data, by = "Year")

phil_scores_long <- phil_scores %>%
  pivot_longer(cols = c(Reading, Mathematics, Science), names_to = "Subject", values_to = "Score")


# Correlation graph
return(ggplot(phil_scores_long, aes(x = Budget, y = Score, color = Subject)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1.2) +
  geom_text(aes(label = Year), vjust = -1, size = 3) +
  scale_color_manual(values = c("steelblue", "darkred", "darkgreen")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Relationship Between DepEd Budget and PISA Scores (Philippines)",
    subtitle = "Dashed lines show linear regression per subject",
    x = "DepEd Budget (Billion PHP)",
    y = "PISA Score",
    color = "Subject"))}
cor_ph_vs_deped()

line_OECD_vs_PH_function <- function(){
#OECD average vs PH line 
oecd_scores <- bind_rows(
  OECD2018AverageScore %>% mutate(Year = 2018, Country = "OECD Average"),
  OECD2022AverageScore %>% mutate(Year = 2022, Country = "OECD Average")
)

ph_vs_oecd <- bind_rows(
  phil_scores_long %>% mutate(Country = "Philippines"),
  oecd_scores
)

# Line chart
ggplot(ph_vs_oecd, aes(x = Year, y = Score, color = Country, linetype = Country)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Subject, scales = "free_y") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = c("Philippines" = "steelblue", "OECD Average" = "orange")) +
  labs(
    title = "PISA Scores: Philippines vs OECD Average (2018 & 2022)",
    x = "Year",
    y = "Score")}
line_OECD_vs_PH_function()

scatter_all_scores_2018_function <- function(){
#scatter plot of all scores and line of OECD average
# Scatter plot of all scores by subject (2018)
PISA_long <- PISA_2018_scores %>%
  pivot_longer(cols = c(Reading, Mathematics, Science),
               names_to = "Subject", values_to = "Score")

# Add OECD 2018 average to use for reference lines
OECD_avg_lines <- OECD2018AverageScore %>% 
  rename(Average = Score)

# Plot
ggplot(PISA_long, aes(x = reorder(Country, Score), y = Score, color = Subject)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(data = OECD_avg_lines, aes(yintercept = Average, color = Subject),
             linetype = "dashed", size = 1) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "PISA 2018 Scores by Country with OECD Average Lines",
    x = "Country",
    y = "Score",
    color = "Subject")}
scatter_all_scores_2018_function()

scatter_all_scores_2022_function <- function(){
# Scatter plot of all scores by subject (2022)
PISA_long <- PISA_2022_scores %>%
  pivot_longer(cols = c(Reading, Mathematics, Science),
               names_to = "Subject", values_to = "Score")

# Add OECD 2022 average to use for reference lines
OECD_avg_lines <- OECD2022AverageScore %>% 
  rename(Average = Score)

# Plot
ggplot(PISA_long, aes(x = reorder(Country, Score), y = Score, color = Subject)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(data = OECD_avg_lines, aes(yintercept = Average, color = Subject),
             linetype = "dashed", size = 1) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "PISA 2022 Scores by Country with OECD Average Lines",
    x = "Country",
    y = "Score",
    color = "Subject")}
scatter_all_scores_2022_function()

bar_ph_vs_world_2018_function <- function() {
  # Similar-income countries to Philippines
  similar_income_countries <- c("Indonesia", "Thailand", "Vietnam", "Malaysia", "India", "Brazil")
  
  # Pivot to long format
  PISA_long <- PISA_2018_scores %>%
    pivot_longer(cols = c(Reading, Mathematics, Science), 
                 names_to = "Subject", values_to = "Score")
  
  # Identify min & max scoring countries per subject
  top_bottom <- PISA_long %>%
    group_by(Subject) %>%
    summarise(
      MaxCountry = Country[which.max(Score)],
      MinCountry = Country[which.min(Score)],
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(MaxCountry, MinCountry), values_to = "Country") %>%
    distinct(Subject, Country)
  
  # Countries to include
  selected_countries <- unique(c(
    "Philippines", "OECD Average",
    similar_income_countries,
    top_bottom$Country
  ))
  
  # Filter dataset to selected countries
  filtered_PISA <- PISA_long %>%
    filter(Country %in% selected_countries) %>%
    mutate(Highlight = case_when(
      Country == "Philippines" ~ "Philippines",
      Country == "OECD Average" ~ "OECD Average",
      Country %in% top_bottom$Country ~ "Top/Bottom Scorer",
      TRUE ~ "Similar-Income"
    ))
  
  # Plot
  return(
    ggplot(filtered_PISA, aes(x = reorder(Country, Score), y = Score, fill = Highlight)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Subject, scales = "free_y") +
      scale_fill_manual(values = c(
        "Philippines" = "red",
        "OECD Average" = "orange",
        "Top/Bottom Scorer" = "steelblue",
        "Similar-Income" = "gray80"
      )) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      labs(
        title = "Philippines vs OECD, Top/Bottom and Similar-Income Countries (PISA 2018)",
        x = "Country",
        y = "Score",
        fill = "Group"))}
bar_ph_vs_world_2018_function()

bar_ph_vs_world_2022_function <- function() {
  # Similar-income countries to Philippines
  similar_income_countries <- c("Indonesia", "Thailand", "Vietnam", "Malaysia", "India", "Brazil")
  
  # Pivot to long format
  PISA_long <- PISA_2022_scores %>%
    pivot_longer(cols = c(Reading, Mathematics, Science), 
                 names_to = "Subject", values_to = "Score")
  
  # Identify min & max scoring countries per subject
  top_bottom <- PISA_long %>%
    group_by(Subject) %>%
    summarise(
      MaxCountry = Country[which.max(Score)],
      MinCountry = Country[which.min(Score)],
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(MaxCountry, MinCountry), values_to = "Country") %>%
    distinct(Subject, Country)
  
  # Countries to include
  selected_countries <- unique(c(
    "Philippines", "OECD Average",
    similar_income_countries,
    top_bottom$Country
  ))
  
  # Filter dataset to selected countries
  filtered_PISA <- PISA_long %>%
    filter(Country %in% selected_countries) %>%
    mutate(Highlight = case_when(
      Country == "Philippines" ~ "Philippines",
      Country == "OECD Average" ~ "OECD Average",
      Country %in% top_bottom$Country ~ "Top/Bottom Scorer",
      TRUE ~ "Similar-Income"
    ))
  
  # Plot
  return(
    ggplot(filtered_PISA, aes(x = reorder(Country, Score), y = Score, fill = Highlight)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Subject, scales = "free_y") +
      scale_fill_manual(values = c(
        "Philippines" = "red",
        "OECD Average" = "orange",
        "Top/Bottom Scorer" = "steelblue",
        "Similar-Income" = "gray80"
      )) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      labs(
        title = "Philippines vs OECD, Top/Bottom and Similar-Income Countries (PISA 2022)",
        x = "Country",
        y = "Score",
        fill = "Group"))}
bar_ph_vs_world_2022_function()
