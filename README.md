# ==============================================================================
# QUESTION 1: Which campaign had the highest reach?
# ==============================================================================

# Load required libraries (as per your request)
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualizations
library(scales)      # For formatting axis labels

# Read the Excel data (replace with your actual file path)
data <- read_excel("Book1.xlsx", sheet = "Sheet1")

# Clean column names (as per your code)
colnames(data) <- c("Campaign_ID", "Campaign_Name", "Audience", "Age", "Geography", 
                    "Reach", "Impressions", "Frequency", "Clicks", "Unique_Clicks", 
                    "Unique_Link_Clicks", "CTR", "Unique_CTR", "Amount_Spent_INR", 
                    "CPC", "CPR", "Dropdown1", "Dropdown2")
data <- data %>% select(-Dropdown1, -Dropdown2)

# Separate individual countries and groups (as per your code)
individual_data <- data %>% 
  filter(!Geography %in% c("Group 1 (Australia, Canada, United Kingdom, Ghana, Nigeria, Pakistan, United States)", 
                           "Group 2 (Australia, Canada, United Kingdom, Ghana, Niger, Nigeria, Nepal, Pakistan, Thailand, Taiwan)"))
group_data <- data %>% 
  filter(Geography %in% c("Group 1 (Australia, Canada, United Kingdom, Ghana, Nigeria, Pakistan, United States)", 
                          "Group 2 (Australia, Canada, United Kingdom, Ghana, Niger, Nigeria, Nepal, Pakistan, Thailand, Taiwan)"))

# ------------------------------------------------------------------------------
# ANSWER TO Q1: Campaign with highest reach
# ------------------------------------------------------------------------------

# Calculate total reach per campaign
reach_summary <- data %>%
  group_by(Campaign_ID, Campaign_Name) %>%
  summarise(Total_Reach = sum(Reach), .groups = "drop") %>%
  arrange(desc(Total_Reach))

# Visualization
ggplot(reach_summary, aes(x = reorder(Campaign_Name, Total_Reach), y = Total_Reach, fill = Campaign_ID)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = comma(Total_Reach)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Q1: Campaigns by Total Reach",
    x = NULL,
    y = "Total Reach",
    fill = "Campaign ID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# Save the plot
ggsave("Q1_Campaign_Reach.png", width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------------------------
# QUESTION 2: Impressions by audience type
# ------------------------------------------------------------------------------
impressions_by_audience <- data %>%
  group_by(Audience) %>%
  summarise(Total_Impressions = sum(Impressions), .groups = "drop")

ggplot(impressions_by_audience, 
       aes(x = Audience, y = Total_Impressions, fill = Audience)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = comma(Total_Impressions)), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Q2: Total Impressions by Audience Type",
       x = NULL,
       y = "Total Impressions") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Q2_Impressions_by_Audience.png", width = 6, height = 5, dpi = 300)


# ------------------------------------------------------------------------------
# QUESTION 3: Frequency vs. engagement
# ------------------------------------------------------------------------------
ggplot(data, 
       aes(x = Frequency, y = Clicks, color = Audience, size = Reach)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  labs(title = "Q3: Frequency vs. Clicks",
       x = "Frequency (Impressions/Reach)",
       y = "Total Clicks",
       color = "Audience") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Q3_Frequency_vs_Clicks.png", width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# QUESTION 4: Campaign CTR comparison
# ------------------------------------------------------------------------------
ctr_summary <- data %>%
  group_by(Campaign_ID) %>%
  summarise(Avg_CTR = mean(CTR), .groups = "drop") %>%
  arrange(desc(Avg_CTR))

ggplot(ctr_summary, 
       aes(x = reorder(Campaign_ID, Avg_CTR), y = Avg_CTR, fill = Avg_CTR)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = percent(Avg_CTR, accuracy = 0.1)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_gradient(low = "#ff6b6b", high = "#51cf66", guide = "none") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Q4: Average CTR by Campaign",
       x = NULL,
       y = "Click-Through Rate") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Q4_CTR_by_Campaign.png", width = 7, height = 5, dpi = 300)

# Q5: Unique CTR vs. Regular CTR
ggplot(data, aes(x = CTR, y = Unique_CTR, color = Audience)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(linetype = "dashed", color = "gray50") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q5: Unique CTR vs Regular CTR Comparison",
       x = "Click-Through Rate (CTR)",
       y = "Unique CTR",
       color = "Audience") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q5_CTR_Comparison.png", width = 7, height = 5, dpi = 300)


# Q6: Age engagement (Students)
data %>%
  filter(Audience == "Students") %>%
  group_by(Age) %>%
  summarise(Avg_Clicks = mean(Clicks), .groups = "drop") %>%
  ggplot(aes(x = Age, y = Avg_Clicks, fill = Age)) +
  geom_col(width = 0.7) +
  labs(title = "Q6: Average Clicks by Age Group (Students)",
       x = "Age Group",
       y = "Average Clicks") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q6_Age_Clicks_Students.png", width = 6, height = 5, dpi = 300)

# Q7: Clicks by audience
data %>%
  group_by(Audience) %>%
  summarise(Total_Clicks = sum(Clicks), .groups = "drop") %>%
  ggplot(aes(x = Audience, y = Total_Clicks, fill = Audience)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = comma(Total_Clicks)), vjust = -0.5, size = 3.5) +
  labs(title = "Q7: Total Clicks by Audience Type",
       x = NULL,
       y = "Total Clicks") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q7_Clicks_by_Audience.png", width = 6, height = 5, dpi = 300)

# Q8: Geography performance
data %>%
  mutate(Geography = ifelse(str_detect(Geography, "Group"), 
                            str_extract(Geography, "Group [12]"), 
                            Geography)) %>%
  group_by(Geography) %>%
  summarise(Total_ULC = sum(Unique_Link_Clicks), .groups = "drop") %>%
  ggplot(aes(x = reorder(Geography, Total_ULC), y = Total_ULC, fill = Geography)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(title = "Q8: Unique Link Clicks by Geography",
       x = NULL,
       y = "Unique Link Clicks") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q8_Geography_Performance.png", width = 7, height = 5, dpi = 300)

# Q9: Group comparison
data %>%
  filter(str_detect(Geography, "Group")) %>%
  mutate(Group = str_extract(Geography, "Group [12]")) %>%
  group_by(Group) %>%
  summarise(Avg_CTR = mean(CTR), .groups = "drop") %>%
  ggplot(aes(x = Group, y = Avg_CTR, fill = Group)) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q9: Average CTR by Geography Group",
       x = NULL,
       y = "Click-Through Rate") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q9_Group_Comparison.png", width = 6, height = 5, dpi = 300)

# Q10: Age group engagement
data %>%
  mutate(Age_Group = ifelse(Age %in% c("13-17", "18-24"), "Young (13-24)", "Older (25-64)")) %>%
  group_by(Age_Group) %>%
  summarise(Avg_Clicks = mean(Clicks), .groups = "drop") %>%
  ggplot(aes(x = Age_Group, y = Avg_Clicks, fill = Age_Group)) +
  geom_col(width = 0.5) +
  labs(title = "Q10: Average Clicks by Age Group",
       x = NULL,
       y = "Average Clicks") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q10_Age_Group_Engagement.png", width = 6, height = 5, dpi = 300)

# Q11: CPC by campaign
data %>%
  group_by(Campaign_ID) %>%
  summarise(Avg_CPC = mean(CPC), .groups = "drop") %>%
  arrange(Avg_CPC) %>%
  ggplot(aes(x = reorder(Campaign_ID, -Avg_CPC), y = Avg_CPC, fill = Avg_CPC)) +
  geom_col(width = 0.7) +
  scale_fill_gradient(low = "#51cf66", high = "#ff6b6b") +
  coord_flip() +
  labs(title = "Q11: Average Cost Per Click by Campaign",
       x = NULL,
       y = "Cost Per Click (INR)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q11_CPC_by_Campaign.png", width = 7, height = 5, dpi = 300)

# Q12: CPR by audience
data %>%
  group_by(Audience) %>%
  summarise(Avg_CPR = mean(CPR), .groups = "drop") %>%
  ggplot(aes(x = Audience, y = Avg_CPR, fill = Audience)) +
  geom_col(width = 0.6) +
  labs(title = "Q12: Average Cost Per Result by Audience",
       x = NULL,
       y = "Cost Per Result (INR)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q12_CPR_by_Audience.png", width = 6, height = 5, dpi = 300)


# Q13: Spend vs. performance
ggplot(data, aes(x = Amount_Spent_INR, y = Unique_Link_Clicks)) +
  geom_point(aes(color = Audience), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(labels = comma) +
  labs(title = "Q13: Ad Spend vs. Unique Link Clicks",
       x = "Amount Spent (INR)",
       y = "Unique Link Clicks",
       color = "Audience") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q13_Spend_vs_Performance.png", width = 7, height = 5, dpi = 300)


# Q14: ROI by campaign
data %>%
  group_by(Campaign_ID) %>%
  summarise(CPR = mean(CPR), .groups = "drop") %>%
  arrange(CPR) %>%
  ggplot(aes(x = reorder(Campaign_ID, -CPR), y = CPR, fill = CPR)) +
  geom_col(width = 0.7) +
  scale_fill_gradient(low = "#51cf66", high = "#ff6b6b") +
  coord_flip() +
  labs(title = "Q14: Cost Per Result by Campaign (Lower is Better)",
       x = NULL,
       y = "Cost Per Result (INR)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q14_ROI_by_Campaign.png", width = 7, height = 5, dpi = 300)

# Q15: Frequency vs. CTR
ggplot(data, aes(x = Frequency, y = CTR, color = Audience)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q15: Frequency Impact on CTR",
       x = "Frequency",
       y = "Click-Through Rate",
       color = "Audience") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q15_Frequency_vs_CTR.png", width = 7, height = 5, dpi = 300)

# Q16: Geography comparison
data %>%
  mutate(Region = ifelse(Geography %in% c("USA", "UK", "Canada", "Australia"), 
                         "Western", "Non-Western")) %>%
  group_by(Region) %>%
  summarise(Avg_ULC = mean(Unique_Link_Clicks), .groups = "drop") %>%
  ggplot(aes(x = Region, y = Avg_ULC, fill = Region)) +
  geom_col(width = 0.6) +
  labs(title = "Q16: Unique Link Clicks by Region",
       x = NULL,
       y = "Average Unique Link Clicks") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q16_Region_Performance.png", width = 6, height = 5, dpi = 300)

# Q17: CPR by age
data %>%
  group_by(Age) %>%
  summarise(CPR = mean(CPR), .groups = "drop") %>%
  ggplot(aes(x = reorder(Age, CPR), y = CPR, fill = CPR)) +
  geom_col(width = 0.7) +
  scale_fill_gradient(low = "#51cf66", high = "#ff6b6b") +
  coord_flip() +
  labs(title = "Q17: Cost Per Result by Age Group",
       x = NULL,
       y = "Cost Per Result (INR)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q17_CPR_by_Age.png", width = 7, height = 5, dpi = 300)

# Q19: Best performing combo
data %>%
  group_by(Audience, Age, Geography) %>%
  summarise(ULC = mean(Unique_Link_Clicks), .groups = "drop") %>%
  arrange(desc(ULC)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(paste(Audience, Age, Geography), y = ULC, fill = Audience)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(title = "Q19: Top Audience-Age-Geography Combos",
       x = NULL,
       y = "Average Unique Link Clicks") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Q19_Top_Combos.png", width = 9, height = 5, dpi = 300)

# Q20: Underperforming campaigns
data %>%
  mutate(CTR_Rank = percent_rank(CTR)) %>%
  filter(Impressions > median(Impressions), CTR_Rank < 0.3) %>%
  ggplot(aes(x = reorder(Campaign_ID, CTR), y = CTR, size = Impressions, color = Audience)) +
  geom_point(alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q20: High-Impression, Low-CTR Campaigns",
       x = "Campaign",
       y = "Click-Through Rate",
       size = "Impressions",
       color = "Audience") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Q20_Underperforming_Campaigns.png", width = 8, height = 5, dpi = 300)

# Total Spend per Campaign
data %>%
  group_by(Campaign_ID, Campaign_Name) %>%
  summarise(Total_Spend = sum(Amount_Spent_INR), .groups = "drop") %>%
  ggplot(aes(x = reorder(Campaign_Name, Total_Spend), y = Total_Spend, fill = Campaign_ID)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Campaign Cost (INR)", x = NULL, y = "Amount Spent") +
  theme_minimal()


data %>%
  filter(str_detect(Geography, "Group")) %>%
  mutate(Group = str_extract(Geography, "Group [12]")) %>%
  ggplot(aes(x = Frequency, y = CTR, size = Reach, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CTR vs Frequency by Group", x = "Frequency", y = "CTR") +
  theme_minimal()


data %>%
  group_by(Campaign_Name, Age) %>%
  summarise(Total_Reach = sum(Reach), .groups = "drop") %>%
  ggplot(aes(x = Age, y = Total_Reach, fill = Age)) +
  geom_col(position = "dodge") +
  facet_wrap(~Campaign_Name) +
  labs(title = "Reach by Campaign and Age Group", x = "Age", y = "Total Reach") +
  theme_minimal()


data %>%
  filter(Campaign_Name == "Campaign A") %>%
  group_by(Age) %>%
  summarise(Avg_CTR = mean(CTR), .groups = "drop") %>%
  ggplot(aes(x = Age, y = Avg_CTR, fill = Age)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "CTR by Age Group for Campaign A", x = "Age", y = "CTR") +
  theme_minimal()

