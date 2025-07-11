# ==============================================================================
# QUESTION 1: Which campaign had the highest reach?
# ==============================================================================

# Load required libraries (as per your request)
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualizations
library(scales)      # For formatting axis labels
library(stringr)     # For string manipulation

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

# Q8: Geography performance - FIXED VERSION
data %>%
  mutate(Geography = case_when(
    str_detect(Geography, "Group 1") ~ "Group 1",
    str_detect(Geography, "Group 2") ~ "Group 2",
    TRUE ~ Geography
  )) %>%
  group_by(Geography) %>%
  summarise(Total_ULC = sum(Unique_Link_Clicks), .groups = "drop") %>%
  ggplot(aes(x = reorder(Geography, Total_ULC), y = Total_ULC, fill = Geography)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(Total_ULC)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
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
ggsave("Q_Total_Spend_Campaign.png", width = 8, height = 5, dpi = 300)


# CTR vs Frequency by Group
data %>%
  filter(str_detect(Geography, "Group")) %>%
  mutate(Group = str_extract(Geography, "Group [12]")) %>%
  ggplot(aes(x = Frequency, y = CTR, size = Reach, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CTR vs Frequency by Group", x = "Frequency", y = "CTR") +
  theme_minimal()
ggsave("Q_CTR_vs_Frequency_by_Group.png", width = 7, height = 5, dpi = 300)


# Reach by Campaign and Age Group
data %>%
  group_by(Campaign_Name, Age) %>%
  summarise(Total_Reach = sum(Reach), .groups = "drop") %>%
  ggplot(aes(x = Age, y = Total_Reach, fill = Age)) +
  geom_col(position = "dodge") +
  facet_wrap(~Campaign_Name) +
  labs(title = "Reach by Campaign and Age Group", x = "Age", y = "Total Reach") +
  theme_minimal()
ggsave("Q_Reach_by_Campaign_and_Age.png", width = 10, height = 6, dpi = 300)


# Campaign A – CTR by Age
data %>%
  filter(Campaign_Name == "Campaign A") %>%
  group_by(Age) %>%
  summarise(Avg_CTR = mean(CTR), .groups = "drop") %>%
  ggplot(aes(x = Age, y = Avg_CTR, fill = Age)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "CTR by Age Group for Campaign A", x = "Age", y = "CTR") +
  theme_minimal()
ggsave("Q_CampaignA_CTR_by_Age.png", width = 7, height = 5, dpi = 300)

 
# Load required libraries
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(ggplot2)     # For visualizations
library(scales)      # For formatting axis labels

# Read the Excel data (replace with your actual file path)
data <- read_excel("Book1.xlsx", sheet = "Sheet1")

# Clean column names
colnames(data) <- c("Campaign_ID", "Campaign_Name", "Audience", "Age", "Geography", 
                    "Reach", "Impressions", "Frequency", "Clicks", "Unique_Clicks", 
                    "Unique_Link_Clicks", "CTR", "Unique_CTR", "Amount_Spent_INR", 
                    "CPC", "CPR", "Dropdown1", "Dropdown2")
data <- data %>% select(-Dropdown1, -Dropdown2)

# ------------------------------------------------------------------------------
# Summarize cost and click metrics
# ------------------------------------------------------------------------------
cost_click_summary <- data %>%
  group_by(Campaign_ID, Campaign_Name) %>%
  summarise(
    Total_Clicks = sum(Clicks),
    Total_ULC = sum(Unique_Link_Clicks),
    Avg_CPC = mean(CPC),
    Avg_CPR = mean(CPR),
    Total_Spend = sum(Amount_Spent_INR),
    .groups = "drop"
  ) %>%
  mutate(
    CPC_Rank = percent_rank(-Avg_CPC), # Lower CPC is better
    CPR_Rank = percent_rank(-Avg_CPR), # Lower CPR is better
    Clicks_Rank = percent_rank(Total_Clicks), # Higher clicks is better
    ULC_Rank = percent_rank(Total_ULC), # Higher ULC is better
    Performance_Score = (CPC_Rank + CPR_Rank + Clicks_Rank + ULC_Rank) / 4 # Average rank
  ) %>%
  arrange(Performance_Score) # Sort by performance score (lower is worse)

# Display the summary table
print(cost_click_summary, width = Inf)

# Save the summary to a CSV file
write.csv(cost_click_summary, "Cost_Clicks_Summary.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Plot 1: CPC and CPR by Campaign
# ------------------------------------------------------------------------------
ggplot(cost_click_summary, aes(x = reorder(Campaign_Name, Avg_CPR))) +
  geom_bar(aes(y = Avg_CPR, fill = Avg_CPR), stat = "identity", width = 0.5) +
  geom_point(aes(y = Avg_CPC * 10, color = Avg_CPC), size = 3) + # Scale CPC for visibility
  scale_y_continuous(
    name = "Avg CPR (INR)",
    sec.axis = sec_axis(~./10, name = "Avg CPC (INR)", labels = comma)
  ) +
  scale_fill_gradient(low = "#51cf66", high = "#ff6b6b") +
  scale_color_gradient(low = "#51cf66", high = "#ff6b6b") +
  coord_flip() +
  labs(title = "Cost Efficiency: CPC and CPR by Campaign", x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.y = element_text(size = 8)
  )
ggsave("Cost_Efficiency_CPC_CPR.png", width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# Plot 2: Clicks and ULC vs. Spend
# ------------------------------------------------------------------------------
ggplot(cost_click_summary, aes(x = Total_Spend)) +
  geom_point(aes(y = Total_Clicks, color = "Clicks", size = Total_Clicks), alpha = 0.7) +
  geom_point(aes(y = Total_ULC * 2, color = "Unique Link Clicks"), size = 3, shape = 17, alpha = 0.7) + # Scale ULC for visibility
  scale_y_continuous(
    name = "Total Clicks",
    sec.axis = sec_axis(~./2, name = "Total Unique Link Clicks", labels = comma)
  ) +
  scale_color_manual(values = c("Clicks" = "#51cf66", "Unique Link Clicks" = "#ff6b6b")) +
  labs(title = "Clicks and ULC vs. Ad Spend by Campaign", x = "Total Spend (INR)", color = "Metric") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Clicks_ULC_vs_Spend.png", width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# Statistical Test for CPR
# ------------------------------------------------------------------------------
anova_result_cpr <- aov(CPR ~ Campaign_ID, data = data)
summary(anova_result_cpr)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Superhero U Campaign Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Campaign Performance", tabName = "campaign", icon = icon("chart-line")),
      menuItem("Audience Analysis", tabName = "audience", icon = icon("users")),
      menuItem("Geography Analysis", tabName = "geo", icon = icon("globe")),
      menuItem("Cost Efficiency", tabName = "cost", icon = icon("money-bill-wave"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_spend"),
                valueBoxOutput("total_clicks"),
                valueBoxOutput("avg_ctr")
              ),
              fluidRow(
                box(width = 6, plotlyOutput("reach_plot")),
                box(width = 6, plotlyOutput("impressions_plot"))
              )
      ),
      
      # Campaign Performance Tab
      tabItem(tabName = "campaign",
              fluidRow(
                box(width = 12, selectInput("campaign_select", "Select Campaign:", 
                                            choices = unique(data$Campaign_ID)))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("ctr_comparison")),
                box(width = 6, plotlyOutput("frequency_plot"))
              )
      ),
      
      # Audience Analysis Tab
      tabItem(tabName = "audience",
              fluidRow(
                box(width = 6, plotlyOutput("age_clicks")),
                box(width = 6, plotlyOutput("audience_clicks"))
              )
      ),
      
      # Geography Analysis Tab
      tabItem(tabName = "geo",
              fluidRow(
                box(width = 6, plotlyOutput("geo_performance")),
                box(width = 6, plotlyOutput("region_comparison"))
              )
      ),
      
      # Cost Efficiency Tab
      tabItem(tabName = "cost",
              fluidRow(
                box(width = 6, plotlyOutput("cpc_plot")),
                box(width = 6, plotlyOutput("cpr_plot"))
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # KPIs
  output$total_spend <- renderValueBox({
    total <- sum(data$Amount_Spent_INR)
    valueBox(
      paste0("₹", format(round(total), big.mark = ",")),
      "Total Spend",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$total_clicks <- renderValueBox({
    total <- sum(data$Clicks)
    valueBox(
      format(total, big.mark = ","),
      "Total Clicks",
      icon = icon("mouse-pointer"),
      color = "blue"
    )
  })
  
  output$avg_ctr <- renderValueBox({
    avg_ctr <- mean(data$CTR)
    valueBox(
      paste0(round(avg_ctr * 100, 1), "%"),
      "Average CTR",
      icon = icon("percent"),
      color = "purple"
    )
  })
  
  # Plots
  output$reach_plot <- renderPlotly({
    reach_summary <- data %>%
      group_by(Campaign_ID) %>%
      summarise(Total_Reach = sum(Reach), .groups = "drop")
    
    ggplotly(
      ggplot(reach_summary, aes(x = reorder(Campaign_ID, Total_Reach), y = Total_Reach,
                                text = paste("Campaign:", Campaign_ID, "<br>Reach:", Total_Reach))) +
        geom_bar(stat = "identity", fill = "#3c8dbc") +
        coord_flip() +
        labs(title = "Campaign Reach", x = NULL, y = "Total Reach") +
        theme_minimal()
    ) %>% layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$impressions_plot <- renderPlotly({
    imp_summary <- data %>%
      group_by(Audience) %>%
      summarise(Total_Impressions = sum(Impressions), .groups = "drop")
    
    ggplotly(
      ggplot(imp_summary, aes(x = Audience, y = Total_Impressions, fill = Audience,
                              text = paste("Audience:", Audience, "<br>Impressions:", Total_Impressions))) +
        geom_bar(stat = "identity") +
        labs(title = "Impressions by Audience", x = NULL, y = "Total Impressions") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$ctr_comparison <- renderPlotly({
    ggplotly(
      ggplot(data, aes(x = CTR, y = Unique_CTR, color = Audience,
                       text = paste("Campaign:", Campaign_ID, "<br>CTR:", round(CTR * 100, 1), "%",
                                    "<br>Unique CTR:", round(Unique_CTR * 100, 1), "%"))) +
        geom_point(size = 2) +
        geom_abline(linetype = "dashed", color = "gray") +
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent) +
        labs(title = "Unique vs Regular CTR", x = "CTR", y = "Unique CTR") +
        theme_minimal()
    )
  })
  
  output$frequency_plot <- renderPlotly({
    ggplotly(
      ggplot(data, aes(x = Frequency, y = Clicks, color = Audience, size = Reach,
                       text = paste("Campaign:", Campaign_ID, "<br>Frequency:", round(Frequency, 1),
                                    "<br>Clicks:", Clicks))) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(2, 8)) +
        labs(title = "Frequency vs Clicks", x = "Frequency", y = "Clicks") +
        theme_minimal()
    )
  })
  
  output$age_clicks <- renderPlotly({
    student_data <- data %>% filter(Audience == "Students")
    
    ggplotly(
      ggplot(student_data, aes(x = Age, y = Clicks, fill = Age,
                               text = paste("Age:", Age, "<br>Avg Clicks:", round(Clicks, 0)))) +
        geom_bar(stat = "summary", fun = "mean") +
        labs(title = "Student Engagement by Age", x = "Age Group", y = "Average Clicks") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$audience_clicks <- renderPlotly({
    ggplotly(
      data %>%
        group_by(Audience) %>%
        summarise(Total_Clicks = sum(Clicks), .groups = "drop") %>%
        ggplot(aes(x = Audience, y = Total_Clicks, fill = Audience,
                   text = paste("Audience:", Audience, "<br>Total Clicks:", Total_Clicks))) +
        geom_bar(stat = "identity") +
        labs(title = "Total Clicks by Audience", x = NULL, y = "Total Clicks") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$geo_performance <- renderPlotly({
    geo_data <- data %>%
      mutate(Geography = case_when(
        str_detect(Geography, "Group 1") ~ "Group 1",
        str_detect(Geography, "Group 2") ~ "Group 2",
        TRUE ~ Geography
      ))
    
    ggplotly(
      geo_data %>%
        group_by(Geography) %>%
        summarise(Total_ULC = sum(Unique_Link_Clicks), .groups = "drop") %>%
        ggplot(aes(x = reorder(Geography, Total_ULC), y = Total_ULC, fill = Geography,
                   text = paste("Geography:", Geography, "<br>ULC:", Total_ULC))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Unique Link Clicks by Geography", x = NULL, y = "Unique Link Clicks") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$region_comparison <- renderPlotly({
    region_data <- data %>%
      mutate(Region = ifelse(Geography %in% c("USA", "UK", "Canada", "Australia"),
                             "Western", "Non-Western"))
    
    ggplotly(
      region_data %>%
        group_by(Region) %>%
        summarise(Avg_CTR = mean(CTR), .groups = "drop") %>%
        ggplot(aes(x = Region, y = Avg_CTR, fill = Region,
                   text = paste("Region:", Region, "<br>CTR:", round(Avg_CTR * 100, 1), "%"))) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = percent) +
        labs(title = "CTR by Region", x = NULL, y = "Average CTR") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$cpc_plot <- renderPlotly({
    ggplotly(
      data %>%
        group_by(Campaign_ID) %>%
        summarise(Avg_CPC = mean(CPC), .groups = "drop") %>%
        ggplot(aes(x = reorder(Campaign_ID, Avg_CPC), y = Avg_CPC, fill = Avg_CPC,
                   text = paste("Campaign:", Campaign_ID, "<br>CPC: ₹", round(Avg_CPC, 2)))) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "#51cf66", high = "#ff6b6b") +
        coord_flip() +
        labs(title = "Cost Per Click by Campaign", x = NULL, y = "Average CPC (INR)") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$cpr_plot <- renderPlotly({
    ggplotly(
      data %>%
        group_by(Campaign_ID) %>%
        summarise(Avg_CPR = mean(CPR), .groups = "drop") %>%
        ggplot(aes(x = reorder(Campaign_ID, Avg_CPR), y = Avg_CPR, fill = Avg_CPR,
                   text = paste("Campaign:", Campaign_ID, "<br>CPR: ₹", round(Avg_CPR, 2)))) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "#51cf66", high = "#ff6b6b") +
        coord_flip() +
        labs(title = "Cost Per Result by Campaign", x = NULL, y = "Average CPR (INR)") +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)



library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(plotly)

# Read and clean data
data <- read_excel("Book1.xlsx", sheet = "Sheet1")
colnames(data) <- c("Campaign_ID", "Campaign_Name", "Audience", "Age", "Geography", 
                    "Reach", "Impressions", "Frequency", "Clicks", "Unique_Clicks", 
                    "Unique_Link_Clicks", "CTR", "Unique_CTR", "Amount_Spent_INR", 
                    "CPC", "CPR", "Dropdown1", "Dropdown2")
data <- data %>% select(-Dropdown1, -Dropdown2)

# Pre-compute summaries for efficiency
reach_summary <- data %>%
  group_by(Campaign_ID, Campaign_Name) %>%
  summarise(Total_Reach = sum(Reach), .groups = "drop") %>%
  arrange(desc(Total_Reach))

cost_click_summary <- data %>%
  group_by(Campaign_ID, Campaign_Name) %>%
  summarise(
    Total_Clicks = sum(Clicks),
    Total_ULC = sum(Unique_Link_Clicks),
    Avg_CPC = mean(CPC),
    Avg_CPR = mean(CPR),
    Total_Spend = sum(Amount_Spent_INR),
    .groups = "drop"
  )

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Campaign Analytics Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Campaign Performance", tabName = "campaign", icon = icon("chart-bar")),
      menuItem("Audience Analysis", tabName = "audience", icon = icon("users")),
      menuItem("Geographic Insights", tabName = "geography", icon = icon("globe")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    ),
    selectInput("campaignFilter", "Select Campaign:", 
                choices = c("All", unique(data$Campaign_Name)), selected = "All"),
    selectInput("audienceFilter", "Select Audience:", 
                choices = c("All", unique(data$Audience)), selected = "All")
  ),
  dashboardBody(
    tabItems(
      # Campaign Performance Tab
      tabItem(
        tabName = "campaign",
        fluidRow(
          valueBoxOutput("reachBox", width = 3),
          valueBoxOutput("ctrBox", width = 3),
          valueBoxOutput("cpcBox", width = 3),
          valueBoxOutput("spendBox", width = 3)
        ),
        fluidRow(
          box(
            title = "Campaigns by Total Reach", status = "primary", solidHeader = TRUE,
            plotlyOutput("reachPlot"), width = 6,
            downloadButton("downloadReach", "Download Plot")
          ),
          box(
            title = "CTR by Campaign", status = "primary", solidHeader = TRUE,
            plotlyOutput("ctrPlot"), width = 6,
            downloadButton("downloadCTR", "Download Plot")
          )
        ),
        fluidRow(
          box(
            title = "Spend vs. Unique Link Clicks", status = "primary", solidHeader = TRUE,
            plotlyOutput("spendPlot"), width = 12
          )
        )
      ),
      # Audience Analysis Tab
      tabItem(
        tabName = "audience",
        fluidRow(
          valueBoxOutput("clicksBox", width = 3),
          valueBoxOutput("impressionsBox", width = 3),
          valueBoxOutput("cprBox", width = 3)
        ),
        fluidRow(
          box(
            title = "Impressions by Audience", status = "primary", solidHeader = TRUE,
            plotlyOutput("impressionsPlot"), width = 6,
            downloadButton("downloadImpressions", "Download Plot")
          ),
          box(
            title = "Clicks by Audience", status = "primary", solidHeader = TRUE,
            plotlyOutput("clicksPlot"), width = 6,
            downloadButton("downloadClicks", "Download Plot")
          )
        )
      ),
      # Geographic Insights Tab
      tabItem(
        tabName = "geography",
        fluidRow(
          box(
            title = "Unique Link Clicks by Geography", status = "primary", solidHeader = TRUE,
            plotlyOutput("geoPlot"), width = 6,
            downloadButton("downloadGeo", "Download Plot")
          ),
          box(
            title = "CTR by Geography Group", status = "primary", solidHeader = TRUE,
            plotlyOutput("groupPlot"), width = 6,
            downloadButton("downloadGroup", "Download Plot")
          )
        )
      ),
      # Data Table Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Campaign Summary Data", status = "primary", solidHeader = TRUE,
            DTOutput("dataTable"), width = 12,
            downloadButton("downloadData", "Download CSV")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data
    if (input$campaignFilter != "All") {
      df <- df %>% filter(Campaign_Name == input$campaignFilter)
    }
    if (input$audienceFilter != "All") {
      df <- df %>% filter(Audience == input$audienceFilter)
    }
    df
  })
  
  # KPIs
  output$reachBox <- renderValueBox({
    total_reach <- sum(filtered_data()$Reach)
    valueBox(format(total_reach, big.mark = ","), "Total Reach", icon = icon("users"), color = "blue")
  })
  
  output$clicksBox <- renderValueBox({
    total_clicks <- sum(filtered_data()$Clicks)
    valueBox(format(total_clicks, big.mark = ","), "Total Clicks", icon = icon("mouse-pointer"), color = "green")
  })
  
  output$ctrBox <- renderValueBox({
    avg_ctr <- mean(filtered_data()$CTR) * 100
    valueBox(sprintf("%.2f%%", avg_ctr), "Average CTR", icon = icon("percent"), color = "yellow")
  })
  
  output$spendBox <- renderValueBox({
    total_spend <- sum(filtered_data()$Amount_Spent_INR)
    valueBox(paste("₹", format(total_spend, big.mark = ",")), "Total Spend", icon = icon("inr"), color = "red")
  })
  
  output$impressionsBox <- renderValueBox({
    total_impressions <- sum(filtered_data()$Impressions)
    valueBox(format(total_impressions, big.mark = ","), "Total Impressions", icon = icon("eye"), color = "purple")
  })
  
  output$cpcBox <- renderValueBox({
    avg_cpc <- mean(filtered_data()$CPC)
    valueBox(sprintf("₹%.2f", avg_cpc), "Average CPC", icon = icon("money-bill"), color = "orange")
  })
  
  output$cprBox <- renderValueBox({
    avg_cpr <- mean(filtered_data()$CPR)
    valueBox(sprintf("₹%.2f", avg_cpr), "Average CPR", icon = icon("money-check"), color = "teal")
  })
  
  # Plot 1: Campaign Reach (Q1)
  output$reachPlot <- renderPlotly({
    reach_summary <- filtered_data() %>%
      group_by(Campaign_ID, Campaign_Name) %>%
      summarise(Total_Reach = sum(Reach), .groups = "drop") %>%
      arrange(desc(Total_Reach))
    
    p <- ggplot(reach_summary, aes(x = reorder(Campaign_Name, Total_Reach), y = Total_Reach, fill = Campaign_ID)) +
      geom_bar(stat = "identity", width = 0.8) +
      geom_text(aes(label = comma(Total_Reach)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Campaigns by Total Reach", x = NULL, y = "Total Reach", fill = "Campaign ID") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")
    ggplotly(p)
  })
  
  output$downloadReach <- downloadHandler(
    filename = "reach_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 8, height = 5, dpi = 300)
    }
  )
  
  # Plot 2: CTR by Campaign (Q4)
  output$ctrPlot <- renderPlotly({
    ctr_summary <- filtered_data() %>%
      group_by(Campaign_ID) %>%
      summarise(Avg_CTR = mean(CTR), .groups = "drop") %>%
      arrange(desc(Avg_CTR))
    
    p <- ggplot(ctr_summary, aes(x = reorder(Campaign_ID, Avg_CTR), y = Avg_CTR, fill = Avg_CTR)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = percent(Avg_CTR, accuracy = 0.1)), hjust = -0.1, size = 3) +
      coord_flip() +
      scale_fill_gradient(low = "#ff6b6b", high = "#51cf66", guide = "none") +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Average CTR by Campaign", x = NULL, y = "Click-Through Rate") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p)
  })
  
  output$downloadCTR <- downloadHandler(
    filename = "ctr_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 7, height = 5, dpi = 300)
    }
  )
  
  # Plot 3: Spend vs. Unique Link Clicks (Q13)
  output$spendPlot <- renderPlotly({
    cost_click_summary <- filtered_data() %>%
      group_by(Campaign_ID, Campaign_Name) %>%
      summarise(Total_Spend = sum(Amount_Spent_INR), Total_ULC = sum(Unique_Link_Clicks), .groups = "drop")
    
    p <- ggplot(cost_click_summary, aes(x = Total_Spend, y = Total_ULC)) +
      geom_point(aes(color = Campaign_Name), size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      scale_x_continuous(labels = comma) +
      labs(title = "Ad Spend vs. Unique Link Clicks", x = "Total Spend (INR)", y = "Unique Link Clicks", color = "Campaign") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p)
  })
  
  output$downloadSpend <- downloadHandler(
    filename = "spend_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 7, height = 5, dpi = 300)
    }
  )
  
  # Plot 4: Impressions by Audience (Q2)
  output$impressionsPlot <- renderPlotly({
    impressions_by_audience <- filtered_data() %>%
      group_by(Audience) %>%
      summarise(Total_Impressions = sum(Impressions), .groups = "drop")
    
    p <- ggplot(impressions_by_audience, aes(x = Audience, y = Total_Impressions, fill = Audience)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = comma(Total_Impressions)), vjust = -0.5, size = 3.5) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Total Impressions by Audience Type", x = NULL, y = "Total Impressions") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p)
  })
  
  output$downloadImpressions <- downloadHandler(
    filename = "impressions_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 6, height = 5, dpi = 300)
    }
  )
  
  # Plot 5: Clicks by Audience (Q7)
  output$clicksPlot <- renderPlotly({
    clicks_by_audience <- filtered_data() %>%
      group_by(Audience) %>%
      summarise(Total_Clicks = sum(Clicks), .groups = "drop")
    
    p <- ggplot(clicks_by_audience, aes(x = Audience, y = Total_Clicks, fill = Audience)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = comma(Total_Clicks)), vjust = -0.5, size = 3.5) +
      labs(title = "Total Clicks by Audience Type", x = NULL, y = "Total Clicks") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p)
  })
  
  output$downloadClicks <- downloadHandler(
    filename = "clicks_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 6, height = 5, dpi = 300)
    }
  )
  
  # Plot 6: Unique Link Clicks by Geography (Q8)
  output$geoPlot <- renderPlotly({
    geo_summary <- filtered_data() %>%
      mutate(Geography = case_when(
        str_detect(Geography, "Group 1") ~ "Group 1",
        str_detect(Geography, "Group 2") ~ "Group 2",
        TRUE ~ Geography
      )) %>%
      group_by(Geography) %>%
      summarise(Total_ULC = sum(Unique_Link_Clicks), .groups = "drop")
    
    p <- ggplot(geo_summary, aes(x = reorder(Geography, Total_ULC), y = Total_ULC, fill = Geography)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = comma(Total_ULC)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Unique Link Clicks by Geography", x = NULL, y = "Unique Link Clicks") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p)
  })
  
  output$downloadGeo <- downloadHandler(
    filename = "geo_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 7, height = 5, dpi = 300)
    }
  )
  
  # Plot 7: CTR by Geography Group (Q9)
  output$groupPlot <- renderPlotly({
    group_summary <- filtered_data() %>%
      filter(str_detect(Geography, "Group")) %>%
      mutate(Group = str_extract(Geography, "Group [12]")) %>%
      group_by(Group) %>%
      summarise(Avg_CTR = mean(CTR), .groups = "drop")
    
    p <- ggplot(group_summary, aes(x = Group, y = Avg_CTR, fill = Group)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Average CTR by Geography Group", x = NULL, y = "Click-Through Rate") +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
    ggplotly(p)
  })
  
  output$downloadGroup <- downloadHandler(
    filename = "group_plot.png",
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 6, height = 5, dpi = 300)
    }
  )
  
  # Data Table
  output$dataTable <- renderDT({
    datatable(cost_click_summary, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = "campaign_summary.csv",
    content = function(file) {
      write.csv(cost_click_summary, file, row.names = FALSE)
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
