# Load required libraries for data manipulation, visualization, and modeling
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(plotly)      # For interactive visualizations
library(viridis)     # For beautiful, colorblind-friendly color palettes
library(htmlwidgets) # For saving interactive plots as HTML
library(randomForest)# For feature importance analysis

# Read the Excel data (assuming the file is named 'Book1.xlsx')
# Replace 'path/to/Book1.xlsx' with the actual file path if needed
data <- read_excel("Book1.xlsx", sheet = "Sheet1")

# Clean column names to remove spaces and special characters for easier handling
colnames(data) <- c("Campaign_ID", "Campaign_Name", "Audience", "Age", "Geography", 
                    "Reach", "Impressions", "Frequency", "Clicks", "Unique_Clicks", 
                    "Unique_Link_Clicks", "CTR", "Unique_CTR", "Amount_Spent_INR", 
                    "CPC", "CPR", "Dropdown1", "Dropdown2")

# Remove unnecessary dropdown columns
data <- data %>% select(-Dropdown1, -Dropdown2)

# Separate data into individual countries and groups
# Individual countries dataset: Exclude Group 1 and Group 2
individual_data <- data %>% 
  filter(!Geography %in% c("Group 1 (Australia, Canada, United Kingdom, Ghana, Nigeria, Pakistan, United States)", 
                           "Group 2 (Australia, Canada, United Kingdom, Ghana, Niger, Nigeria, Nepal, Pakistan, Thailand, Taiwan)"))

# Group dataset: Include only Group 1 and Group 2
group_data <- data %>% 
  filter(Geography %in% c("Group 1 (Australia, Canada, United Kingdom, Ghana, Nigeria, Pakistan, United States)", 
                          "Group 2 (Australia, Canada, United Kingdom, Ghana, Niger, Nigeria, Nepal, Pakistan, Thailand, Taiwan)"))

# Save group data to a separate CSV for reference
write.csv(group_data, "group_data.csv", row.names = FALSE)

# Define a color palette using viridis for vibrant, colorblind-friendly colors
color_palette <- viridis::viridis(10, option = "D")

# --- Feature Importance Analysis ---
# Purpose: Identify key predictors of CTR using a random forest model
# Prepare data for random forest: Convert categorical variables to factors
rf_data <- individual_data %>% 
  select(Audience, Age, Geography, Reach, Impressions, Frequency, Amount_Spent_INR, CTR) %>%
  mutate(Audience = as.factor(Audience),
         Age = as.factor(Age),
         Geography = as.factor(Geography))

# Train random forest model to predict CTR
set.seed(123) # For reproducibility
rf_model <- randomForest(CTR ~ ., data = rf_data, ntree = 100, importance = TRUE)

# Extract feature importance
importance_df <- as.data.frame(importance(rf_model, type = 1)) %>%
  tibble::rownames_to_column(var = "Feature") %>%
  rename(Importance = `%IncMSE`)

# Visualization 1: Interactive Bar Plot of Feature Importance
# Purpose: Show which features (Audience, Age, Geography, etc.) most influence CTR
p1 <- plot_ly(data = importance_df, 
              x = ~reorder(Feature, Importance), 
              y = ~Importance, 
              type = "bar",
              marker = list(color = color_palette[1]),
              hoverinfo = "text",
              text = ~paste("Feature: ", Feature, "<br>",
                            "Importance: ", round(Importance, 2))) %>%
  layout(title = "Feature Importance for CTR Prediction",
         xaxis = list(title = "Feature", tickangle = 45),
         yaxis = list(title = "Importance (%IncMSE)"),
         margin = list(b = 150))

# --- Individual Countries Visualizations ---

# Visualization 2: Interactive Violin Plot of CTR by Age and Audience
# Purpose: Explore CTR distribution across age groups, segmented by audience
p2 <- plot_ly(data = individual_data, 
              x = ~Age, 
              y = ~CTR, 
              color = ~Audience, 
              colors = color_palette[2:3], 
              type = "violin",
              box = list(visible = TRUE),
              meanline = list(visible = TRUE),
              hoverinfo = "text",
              text = ~paste("Age: ", Age, "<br>",
                            "Audience: ", Audience, "<br>",
                            "CTR: ", round(CTR, 2), "%")) %>%
  layout(title = "CTR Distribution by Age and Audience (Individual Countries)",
         xaxis = list(title = "Age Group"),
         yaxis = list(title = "Click-Through Rate (%)"),
         legend = list(title = list(text = "Audience")))

# Visualization 3: Interactive Stacked Bar Plot of Clicks by Audience and Geography
# Purpose: Compare total clicks across audiences for each country
click_geo_data <- individual_data %>% 
  group_by(Geography, Audience) %>% 
  summarise(Total_Clicks = sum(Clicks), .groups = "drop")
p3 <- plot_ly(data = click_geo_data, 
              x = ~Geography, 
              y = ~Total_Clicks, 
              color = ~Audience, 
              colors = color_palette[4:5], 
              type = "bar",
              hoverinfo = "text",
              text = ~paste("Country: ", Geography, "<br>",
                            "Audience: ", Audience, "<br>",
                            "Clicks: ", Total_Clicks)) %>%
  layout(title = "Total Clicks by Audience and Country",
         xaxis = list(title = "Country", tickangle = 45),
         yaxis = list(title = "Total Clicks"),
         barmode = "stack",
         legend = list(title = list(text = "Audience")),
         margin = list(b = 150))

# Visualization 4: Interactive Scatter Plot of Reach vs. Impressions
# Purpose: Explore relationship between Reach and Impressions, colored by Audience
p4 <- plot_ly(data = individual_data, 
              x = ~Reach, 
              y = ~Impressions, 
              color = ~Audience, 
              colors = color_palette[6:7], 
              type = "scatter", 
              mode = "markers",
              hoverinfo = "text",
              text = ~paste("Country: ", Geography, "<br>",
                            "Audience: ", Audience, "<br>",
                            "Reach: ", Reach, "<br>",
                            "Impressions: ", Impressions)) %>%
  layout(title = "Reach vs. Impressions by Audience (Individual Countries)",
         xaxis = list(title = "Reach"),
         yaxis = list(title = "Impressions"),
         legend = list(title = list(text = "Audience")))

# --- Group Data Visualizations ---

# Visualization 5: Interactive Bar Plot of Reach by Group and Audience
# Purpose: Compare Reach for Group 1 and Group 2, colored by audience
p5 <- plot_ly(data = group_data, 
              x = ~Geography, 
              y = ~Reach, 
              color = ~Audience, 
              colors = color_palette[8:9], 
              type = "bar",
              hoverinfo = "text",
              text = ~paste("Group: ", Geography, "<br>",
                            "Audience: ", Audience, "<br>",
                            "Reach: ", Reach)) %>%
  layout(title = "Reach by Group and Audience",
         xaxis = list(title = "Group"),
         yaxis = list(title = "Reach (Unique Views)"),
         barmode = "group",
         legend = list(title = list(text = "Audience")))

# Visualization 6: Interactive Bar Plot of CTR by Group and Age
# Purpose: Compare CTR across age groups for Group 1 and Group 2
p6 <- plot_ly(data = group_data, 
              x = ~Age, 
              y = ~CTR, 
              color = ~Geography, 
              colors = color_palette[9:10], 
              type = "bar",
              hoverinfo = "text",
              text = ~paste("Group: ", Geography, "<br>",
                            "Age: ", Age, "<br>",
                            "CTR: ", round(CTR, 2), "%")) %>%
  layout(title = "CTR by Age and Group",
         xaxis = list(title = "Age Group"),
         yaxis = list(title = "Click-Through Rate (%)"),
         barmode = "group",
         legend = list(title = list(text = "Group")))

# Combine all plots into a single HTML file using subplot
combined_plot <- subplot(p1, p2, p3, p4, p5, p6, 
                         nrows = 3, 
                         titleX = TRUE, 
                         titleY = TRUE, 
                         margin = 0.05)

# Apply a consistent layout for better appearance
combined_plot <- combined_plot %>% 
  layout(title = list(text = "Interactive Facebook Ad Campaign Analysis", 
                      x = 0.5, 
                      font = list(size = 20, color = "#333333")),
         margin = list(t = 100, b = 100))

# Save the interactive visualizations as an HTML file
saveWidget(combined_plot, "facebook_ad_interactive_analysis.html", selfcontained = TRUE)

# Load required libraries for data manipulation and modeling
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(randomForest)# For feature importance
library(broom)       # For tidy regression output

# Read the Excel data (assuming the file is named 'Book1.xlsx')
# Replace 'path/to/Book1.xlsx' with the actual file path if needed
data <- read_excel("Book1.xlsx", sheet = "Sheet1")

# Clean column names to match previous program
colnames(data) <- c("Campaign_ID", "Campaign_Name", "Audience", "Age", "Geography", 
                    "Reach", "Impressions", "Frequency", "Clicks", "Unique_Clicks", 
                    "Unique_Link_Clicks", "CTR", "Unique_CTR", "Amount_Spent_INR", 
                    "CPC", "CPR", "Dropdown1", "Dropdown2")

# Remove unnecessary dropdown columns
data <- data %>% select(-Dropdown1, -Dropdown2)

# Separate data into individual countries and groups
individual_data <- data %>% 
  filter(!Geography %in% c("Group 1 (Australia, Canada, United Kingdom, Ghana, Nigeria, Pakistan, United States)", 
                           "Group 2 (Australia, Canada, United Kingdom, Ghana, Niger, Nigeria, Nepal, Pakistan, Thailand, Taiwan)"))
group_data <- data %>% 
  filter(Geography %in% c("Group 1 (Australia, Canada, United Kingdom, Ghana, Nigeria, Pakistan, United States)", 
                          "Group 2 (Australia, Canada, United Kingdom, Ghana, Niger, Nigeria, Nepal, Pakistan, Thailand, Taiwan)"))

# --- p1: Feature Importance for CTR Prediction ---
# Train random forest model for feature importance
rf_data <- individual_data %>% 
  select(Audience, Age, Geography, Reach, Impressions, Frequency, Amount_Spent_INR, CTR) %>%
  mutate(Audience = as.factor(Audience), Age = as.factor(Age), Geography = as.factor(Geography))
set.seed(123)
rf_model <- randomForest(CTR ~ ., data = rf_data, ntree = 100, importance = TRUE)
importance_df <- as.data.frame(importance(rf_model, type = 1)) %>%
  tibble::rownames_to_column(var = "Feature") %>%
  rename(Importance = `%IncMSE`) %>%
  mutate(Plot = "p1", Metric = "Importance (%IncMSE)")

# --- p2: CTR Distribution by Age and Audience ---
p2_data <- individual_data %>% 
  group_by(Age, Audience) %>% 
  summarise(Mean_CTR = mean(CTR, na.rm = TRUE),
            Median_CTR = median(CTR, na.rm = TRUE), .groups = "drop") %>%
  mutate(Plot = "p2", Metric = "Mean and Median CTR (%)")

# --- p3: Total Clicks by Audience and Country ---
p3_data <- individual_data %>% 
  group_by(Geography, Audience) %>% 
  summarise(Total_Clicks = sum(Clicks, na.rm = TRUE), .groups = "drop") %>%
  mutate(Plot = "p3", Metric = "Total Clicks")

# --- p4: Reach vs. Impressions by Audience ---
# Fit linear regression to estimate y-intercept
lm_model <- lm(Impressions ~ Reach, data = individual_data)
lm_summary <- tidy(lm_model) %>%
  filter(term == "(Intercept)") %>%
  select(estimate) %>%
  mutate(Plot = "p4", Metric = "Y-Intercept (Impressions)", Feature = "Intercept") %>%
  rename(Value = estimate)
p4_summary <- individual_data %>% 
  summarise(Min_Reach = min(Reach), Max_Reach = max(Reach),
            Min_Impressions = min(Impressions), Max_Impressions = max(Impressions)) %>%
  mutate(Plot = "p4", Metric = "Range", Feature = "Reach/Impressions") %>%
  tidyr::pivot_longer(cols = c(Min_Reach, Max_Reach, Min_Impressions, Max_Impressions),
                      names_to = "Sub_Metric", values_to = "Value")
p4_data <- bind_rows(lm_summary, p4_summary)

# --- p5: Reach by Group and Audience ---
p5_data <- group_data %>% 
  group_by(Geography, Audience) %>% 
  summarise(Total_Reach = sum(Reach, na.rm = TRUE), .groups = "drop") %>%
  mutate(Plot = "p5", Metric = "Total Reach")

# --- p6: CTR by Age and Group ---
p6_data <- group_data %>% 
  group_by(Age, Geography) %>% 
  summarise(Mean_CTR = mean(CTR, na.rm = TRUE), .groups = "drop") %>%
  mutate(Plot = "p6", Metric = "Mean CTR (%)")

# Combine all outputs into a single data frame
output_summary <- bind_rows(
  importance_df %>% select(Plot, Metric, Feature, Importance) %>% 
    rename(Value = Importance),
  p2_data %>% select(Plot, Metric, Age, Audience, Mean_CTR, Median_CTR) %>% 
    tidyr::pivot_longer(cols = c(Mean_CTR, Median_CTR), names_to = "Sub_Metric", values_to = "Value") %>%
    mutate(Feature = paste(Age, Audience, sep = "_")),
  p3_data %>% select(Plot, Metric, Geography, Audience, Total_Clicks) %>% 
    rename(Value = Total_Clicks) %>% mutate(Feature = paste(Geography, Audience, sep = "_")),
  p4_data %>% select(Plot, Metric, Feature, Value),
  p5_data %>% select(Plot, Metric, Geography, Audience, Total_Reach) %>% 
    rename(Value = Total_Reach) %>% mutate(Feature = paste(Geography, Audience, sep = "_")),
  p6_data %>% select(Plot, Metric, Age, Geography, Mean_CTR) %>% 
    rename(Value = Mean_CTR) %>% mutate(Feature = paste(Age, Geography, sep = "_"))
)

# Print the summary
print("Summary of Plot Outputs:")
print(output_summary)

# Save the summary as a CSV
write.csv(output_summary, "facebook_ad_plots_outputs.csv", row.names = FALSE)
