rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

libs <- c("tidyverse", "ggplot2", "ggrepel", "scales")
lapply(libs, require, character.only = TRUE)


palette <- c("#003964", "#00bbce", "#A7C539", "#D33E2C",
             "#F15B43", "#E4E541", "#00665a", "#D9D9D9", "#000000")

theme_kse <- theme_minimal() +
  theme(
    panel.grid            = element_blank(),
    plot.background       = element_rect(fill = "white", color = "white"),
    axis.line             = element_blank(),
    text                  = element_text(color = palette[1], size = 13, face = "bold"),
    axis.title.x          = element_text(margin = margin(t = 10)),
    axis.title.y          = element_text(margin = margin(r = 10)),
    axis.text.x           = element_text(angle = 0, vjust = 0.5, color = palette[1],
                                         margin = margin(t = 10)),
    axis.text.y           = element_text(color = palette[1], margin = margin(r = 10)),
    panel.grid.major.y    = element_line(color = palette[2], linewidth = 0.25),
    legend.title          = element_blank(),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title   = element_text(margin = margin(t = 5, r = 5, b = 10, l = 5), size = 14),
    plot.caption = element_text(hjust = 0, face = "italic",
                                margin = margin(t = 5, r = 5, b = 3, l = 5)),
    axis.ticks.x = element_line(color = palette[1], linewidth = 1)
  )

save_fig <- function(gg_obj, name) {
  ggsave(filename = paste0("figures/", name, ".png"), plot = gg_obj,
         width = 8 * 14 / 5, height = 5.5 * 14 / 5, units = "cm", bg = "#FFFFFF")
  invisible(gg_obj)
}

dir.create("figures", showWarnings = FALSE)


classify_country <- function(df) {
  df %>% mutate(
    region = case_when(
      country %in% c(
        "France","Germany","Austria","Belgium","Netherlands","Luxembourg",
        "Switzerland","Ireland","United Kingdom","Norway","Sweden","Denmark",
        "Finland","Iceland","Portugal","Spain","Italy","Greece","Malta",
        "Estonia","Latvia","Lithuania","Slovenia","Slovakia","Czechia","Croatia",
        "Poland","Hungary","Romania","Bulgaria","Cyprus","Guernsey","Isle of Man",
        "Gibraltar","Principality of Monaco"
      ) ~ "Western Europe",

      country %in% c(
        "Ukraine","Russia","Belarus","Serbia","Albania","Kosovo","Montenegro",
        "Moldova","Armenia","Georgia","Azerbaijan","Kazakhstan","Kyrgyzstan",
        "Tajikistan","Uzbekistan","Turkmenistan"
      ) ~ "Eastern Europe",

      country %in% c(
        "Turkey","United Arab Emirates","Qatar","Saudi Arabia","Israel","Egypt",
        "Iran","Iraq","Lebanon","Syria","Libya","Algeria","Tunisia","Morocco",
        "Bahrain","Oman","Yemen","Djibouti","Jordan"
      ) ~ "Middle East & N. Africa",

      country %in% c(
        "South Africa","Nigeria","Ethiopia","Kenya","Ghana","Ivory Coast",
        "Cameroon","Uganda","Tanzania","Zimbabwe","Mozambique","Angola","Sudan",
        "Chad","Niger","Mali","Zambia","Botswana","Namibia","Rwanda","Eswatini",
        "Benin","Togo","Burundi","Sierra Leone","Liberia","Guinea","Madagascar",
        "Central Africa","Congo","Congo (Dem. Republic)","Eritrea","Comoros",
        "Cape Verde","Lesotho","Gabon","Equatorial Guinea","Mauritania","Somalia",
        "South Sudan","Papua New Guinea","Cocos (Keeling) Islands"
      ) ~ "Sub-Saharan Africa",

      country %in% c(
        "Japan","South Korea","Australia","New Zealand","China","India",
        "Indonesia","Malaysia","Philippines","Vietnam","Thailand","Cambodia",
        "Laos","Burma","Bangladesh","Sri Lanka","Pakistan","Nepal","Bhutan",
        "Singapore","Hong Kong","Macao","Taiwan","Mongolia","Timor-Leste"
      ) ~ "Asia-Pacific",

      country %in% c(
        "United States","Canada","Mexico","Brazil","Argentina","Colombia",
        "Chile","Ecuador","Peru","Bolivia","Paraguay","Uruguay","Venezuela",
        "Dominican Republic","Honduras","Guatemala","El Salvador","Nicaragua",
        "Costa Rica","Panama","Cuba","Puerto Rico","Bermuda","Bahamas",
        "Barbados","Cayman Islands","Turks and Caicos Islands","Virgin Islands",
        "American Samoa","Haiti","Suriname"
      ) ~ "Americas",

      country %in% c(
        "Marshall Islands","Kiribati","Tonga","Samoa","Fiji",
        "Vanuatu","Solomon Islands"
      ) ~ "Pacific Islands",

      TRUE ~ "Other"
    ),

    dev_level = case_when(
      country %in% c(
        # Western Europe
        "France","Germany","Austria","Belgium","Netherlands","Luxembourg",
        "Switzerland","Ireland","United Kingdom","Norway","Sweden","Denmark",
        "Finland","Iceland","Portugal","Spain","Italy","Greece","Malta",
        "Estonia","Latvia","Lithuania","Slovenia","Slovakia","Czechia","Croatia",
        "Cyprus","Guernsey","Isle of Man","Gibraltar","Principality of Monaco",
        # Anglosphere
        "United States","Canada","Australia","New Zealand",
        # East Asia
        "Japan","South Korea","Singapore","Hong Kong","Macao","Taiwan",
        # Gulf
        "Qatar","United Arab Emirates","Bahrain","Saudi Arabia",
        # Other high-income
        "Israel","Bermuda","Cayman Islands","Virgin Islands","Puerto Rico"
      ) ~ "Advanced",

      country %in% c(
        "China","India","Brazil","Russia","Turkey","Mexico","Indonesia",
        "Malaysia","Thailand","South Africa","Argentina","Colombia","Chile",
        "Poland","Hungary","Romania","Bulgaria","Ukraine","Egypt","Morocco",
        "Vietnam","Philippines","Pakistan","Bangladesh","Kazakhstan",
        "Azerbaijan","Georgia","Armenia","Ecuador","Dominican Republic",
        "Serbia","Albania","Kosovo","Montenegro","Moldova","Bolivia","Paraguay",
        "Guatemala","Honduras","El Salvador","Nicaragua","Tunisia","Algeria",
        "Libya","Iraq","Iran","Lebanon","Nigeria","Ghana","Ivory Coast",
        "Cameroon","Kenya","Angola","Sudan","Zimbabwe","Uganda","Tanzania",
        "Mozambique","Zambia","Ethiopia","Gabon","Equatorial Guinea","Kyrgyzstan",
        "Tajikistan","Uzbekistan","Turkmenistan","Laos","Cambodia","Sri Lanka",
        "Nepal","Burma","Bhutan","Timor-Leste","Mongolia","Namibia",
        "Botswana","Venezuela","Peru","Uruguay","Panama","Costa Rica","Cuba",
        "Suriname","Oman","Jordan","Syria","Yemen","Djibouti","Eswatini",
        "Papua New Guinea","Rwanda","Benin","Togo","Senegal","Mali","Mauritania"
      ) ~ "Emerging",

      TRUE ~ "Developing"
    )
  )
}


clean <- function(df) {
  df %>% drop_na() %>% mutate(across(where(is.character), trimws))
}

corruption   <- clean(read.csv("data/corruption.csv"))
cost_living  <- clean(read.csv("data/cost_of_living.csv"))
richest      <- clean(read.csv("data/richest_countries.csv"))
tourism      <- clean(read.csv("data/tourism.csv"))
unemployment <- clean(read.csv("data/unemployment.csv"))


world <- corruption %>%
  full_join(cost_living,  by = "country") %>%
  full_join(richest,      by = "country") %>%
  full_join(tourism,      by = "country") %>%
  full_join(unemployment, by = "country") %>%
  classify_country() %>%
  mutate(
    income_tier = case_when(
      annual_income >= 50000 ~ "High Income (≥$50k)",
      annual_income >= 15000 ~ "Upper-Middle ($15k–50k)",
      annual_income >= 5000  ~ "Lower-Middle ($5k–15k)",
      !is.na(annual_income)  ~ "Low Income (<$5k)"
    ),
    income_tier = factor(income_tier, levels = c(
      "High Income (≥$50k)", "Upper-Middle ($15k–50k)",
      "Lower-Middle ($5k–15k)", "Low Income (<$5k)"
    )),
    value_ratio = purchasing_power_index / cost_index,
    col_quadrant = case_when(
      cost_index >= 100 & purchasing_power_index >= 70 ~ "Expensive & High Power",
      cost_index >= 100 & purchasing_power_index < 70  ~ "Expensive & Low Power",
      cost_index <  100 & purchasing_power_index >= 50 ~ "Affordable & High Power",
      !is.na(cost_index)                               ~ "Affordable & Low Power"
    ),
    unemp_rank = rank(desc(unemployment_rate), na.last = "keep"),
    unemp_z    = round(
      (unemployment_rate - mean(unemployment_rate, na.rm = TRUE)) /
        sd(unemployment_rate, na.rm = TRUE), 2)
  )

cat(sprintf("\nWorld dataset: %d countries, %d columns\n",
            nrow(world), ncol(world)))


w_corruption   <- world %>% filter(!is.na(annual_income))
w_cost         <- world %>% filter(!is.na(cost_index))
w_richest      <- world %>% filter(!is.na(gdp_per_capita))
w_tourism      <- world %>% filter(!is.na(tourists_in_millions))
w_unemployment <- world %>% filter(!is.na(unemployment_rate))


# Corruption/Income
cat("Corruption ~ Income correlation")
r <- cor(w_corruption$annual_income, w_corruption$corruption_index)
cat(sprintf("Pearson r = %.3f  (strong negative: richer → less corrupt)\n", r))

cat("Avg income & corruption by income tier")
w_corruption %>%
  group_by(income_tier) %>%
  summarise(avg_income     = round(mean(annual_income), -2),
            avg_corruption = round(mean(corruption_index), 1),
            n              = n()) %>% print()

cat("Corruption outliers (residuals from linear trend)")
trend_model    <- lm(corruption_index ~ annual_income, data = w_corruption)
corruption_resid <- w_corruption %>%
  mutate(predicted = round(predict(trend_model), 1),
         residual  = round(corruption_index - predicted, 1))

cat("-- More corrupt than expected --\n")
corruption_resid %>% filter(residual > 0) %>% arrange(desc(residual)) %>%
  select(country, annual_income, corruption_index, predicted, residual) %>%
  head(4) %>% print()

cat("-- Less corrupt than expected --\n")
corruption_resid %>% filter(residual < 0) %>% arrange(residual) %>%
  select(country, annual_income, corruption_index, predicted, residual) %>%
  head(4) %>% print()

cat("-- Closest to trend --\n")
corruption_resid %>% arrange(abs(residual)) %>%
  select(country, annual_income, corruption_index, predicted, residual) %>%
  head(5) %>% print()

# Cost of living
cat("Cost of living: distribution")
w_cost %>%
  summarise(mean_cost   = round(mean(cost_index), 1),
            median_cost = round(median(cost_index), 1),
            mean_pp     = round(mean(purchasing_power_index), 1),
            median_pp   = round(median(purchasing_power_index), 1)) %>% print()

cat("Correlations")
cat(sprintf("r(income, PP)   = %.3f\n", cor(w_cost$monthly_income, w_cost$purchasing_power_index)))
cat(sprintf("r(cost,   PP)   = %.3f\n", cor(w_cost$cost_index,     w_cost$purchasing_power_index)))
cat(sprintf("r(cost, income) = %.3f\n", cor(w_cost$cost_index,     w_cost$monthly_income)))

cat("Linear model: PP ~ cost_index")
col_model <- lm(purchasing_power_index ~ cost_index, data = w_cost)
cat(sprintf("R² = %.3f,  slope = %.3f\n", summary(col_model)$r.squared, coef(col_model)[2]))

cat("Outliers: residuals from PP ~ cost trend")
cost_resid <- w_cost %>%
  mutate(predicted_pp = round(predict(col_model), 1),
         residual     = round(purchasing_power_index - predicted_pp, 1),
         z_score      = round((residual - mean(residual)) / sd(residual), 2))

cat("-- Far above trend --\n")
cost_resid %>% filter(z_score > 1.5) %>% arrange(desc(z_score)) %>%
  select(country, cost_index, purchasing_power_index, predicted_pp, residual, z_score) %>% print()

cat("-- Far below trend --\n")
cost_resid %>% filter(z_score < -1.5) %>% arrange(z_score) %>%
  select(country, cost_index, purchasing_power_index, predicted_pp, residual, z_score) %>% print()

cat(" Quadrant breakdown")
w_cost %>% count(col_quadrant) %>%
  mutate(pct = round(100 * n / sum(n), 0)) %>% print()

cat("Best value (PP / cost ratio)")
w_cost %>% arrange(desc(value_ratio)) %>%
  select(country, cost_index, monthly_income, purchasing_power_index, value_ratio) %>%
  head(8) %>% print()

cat("Worst value (PP / cost ratio)")
w_cost %>% arrange(value_ratio) %>%
  select(country, cost_index, monthly_income, purchasing_power_index, value_ratio) %>%
  head(8) %>% print()

cat("High cost + low income (<$3000/mo)")
w_cost %>% filter(cost_index > 100, monthly_income < 3000) %>%
  arrange(monthly_income) %>%
  select(country, cost_index, monthly_income, purchasing_power_index) %>% print()

# Tourism
cat("Tourism by world region")
w_tourism %>%
  group_by(region) %>%
  summarise(countries        = n(),
            total_tourists_M = round(sum(tourists_in_millions), 1),
            avg_per_tourist  = round(mean(receipts_per_tourist), 0),
            avg_pct_gdp      = round(mean(percentage_of_gdp), 1)) %>%
  arrange(desc(total_tourists_M)) %>% print()

cat("Tourism by development level")
w_tourism %>%
  group_by(dev_level) %>%
  summarise(countries        = n(),
            total_tourists_M = round(sum(tourists_in_millions), 1),
            avg_per_tourist  = round(mean(receipts_per_tourist), 0),
            avg_pct_gdp      = round(mean(percentage_of_gdp), 1)) %>%
  arrange(desc(total_tourists_M)) %>% print()

cat("Top 5 by volume")
w_tourism %>% arrange(desc(tourists_in_millions)) %>%
  select(country, tourists_in_millions, receipts_in_billions, receipts_per_tourist) %>%
  head(5) %>% print()

cat("Top 5 by total revenue")
w_tourism %>% arrange(desc(receipts_in_billions)) %>%
  select(country, tourists_in_millions, receipts_in_billions, receipts_per_tourist) %>%
  head(5) %>% print()

cat("Top 5 revenue per tourist")
w_tourism %>% arrange(desc(receipts_per_tourist)) %>%
  select(country, tourists_in_millions, receipts_per_tourist, percentage_of_gdp) %>%
  head(5) %>% print()

cat("Top GDP dependence")
w_tourism %>% arrange(desc(percentage_of_gdp)) %>%
  select(country, tourists_in_millions, receipts_per_tourist, percentage_of_gdp) %>%
  head(8) %>% print()

# Unemployment
cat("Unemployment distribution")
w_unemployment %>%
  summarise(mean   = round(mean(unemployment_rate), 1),
            median = round(median(unemployment_rate), 1),
            sd     = round(sd(unemployment_rate), 1),
            q25    = quantile(unemployment_rate, .25),
            q75    = quantile(unemployment_rate, .75),
            min    = min(unemployment_rate),
            max    = max(unemployment_rate)) %>% print()

cat("Unemployment by region")
w_unemployment %>%
  group_by(region) %>%
  summarise(n      = n(),
            mean   = round(mean(unemployment_rate), 1),
            median = round(median(unemployment_rate), 1),
            min    = min(unemployment_rate),
            max    = max(unemployment_rate)) %>%
  arrange(desc(mean)) %>% print()

cat("Unemployment by development level")
w_unemployment %>%
  group_by(dev_level) %>%
  summarise(n      = n(),
            mean   = round(mean(unemployment_rate), 1),
            median = round(median(unemployment_rate), 1),
            min    = min(unemployment_rate),
            max    = max(unemployment_rate)) %>%
  arrange(desc(mean)) %>% print()

cat("Wealthy but high unemployment (GDP > $30k, rate > 7%)")
w_unemployment %>%
  filter(!is.na(gdp_per_capita), gdp_per_capita > 30000, unemployment_rate > 7) %>%
  arrange(desc(unemployment_rate)) %>%
  select(country, gdp_per_capita, unemployment_rate) %>% print()

cat("Developing countries with unemployment < 3%")
w_unemployment %>%
  filter(dev_level == "Developing", unemployment_rate < 3) %>%
  arrange(unemployment_rate) %>%
  select(country, unemployment_rate, region) %>% print()


## Figure 1: Corruption Index vs Annual Income ---------------------------------
label_c1 <- c("Denmark","Switzerland","Somalia","South Sudan","Venezuela",
               "United States","China","Ukraine","Norway","Singapore","India")

fig1 <- w_corruption %>%
  mutate(label = if_else(country %in% label_c1, country, "")) %>%
  ggplot(aes(x = annual_income, y = corruption_index, color = income_tier)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = palette[4],
              linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(aes(label = label), size = 3, fontface = "plain",
                  color = palette[1], max.overlaps = 15, seed = 42) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_color_manual(values = setNames(palette[1:4],
    c("High Income (≥$50k)", "Upper-Middle ($15k–50k)",
      "Lower-Middle ($5k–15k)", "Low Income (<$5k)"))) +
  labs(title = "Figure 1: Income per Capita and Corruption",
       x = "Annual Income per Capita (USD)", y = "Corruption Index") +
  theme_kse +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(color = palette[8], linewidth = 0.15))

save_fig(fig1, "fig1_corruption_income")


## Figure 2: Top 15 Richest Countries -----------------------------------------
fig2 <- w_richest %>%
  slice_max(gdp_per_capita, n = 15) %>%
  mutate(country = fct_reorder(country, gdp_per_capita),
         is_top5 = gdp_per_capita >= sort(gdp_per_capita, decreasing = TRUE)[5]) %>%
  ggplot(aes(x = gdp_per_capita, y = country, fill = is_top5)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = dollar(gdp_per_capita, prefix = "$", big.mark = ",")),
            hjust = -0.1, size = 3.2, color = palette[1], fontface = "bold") +
  scale_fill_manual(values = c(`FALSE` = palette[2], `TRUE` = palette[1]), guide = "none") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  expand_limits(x = max(w_richest$gdp_per_capita, na.rm = TRUE) * 1.18) +
  labs(title = "Figure 2: Top 15 Countries by GDP per Capita",
       x = "GDP per Capita (USD)", y = NULL) +
  theme_kse +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = palette[2], linewidth = 0.25))

save_fig(fig2, "fig2_gdp_per_capita")


## Figure 3: Tourism Volume vs Receipts per Tourist ---------------------------
label_t <- c("Australia","Qatar","Germany","France","South Korea","Hong Kong",
             "Moldova","United States","Japan","Macao","United Arab Emirates","Portugal")

fig3 <- w_tourism %>%
  mutate(label      = if_else(country %in% label_t, country, ""),
         is_notable = country %in% label_t) %>%
  ggplot(aes(x = tourists_in_millions, y = receipts_per_tourist)) +
  geom_point(aes(color = is_notable, size = percentage_of_gdp), alpha = 0.85) +
  geom_text_repel(aes(label = label), size = 3, color = palette[1],
                  fontface = "plain", max.overlaps = 20, seed = 42) +
  scale_color_manual(values = c(`FALSE` = palette[1], `TRUE` = palette[2]), guide = "none") +
  scale_size_continuous(range = c(2, 9), name = "% of GDP") +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "Figure 3: Tourism Volume vs Spending per Visitor",
       x = "International Tourists (millions)", y = "Receipts per Tourist (USD)") +
  theme_kse +
  theme(panel.grid.major.x = element_line(color = palette[2], linewidth = 0.2),
        legend.position = "bottom")

save_fig(fig3, "fig3_tourism")


## Figure 4: Unemployment ------------------------------------------------------
top10 <- w_unemployment %>% slice_min(unemp_rank, n = 10)
bot10 <- w_unemployment %>% slice_max(unemp_rank, n = 10)

unemp_plot <- bind_rows(top10 %>% mutate(group = "Highest"),
                        bot10 %>% mutate(group = "Lowest")) %>%
  mutate(country = fct_reorder(country, unemployment_rate))

fig4 <- unemp_plot %>%
  ggplot(aes(x = unemployment_rate, y = country, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(unemployment_rate, "%")),
            hjust = -0.1, size = 3.2, color = palette[1], fontface = "bold") +
  scale_fill_manual(values = c("Highest" = palette[1], "Lowest" = palette[2])) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  expand_limits(x = max(unemp_plot$unemployment_rate) * 1.13) +
  labs(title = "Figure 4: Countries with Highest & Lowest Unemployment",
       x = "Unemployment Rate (%)", y = NULL) +
  theme_kse +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = palette[2], linewidth = 0.25),
        legend.position = "top")

save_fig(fig4, "fig4_unemployment")


## Figure 5: Cost of Living vs Purchasing Power --------------------------------
label_c5 <- c("Bermuda","Switzerland","Singapore","Qatar","Haiti","New Caledonia",
               "Norway","United States","Luxembourg","Hong Kong","Iran","China",
               "Brunei","Saudi Arabia")

fig5 <- w_cost %>%
  mutate(label = if_else(country %in% label_c5, country, "")) %>%
  ggplot(aes(x = cost_index, y = purchasing_power_index, color = col_quadrant)) +
  geom_vline(xintercept = 100, linetype = "dashed", color = palette[9], linewidth = 0.4) +
  geom_hline(yintercept = 70,  linetype = "dashed", color = palette[9], linewidth = 0.4) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text_repel(aes(label = label), size = 3, fontface = "plain",
                  color = palette[1], max.overlaps = 20, seed = 42) +
  scale_color_manual(values = c(
    "Expensive & High Power"  = palette[1],
    "Expensive & Low Power"   = palette[2],
    "Affordable & High Power" = palette[3],
    "Affordable & Low Power"  = palette[4]
  )) +
  labs(title = "Figure 5: Cost of Living vs Purchasing Power",
       x = "Cost of Living Index (US = 100)", y = "Purchasing Power Index") +
  theme_kse +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(color = palette[2], linewidth = 0.15),
        panel.grid.major.y = element_line(color = palette[2], linewidth = 0.25))

save_fig(fig5, "fig5_cost_purchasing_power")

cat("\nAll figures saved to ./figures/\n")
