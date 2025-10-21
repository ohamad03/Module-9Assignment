setwd("/Users/user/Desktop/Rproo")

guns <- read.csv("/Users/user/Downloads/Guns.csv")

boxplot(violent ~ law, data = guns,
        main = "Violent Crime Rate by Gun Law",
        xlab = "Shall-Issue Law",
        ylab = "Violent Crime Rate (per 100,000)",
        col = c("lightcoral", "lightblue"))

png("base_r_plot1.png", width = 800, height = 600, res = 100)
boxplot(violent ~ law, data = guns,
        main = "Violent Crime Rate by Gun Law",
        xlab = "Shall-Issue Law",
        ylab = "Violent Crime Rate (per 100,000)",
        col = c("lightcoral", "lightblue"))
dev.off()

plot(guns$income, guns$murder,
     main = "Income vs Murder Rate",
     xlab = "Income (dollars)",
     ylab = "Murder Rate (per 100,000)",
     col = ifelse(guns$law == "yes", "darkgreen", "darkred"),
     pch = 19, cex = 0.8)
legend("topright", 
       legend = c("With Law", "Without Law"),
       col = c("darkgreen", "darkred"),
       pch = 19)

png("base_r_plot2.png", width = 800, height = 600, res = 100)
plot(guns$income, guns$murder,
     main = "Income vs Murder Rate",
     xlab = "Income (dollars)",
     ylab = "Murder Rate (per 100,000)",
     col = ifelse(guns$law == "yes", "darkgreen", "darkred"),
     pch = 19, cex = 0.8)
legend("topright", 
       legend = c("With Law", "Without Law"),
       col = c("darkgreen", "darkred"),
       pch = 19)
dev.off()

library(lattice)

guns$year_group <- cut(guns$year, 
                       breaks = c(1977, 1985, 1993, 1999),
                       labels = c("1977-1985", "1986-1993", "1994-1999"))

xyplot(murder ~ violent | year_group, 
       data = guns,
       main = "Murder vs Violent Crime by Time Period",
       xlab = "Violent Crime Rate",
       ylab = "Murder Rate",
       pch = 19,
       col = "darkblue")

png("lattice_plot1.png", width = 900, height = 600, res = 100)
print(xyplot(murder ~ violent | year_group, 
             data = guns,
             main = "Murder vs Violent Crime by Time Period",
             xlab = "Violent Crime Rate",
             ylab = "Murder Rate",
             pch = 19,
             col = "darkblue"))
dev.off()

bwplot(robbery ~ law, 
       data = guns,
       main = "Robbery Rate by Gun Law",
       xlab = "Shall-Issue Law",
       ylab = "Robbery Rate (per 100,000)",
       fill = c("coral", "skyblue"))

png("lattice_plot2.png", width = 800, height = 600, res = 100)
print(bwplot(robbery ~ law, 
             data = guns,
             main = "Robbery Rate by Gun Law",
             xlab = "Shall-Issue Law",
             ylab = "Robbery Rate (per 100,000)",
             fill = c("coral", "skyblue")))
dev.off()

library(ggplot2)

ggplot(guns, aes(x = year, y = violent, color = law)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Violent Crime Trends by Gun Law Status",
       x = "Year",
       y = "Violent Crime Rate",
       color = "Law Status") +
  theme_minimal()

ggsave("ggplot_plot1.png", width = 10, height = 6, dpi = 100)

ggplot(guns, aes(x = income, y = murder)) +
  geom_point(alpha = 0.4, color = "darkred") +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~ law) +
  labs(title = "Murder Rate vs Income by Law Status",
       x = "Income",
       y = "Murder Rate") +
  theme_minimal()

ggsave("ggplot_plot2.png", width = 10, height = 6, dpi = 100)

list.files()
