library(data.table)
library(ggplot2)
library(MASS)
library(ggrepel)

ridership <- fread("./data/SMART Ridership for Web Posting.csv", header = T)

setnames(ridership, c("Day of the Week", "Onboard Count","Notes About Day"), c("DOW","COUNT","NOTES"))

ridership[ , WEEKEND := "Weekday"]
ridership[DOW %in% c("Saturday","Sunday"), WEEKEND := "Weekend"]
ridership[ , DIGITDATE := as.Date(Date, "%m/%d/%Y")]

ridership[ , WEEK := strftime(DIGITDATE,"%V")]
ridership[ , MONTH := strftime(DIGITDATE,"%m")]
ridership[ , YEAR := strftime(DIGITDATE,"%Y")]


ridership[grepl("ribbon",NOTES, ignore.case = T), LABEL := NOTES]


#ridership[grepl("PSPS|Fire",NOTES, ignore.case = T), LABEL := "PSPS"]


###
ggplot(data=ridership, aes(x=DIGITDATE, y=COUNT, color=WEEKEND)) + 
  geom_point(size=0.8) +
  geom_smooth(method = "lm", se=F, aes(group = interaction(YEAR,WEEKEND))) +
  geom_label_repel(aes(label = LABEL), box.padding = 3, color='black', direction = 'y', segment.size = 1,
                   arrow = arrow(angle = 30, length = unit(0.125, "inches"), ends = "last", type = "open")) +
  #geom_smooth(method = "loess", span=0.5, se=F, aes(linetype = "Loess Moving Average", group=WEEKEND), color='gray50') +
  scale_x_date(NULL, date_labels = "%b-%y", date_breaks = "2 month") +
  scale_y_continuous("Ridership", labels = scales::comma) +
  scale_color_brewer(NULL, palette = "Set1") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1))




