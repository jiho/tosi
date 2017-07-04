library("readxl")
library("stringr")
library("tidyverse")

d <- read_excel("Fig_Time_Line.xlsx", sheet=2)
l <- read_excel("Fig_Time_Line.xlsx", sheet=3)

d <- gather(d, key="var", value="val", -Year) %>% na.omit() %>% arrange(Year)
d$var <- factor(d$var, levels=unique(d$var), labels=str_wrap(unique(d$var), ))

l$label <- ifelse(is.na(l$Event), l$Year, str_wrap(str_c(l$Year, " - ", l$Event), 30))


ggplot(d) +
  geom_path(aes(x=Year, y=val, colour=var), na.rm=TRUE) + facet_grid(var~., scales="free_y", labeller=label_wrap_gen(19)) +
  scale_x_continuous(breaks=l$Year, labels=l$label) +
  theme_classic(base_size=9) + theme(axis.text.x=element_text(angle=35, hjust=1, vjust=0), legend.position=c(0.4, 0.2), axis.title.y=element_blank(), panel.grid.major.x=element_line(colour="grey90"), strip.background=element_blank(), panel.background=element_blank())
ggsave("time_line.pdf", width=13, height=18, units="cm")
ggsave("time_line.svg", width=13, height=18, units="cm")

