library(ggplot2)
library(ggsignif)
library(RColorBrewer)
re1 <- data.frame(word1 = c("Large", "Small", "High", "Low", "Large", "Small", "High", "Low"),
                  con1 = c("Chinese", "Chinese", "Chinese", "Chinese", "Foreigner", "Foreigner", "Foreigner", "Foreigner"), 
                  lpga1 = c(96.1837, 95.6953, 97.8945, 95.7940, 102.2625, 101.4181, 101.5467, 101.474),
                  slpga1 = c(11.1278, 11.0017, 13.7844, 10.7828, 10.7176, 9.9608, 10.4083, 10.34616))
re1$word1 = factor(re1$word1, levels=c("Large", "Small", "High", "Low")) 
p1 <- ggplot(data = re1, aes(x = con1, y = lpga1, fill = word1)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), colour="black", size = 0.8) +
  ylab("PGA(mm)") + xlab("") + coord_cartesian(ylim=c(60, 120)) +
  theme(panel.background = element_rect(fill="white", linetype="solid"),  
        plot.title = element_text(size = 18, vjust = 5, hjust = 0.5), 
        axis.title.y = element_text(size=14, vjust = 3, hjust = 0.5),
        axis.text.y = element_text(size=12, face="bold"), 
        axis.text.x = element_text(size=14, face="bold", colour="black"),
        axis.ticks.x = element_blank(), plot.margin=unit(c(2,1,0,1),'lines'), 
        axis.line = element_line(color="black",size=0.8), 
        legend.text = element_text(size=14), legend.title=element_blank()) +
  ggtitle("POWER GRASP") +
  scale_fill_manual(values=brewer.pal(4,"Blues")) +
  geom_errorbar(aes(ymin=lpga1-slpga1, ymax=lpga1+slpga1), 
                position = position_dodge(0.9), width = .2, size = 0.8) + 
  geom_signif(annotations = ("*"), y_position = c(112, 117), xmin= c(0.665,1.665), xmax= c(0.89,1.89), 
              textsize = 5, tip_length = 0.05, size = 0.8)
p1

re2 <- data.frame(word2 = c("Large", "Small", "High", "Low", "Large", "Small", "High", "Low"),
                  con2 = c("Chinese", "Chinese", "Chinese", "Chinese", "Foreigner", "Foreigner", "Foreigner", "Foreigner"), 
                  spga2 = c(53.3481, 52.5843, 53.4875, 52.6876, 50.6294, 49.6719, 50.0757, 50.3461),
                  sspga2 = c(11.3363, 11.4565, 12.9790, 11.2596, 7.3546, 8.0325, 7.3314, 8.0819))
re2$word2 = factor(re2$word2, levels=c("Large", "Small", "High", "Low")) 
p2 <- ggplot(data = re2, aes(x = con2, y = spga2, fill = word2)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), colour="black", size = 0.8)+
  ylab("PGA(mm)") + xlab("") + coord_cartesian(ylim=c(20, 80)) +
  theme(panel.background = element_rect(fill="white", linetype="solid"),  
        plot.title = element_text(size = 18, vjust = 5, hjust = 0.5), 
        axis.title.y = element_text(size=14, vjust = 3, hjust = 0.5),
        axis.text.y = element_text(size=12, face="bold"), 
        axis.text.x = element_text(size=14, face="bold", colour="black"),
        axis.ticks.x=element_blank(), plot.margin=unit(c(2,1,0,1),'lines'), 
        axis.line=element_line(color="black",size=0.8),) +
  ggtitle("PRECISION GRASP")  + guides(fill=FALSE) +
  scale_fill_manual(values=brewer.pal(4,"Blues")) +
  geom_errorbar(aes(ymin=spga2-sspga2, ymax=spga2+sspga2), 
                position = position_dodge(0.9), width = .2, size = 0.8) + 
  geom_signif(annotations = ("*"), y_position = c(70, 65), xmin= c(0.665,1.665), xmax= c(0.89,1.89),  
              textsize = 5, tip_length = 0.05, size = 0.8)
p2

re3 <- data.frame(word3 = c("President","Governor-A", "Governor-B", "Mayor", "President","Governor-A", "Governor-B", "Mayor"),
                  con3 = c("Chinese", "Chinese", "Chinese", "Chinese", "Foreigner", "Foreigner", "Foreigner", "Foreigner"), 
                  lpga3 = c(95.3459, 95.2289, 95.3618, 96.0420, 102.0236, 102.4503, 100.796, 102.4992),
                  slpga3 = c(11.78975, 11.35233, 12.3294, 9.5309, 10.48134, 10.15083, 11.28593, 11.05544))
re3$word3 = factor(re3$word3, levels=c("President","Governor-A", "Governor-B", "Mayor")) 
p3 <- ggplot(data = re3, aes(x = con3, y = lpga3, fill = word3)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), colour="black", size = 0.8)+
  ylab("PGA(mm)") + xlab("") + coord_cartesian(ylim=c(60, 120)) +
  theme(panel.background = element_rect(fill="white", linetype="solid"),  
        plot.title = element_text(size = 18, vjust = 5, hjust = 0.5), 
        axis.title.y = element_text(size=14, vjust = 3, hjust = 0.5),
        axis.text.y = element_text(size=12, face="bold"), 
        axis.text.x = element_text(size=14, face="bold", colour="black"),
        axis.ticks.x=element_blank(), plot.margin=unit(c(2,1,0,1),'lines'), 
        axis.line=element_line(color="black",size=0.8),
        legend.text = element_text(size=14), legend.title=element_blank()) +
  ggtitle("POWER GRASP") +
  scale_fill_manual(values=brewer.pal(4,"Greens")) +
  geom_errorbar(aes(ymin=lpga3-slpga3, ymax=lpga3+slpga3), 
                position = position_dodge(0.9), width = .2, size = 0.8) 
p3

re4 <- data.frame(word4 = c("President","Governor-A", "Governor-B", "Mayor", "President","Governor-A", "Governor-B", "Mayor"),
                  con4 = c("Chinese", "Chinese", "Chinese", "Chinese", "Foreigner", "Foreigner", "Foreigner", "Foreigner"), 
                  spga4 = c(54.3088, 51.7654, 54.9041, 54.0508, 51.0778, 50.0228, 50.4026, 50.3037),
                  sspga4 = c(13.51209, 12.91636, 11.22301, 11.60609, 8.26379, 8.58348, 9.30892, 9.25791))
re4$word4 = factor(re4$word4, levels=c("President","Governor-A", "Governor-B", "Mayor")) 
p4 <- ggplot(data = re4, aes(x = con4, y = spga4, fill = word4)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), colour="black", size = 0.8)+
  ylab("PGA(mm)") + xlab("") + coord_cartesian(ylim=c(20, 80)) +
  theme(panel.background = element_rect(fill="white", linetype="solid"),  
        plot.title = element_text(size = 18, vjust = 5, hjust = 0.5), 
        axis.title.y = element_text(size=14, vjust = 3, hjust = 0.5),
        axis.text.y = element_text(size=12, face="bold"), 
        axis.text.x = element_text(size=14, face="bold", colour="black"),
        axis.ticks.x=element_blank(), plot.margin=unit(c(2,1,0,1),'lines'), 
        axis.line=element_line(color="black",size=0.8),) +
  ggtitle("PRECISION GRASP")  + guides(fill=FALSE) +
  scale_fill_manual(values=brewer.pal(4,"Greens")) +
  geom_errorbar(aes(ymin=spga4-sspga4, ymax=spga4+sspga4), 
                position = position_dodge(0.9), width = .2, size = 0.8) + 
  geom_signif(annotations = ("*"), y_position = c(78, 72), xmin=c(0.665, 0.89), xmax=c(0.89, 1.115), 
              textsize = 5, tip_length = 0.05, size = 0.8)
p4

re5 <- data.frame(word5 = c("Large", "Small", "High", "Low", "", "President","Teacher-A", "Teacher-B", "Student"),
                  lpga5 = c(90.8409, 90.1353, 90.8256, 89.9597, 0, 90.315, 90.79, 89.36125, 89.984375),
                  slpga5 = c(5.95339, 5.53677, 6.0874, 5.83332, 0, 5.4830, 6.3105, 6.4655, 6.1495))
re5$word5 = factor(re5$word5, levels=c("Large", "Small", "High", "Low", "","President","Teacher-A", "Teacher-B", "Student")) 
p5 <- ggplot(data = re5, aes(x = word5, y = lpga5, fill = word5)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), colour="black", size = 0.8, width = 1) +
  ylab("PGA(mm)") + xlab("") + coord_cartesian(ylim=c(60, 110)) +
  theme(panel.background = element_rect(fill="white", linetype="solid"),  
        plot.title = element_text(size = 18, vjust = 5, hjust = 0.5), 
        axis.title.y = element_text(size=14, vjust = 3, hjust = 0.5),
        axis.text.y = element_text(size=12, face="bold"), 
        axis.text.x = element_text(size=14, face="bold", colour="black", vjust = 1, hjust = 1, angle = 45),
        axis.ticks.x=element_blank(), plot.margin=unit(c(2,1,0,1),'lines'), 
        axis.line=element_line(color="black",size=0.8)) +
  scale_x_discrete(expand = c(0, 1.5)) +
  ggtitle("POWER GRASP")  + guides(fill=FALSE) +
  scale_fill_manual(values=c(brewer.pal(5,"Blues"), brewer.pal(4,"Reds"))) +
  geom_errorbar(aes(ymin=lpga5-slpga5, ymax=lpga5+slpga5), 
                position = position_dodge(0.7), width = .2, size = 0.8) +
  geom_signif(annotations = ("*"), y_position = 102, xmin=1, xmax=2, 
              textsize = 5, tip_length = 0.02, size = 0.8) +
  geom_vline(aes(xintercept=5), linetype="dashed", size = 0.8)
p5

re6 <- data.frame(word6 = c("Large", "Small", "High", "Low", "", "President","Teacher-A", "Teacher-B", "Student"),
                  spga6 = c(50.1354, 49.1528, 48.8459, 49.0944, 0, 50.741875, 49.87875, 52.0210, 49.7613),
                  sspga6 = c(8.27321, 8.29888, 8.08662, 8.53483, 0, 8.6486, 10.7682, 8.3042, 8.1512))
re6$word6 = factor(re6$word6, levels=c("Large", "Small", "High", "Low", "","President","Teacher-A", "Teacher-B", "Student")) 
p6 <- ggplot(data = re6, aes(x = word6, y = spga6, fill = word6)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), colour="black", size = 0.8, width = 1) +
  ylab("PGA(mm)") + xlab("") + coord_cartesian(ylim=c(20, 70)) +
  theme(panel.background = element_rect(fill="white", linetype="solid"),  
        plot.title = element_text(size = 18, vjust = 5, hjust = 0.5), 
        axis.title.y = element_text(size=14, vjust = 3, hjust = 0.5),
        axis.text.y = element_text(size=12, face="bold"), 
        axis.text.x = element_text(size=14, face="bold", colour="black", vjust = 1, hjust = 1, angle = 45),
        axis.ticks.x=element_blank(), plot.margin=unit(c(2,1,0,1),'lines'), 
        axis.line=element_line(color="black",size=0.8)) +
  scale_x_discrete(expand = c(0, 1.5)) +
  ggtitle("PRECISION GRASP")  + guides(fill=FALSE) +
  scale_fill_manual(values=c(brewer.pal(5,"Blues"), brewer.pal(4,"Reds"))) +
  geom_errorbar(aes(ymin=spga6-sspga6, ymax=spga6+sspga6), 
                position = position_dodge(0.7), width = .2, size = 0.8) +
  geom_signif(annotations = ("*"), y_position = c(62,65), xmin=c(1,8), xmax=c(2,9), 
              textsize = 5, tip_length = 0.02, size = 0.8) +
  geom_vline(aes(xintercept=5), linetype="dashed", size = 0.8)
p6