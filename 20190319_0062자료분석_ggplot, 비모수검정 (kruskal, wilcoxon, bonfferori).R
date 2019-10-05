setwd("C:\\Users\\LEE Do Sup\\Desktop\\R\\Ildong")
result <- read.csv("20181122_0062_lucentis_eylea_migration_results.csv")
library(dplyr)
str(result)

result$癤풦D <- as.character(result$癤풦D)
str(result)

result <- result %>% 
  rename(ID = 癤풦D)

library(ggplot2)

summary_result <- summary(result)
summary_result

result$count <- as.numeric(result$count)
str(result)
summary_result <- summary(result)
summary_result

result$Group <- factor(result$Group, levels = c("Control", "Vehicle", "IDB0062-1.5uM", "IDB0062-3.0uM", "Ranibizumab-1.5uM", 
                                                "Ranibizumab-3.0uM", "Aflibercept-1.5uM", "Aflibercept-3.0uM"))

result_graph <- ggplot(result, aes(x = Group, y = count)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Control (without PDGF-BB), others (with PDGF-BB 30 ng/mL)") +
    ylab("cell count score") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

result_graph


result_m <- result[-(73:96),]

result_graph_2 <- ggplot(result_m, aes(x = Group, y = count)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Control (without PDGF-BB), others (with PDGF-BB 30 ng/mL)") +
  ylab("cell count score") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
result_graph_2  



#통계검증 정규성 분포부터 시
shapiro.test(result$count[result$Group == "CTL"]) # 정규성 없음
shapiro.test(result$count[result$Group == "Veh"])
shapiro.test(result$count[result$Group == "0062-1.5uM"])
shapiro.test(result$count[result$Group == "0062-3.0uM"])
shapiro.test(result$count[result$Group == "lucentis-1.5uM"]) 
shapiro.test(result$count[result$Group == "lucentis-3.0uM"])
shapiro.test(result$count[result$Group == "eylea-1.5uM"])
shapiro.test(result$count[result$Group == "eylea-3.0uM"]) # 정규성 없음

# 정규성이 없기 때문에 Kruskal-Wallis test 수행
result_kru <- kruskal.test(count ~ Group, data = result)
result_kru

# p-value가 충분히 낮으므로 군 간 차이가 존재함
# 비모수 검정의 경우 사후분석이 없기 때문에 Mann-Whitney U test를 6개 그룹 간에 수행 p값 0.05를 총 검정 횟수인 7C2로 나누어 줌
library(dplyr)
A <- result %>% 
  filter(Group == "Veh" | Group == "0062-1.5uM")
B <- result %>% 
  filter(Group == "Veh" | Group == "0062-3.0uM")
C <- result %>% 
  filter(Group == "Veh" | Group == "lucentis-1.5uM")
D <- result %>% 
  filter(Group == "Veh" | Group == "lucentis-3.0uM")
E <- result %>% 
  filter(Group == "Veh" | Group == "eylea-1.5uM")
f <- result %>% 
  filter(Group == "Veh" | Group == "eylea-3.0uM")


wilcox.test(count ~ Group, data = A)
wilcox.test(count ~ Group, data = B)
wilcox.test(count ~ Group, data = C)
wilcox.test(count ~ Group, data = D)
wilcox.test(count ~ Group, data = E)
wilcox.test(count ~ Group, data = f)

post <- c(0.01727, 7.396e-07, 0.0449, 0.5512, 0.0002744, 5.177e-06, 0.05/21)
post1 <- c("A", "B", "C", "D", "E", "f", "p")

kk <- data.frame(post, post1)
View(kk)
plot(kk$post ~ kk$post1)

pk <- data.frame(post/(0.05/21), post1)
View(pk)
plot(pk$post..0.05.21. ~ pk$post1)

#수치요약
result_summary <- result %>% 
  group_by(Group) %>% 
  summarise(avg = mean(count), std = sd(count))

result_summary

