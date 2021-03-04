require(tidyverse)
require(openxlsx)
require(maps)
require(mapdata)


# directory -----------------------------------------------------
dir_input4 = "/Users/Yuki/Dropbox/sokouo1/春調査/Apr"
dir_output4 = "/Users/Yuki/Dropbox/sokouo1/春調査/Apr"

dir_input6 = "/Users/Yuki/Dropbox/sokouo1/春調査/Jun"
dir_output6 = "/Users/Yuki/Dropbox/sokouo1/春調査/Jun"

# data ----------------------------------------------------------
#=== 4月 ===#
setwd(dir_input4)
path = dir_input4
files = list.files(path)

df4 = NULL
for(i in 1:length(files)){
  temp = read.xlsx(paste0(files[i]))
  temp = temp %>% mutate(year = as.numeric(str_sub(files[i], 15, 18)))
  df4 = rbind(df4, temp)
}
df4$month = 4


#=== 6月 ===#
setwd(dir_input6)
path = dir_input6
files = list.files(path)

df6 = NULL
for(i in 1:length(files)){
  temp = read.xlsx(paste0(files[i]))
  temp = temp %>% mutate(year = as.numeric(str_sub(files[i], 19, 22)))
  df6 = rbind(df6, temp)
}
df6$month = 6


colnames(df4); colnames(df6)
# sp.# = 4 in April and 1 in Jun
df = rbind(df4[, c(1,2,5,8,9,10)], df6[, c(2,3,6,9,10,11)])
df = rbind(df4 %>% filter(魚種NO == 4) %>% select(STATIONコード, 水深, 漁獲尾数, year, month), df6 %>% filter(魚種NO == 1) %>% select(STATIONコード, 水深, 漁獲尾数, year, month))



# summary -------------------------------------------------------
# == 各年,各月ごとの漁獲尾数 == #
sum = df %>% dplyr::group_by(year, month) %>% dplyr::summarize(sum = sum(漁獲尾数))


# == 調査点ごとの漁獲尾数 == #
unique(df$STATIONコード)
setwd(dir_input4)
lonlat = read.csv("lonlat.csv", fileEncoding = "CP932")

df2 = df %>% group_by(year, month, STATIONコード, 水深) %>% summarize(sum = sum(漁獲尾数))
df2 = left_join(df2, lonlat, by = c("year", "month", "STATIONコード", "水深"))
df2 = df2 %>% mutate(lon = lon1+lon2/60, lat = lat1+lat2/60) %>% select(-lon1, -lon2, -lat1, -lat2)
summary(df2)

# map
japan = map_data("japan") %>% mutate(long = long - 0.01, lat = lat - 0.01)

g = ggplot() + 
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50") + 
  coord_map(xlim = c(140, 148), ylim = c(36, 44))
p = geom_point(data = df2 %>% na.omit(), aes(x = lon, y = lat, fill = sum), shape = 22, size = 3)
c = scale_fill_gradientn(colours = c("white", "blue", "cyan", "green", "yellow", "orange", "red"))
# c = scale_fill_gradientn(colours = c("white", "blue", "red"))
f = facet_wrap(~ year, ncol = 3)
labs = labs(title = "", x = "", y = "", fill = "Number")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           # axis.text.x = element_text(size = rel(1.8), colour = "black"),
           # axis.text.y = element_text(size = rel(1.8), colour = "black"),
           # axis.text.x = element_text(size = rel(1.5), angle = 90, colour = "black"),
           # axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           strip.text.x = element_text(size = rel(1.3)),
           plot.title = element_text(size = rel(1.3)),
           legend.title = element_text(size = rel(1.3)),
           legend.text = element_text(size = rel(1.3)))

g+p+c+f+theme_bw(base_family = "HiraKakuPro-W3")+labs+th


# == 集計し直し == #
df2 = df2 %>% group_by(year, month, STATIONコード, 水深) %>% summarise(sum = sum(sum))

df3 = left_join(df2, sum %>% dplyr::rename(total = sum), by = c("year", "month")) %>% mutate(freq = sum/total)

check = df3 %>% group_by(year, month, STATIONコード, 水深) %>% summarise(sum = sum(sum), total = mean(total), freq = sum(freq))

setwd(dir_output4)
write.csv(df3, "df3.csv")
write.csv(check, "check.csv")
