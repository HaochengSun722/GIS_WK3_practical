library(sf)
library(sp)
library(here)
library(raster)
library(fs)
library(tidyverse)
library(raster)
library(rmapshaper)
library(ggplot2)
library(plotly)
library(gstat)###用于IDW反距离加权法插值

########################################

#####Chapter 3 Rasters, descriptive statistics and interpolation

########st_layers功能用于读取gpkg内部信息
st_layers(here("gadm36_AUS.gpkg"))

#######layer以0为结尾，在上面一步显示出的只有一个要素，是可以代表整个AUS的图层
Ausoutline <- st_read(here( "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
print(Ausoutline)
plot(Ausoutline$geom)###由于是multi-polygon，因此可以plot

####通过调取proj4-string来了解该图层的CRS
st_crs(Ausoutline)$proj4string
####[1] "+proj=longlat +datum=WGS84 +no_defs"这个表示其位于地球投影坐标系统，
####datadum为WGS84，基础原点为赤道（0，0），

#####如果一个图层没有坐标系CRS，则可以通过EPSG来设置其坐标，4326代表WGS84
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)
#####更准确来说完整步骤为
Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0') %>% 
  st_set_crs(4326)###set设置

#*同时，你需要不停的转化坐标：当前DATADUM为WGS84(地理空间坐标)，
#*我们需要将其转化为平面坐标已进行分析，由于layer位于Australia，
#*因此需要将WGS84转化为GDA94(代表了澳大利亚的平面投影坐标，代码为3112)
#*并在完成分析后将其转化为WGS84坐标存入Geopackage
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)###transform改变
print(AusoutlinePROJECTED)

###Raster part
jan<-raster(here("prac3_data", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan
plot(jan)

# set the proj 4 to a new object,由于加载的文件是raster格式，因此不能使用st_set_crs或者st_transform,要用Raster::projectRaster()
newproj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "###该描述为World Mollweide，projected坐标系，常用于全球层面
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  raster::projectRaster(., crs=newproj)
plot(pr1)
##########也可以用以下代码转回WGS84
pr1 <- pr1 %>%
  projectRaster(., crs="+init=epsg:4326")
plot(pr1)


#####加载数据
fs::dir_info("prac3_data/") ##dir_info()功能，用于查看当前directory下所有文件
listfiles<-dir_info("prac3_data/") %>%##将folder内全部文件导入listfiles
  filter(str_detect(path, ".tif")) %>%##以".tif"作为筛选依据，filter出path
  dplyr::select(path)%>%##选择path，此时所选择的文件构成了一个列表
  dplyr::pull()##pull()功能将列表转化为values
listfiles

###将所有tif数据转入raster stack,是具有相同空间范围和分辨率的栅格图层的集合。
worldclimtemp <- listfiles %>%
  raster::stack()##各文件数据形成一个layer，当前layer为12
worldclimtemp

###对layer进行重命名，由于是raster因此不能用dplyr::rename()
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(worldclimtemp) <- month

###以Australia的各个城镇作为参照，从全球climate temperature数据中提取出Australia的相关数据
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- raster::extract(worldclimtemp, samples)
###通过raster::extract(数据库，参照物)功能，提取数据

Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% ##AUcitytemp为matrix矩阵数据，as_tibble()功能将其转化为表格数据
  add_column(Site = site, .before = "Jan")##add_column(新数据=旧数据，.before="位置")


#####Part2 descriptive statistics

##hist()直方图功能
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
hist(as.numeric(Perthtemp))###统计表格内数据的出现频率，x为数据区间，y为数据的出现频率
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
hist(as.numeric(Perthtemp), 
     breaks=userbreak, ###数据间断的区间
     col="blue", ##颜色
     main="Histogram of Perth Temperature", ##表头
     xlab="Temperature", ##x轴
     ylab="Frequency")##y轴

histinfo <- Perthtemp %>%
  as.numeric()%>%
  hist(.)
histinfo
###可以看到直方图的具体信息，包括间断区间，各区间个数统计，各区间的密度，各区间的中间值

####👇1月平均气温在Australia的分布###

###运用rmapshaper::ms_simplify功能，对图形进行简化
AusoutSIMPLE<-Ausoutline %>%
  ms_simplify(.,keep=0.05)##保留百分之0.05的点
plot(AusoutSIMPLE$geom)
###不建议用，只有加快加载速度的用处，但有可能删掉重要的潜在因素

###根据澳大利亚范围polygon，对1月份平均气温数据进行剪切
####在叠合两组数据前，要先确定两组数据处于同一坐标系
crs(AusoutSIMPLE)
crs(worldclimtemp)

###通过crop功能使两组数据叠合
Austemp <- AusoutSIMPLE %>%
  # now crop our temp data to the extent
  crop(worldclimtemp,.)###crop(数据，边界polygon)
plot(Austemp)

####上一步得出来的是box范围的数据可视化，
###下一步则需要将数据锁定在Australia的范围
exactAus <- Austemp %>%
  mask(.,Ausoutline, na.rm=TRUE)#rm是remove的意思,指定na.rm=T，就会移除na数据
plot(exactAus)

####Austemp和exactAus都是光栅格数据raster bricks，
####brick可以和stack做一个对比，stack是一个图层组，但brick是单一的文件

###👇👇Histogram with ggplot
##传统方法
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")
#OR
#subset with the word Mar
hist(raster::subset(exactAus, "Mar"), col="red", main ="March temperature")###subset子集

###exactAus目前为光栅格数据，做ggpolt需要将其转化为dataframe或tibble数据
exactAusdf <- exactAus %>%
  as.data.frame()

####👇用ggplot显示March的各区间温度分布频率
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
plot(gghist)

###在上图纸的基础上，增加表示温度平均值的蓝线
# add a vertical line to the hisogram showing mean tempearture
##geom_vline()功能，geometry_vertical line,
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),#rm是remove的意思,指定na.rm=T，就会移除na数据
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

####👇用ggplot显示多个月份的各区间温度分布频率
###将Australia一到十二月的温度数据进行变形，变为一个只有月份和温度两个column的表格
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp")

##从其中选出两个月份的时间
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")

##分别得到两个月份的平局温度
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

####与之前做过的相同，plot出两个月份的直方图与平均值虚线
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram( position="identity",alpha=0.5)+###position="identity"表示分别表示data，
  ###如果没有这个部分，为了不相互遮挡数据，两者将叠加起来
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
###最后两行用于设置表格的形式,可用格式包括theme_gray,theme_bw,theme_linedraw...

data_complete_cases <- squishdata %>%
  drop_na()%>% ###drop_na()功能，消除所有表格中的NA
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))
###对month列指定级别，并按降序映射
###如果没有这步的话，后面出现多面直方图的顺序就会打乱

# Plot faceted histogram多面直方图
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+ ##如果把month和.位置互换，就可以得到垂直的
                         ##水平--对比温度temperature 垂直--对比频率frequency
  theme(plot.title = element_text(hjust = 0.5))
###完成后要与显示情况做对比，检验真实性

###👇an interactive histogram可交互式直方图，需要加载poltly
jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")##选出7月的温度

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")##选出1月的温度

# give axis titles给定x,y轴名称
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width给定区间宽度
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist
#####以上可做练习用，自己默写出来🎈🎈🎈🎈🎈🎈

Alltypedata <- squishdata %>%
  group_by(Month) %>%
  summarize (IQR = IQR(Temp, na.rm=TRUE),##interquartile range, IQR四分位间距
            max=max(Temp, na.rm=T),##最大值
            min = min(Temp, na.rm=TRUE),##最小值
            sd = sd(Temp, na.rm=TRUE),##standard deviation标准差
            mean = mean(Temp, na.rm=TRUE))##平均值
Alltypedata

#########Part 3 interpolation#######插值，将点插入栅格数据中

samplestemp<-AUcitytemp%>%
  cbind(.,samples)##cbind()功能，合并矩阵，cbind横向合并，rbind纵向合并

#我们需要告诉R，我们的数据LANT&LONG是空间数据
# convert samples temp to a data frame 
samplestemp<-samplestemp%>%
  sf::st_as_sf(.,coords = c("lon", "lat"), ###coords坐标
           crs =4326,agr = "constant")
samplestemp

plot(Ausoutline$geom)###显示Australia的polygon
plot(st_geometry(samplestemp), add=TRUE)###增加point上去

Ausoutline <- Ausoutline %>%
  st_transform(., 3112)
samplestemp <- samplestemp %>%
  st_transform(., 3112)
Ausoutline###GDA94坐标下的Australia轮廓
samplestemp###GDA94下的Australia温度并带有坐标

###由于sp可以设置栅格or像素大小，因此选用sp格式
samplestempSP <- samplestemp %>%
  as(., 'Spatial')###将sf对象转化为sp对象
AusoutlineSP <- Ausoutline %>%
  as(., 'Spatial')

###创建一个新的空的，dataframe表格，也就是栅格网格sp对象，目的是可以自定栅格大小
emptygrd <- as.data.frame(spsample(AusoutlineSP, n=1000, type="regular", cellsize=200000))
###spsample()创建sp对象；AusoutlinePROJECTED_SP源文件；n=xxx 栅格数量，可以忽略，会被后面栅格大小改写；
###cellsize=xxxxx，栅格大小，也是空间分辨率，单位为米。
names(emptygrd) <- c("X", "Y")
coordinates(emptygrd) <- c("X", "Y")
gridded(emptygrd) <- TRUE  # Create SpatialPixel object
fullgrid(emptygrd) <- TRUE  # Create SpatialGrid object
proj4string(emptygrd) <- proj4string(samplestempSP)
emptygrd <- emptygrd %>%
  st_transform(.,3112)

# Interpolate the grid cells using a power value of 2 反距离的幂值默认取2
interpolate <- gstat::idw(Jan ~ 1, samplestempSP, newdata=emptygrd, idp=2.0)

# Convert output to raster object 
ras <- raster(interpolate)
# Clip the raster to Australia outline
rasmask <- mask(ras, Ausoutline)
# Plot the raster
plot(rasmask)

library(gstat)
detach("package:gstat", unload=TRUE)

####自动获取数据，both WorldClim and GADM都是可以自动获取的
tmean_auto <- getData("worldclim", res=10, var="tmean")
tmean_auto <- tmean_auto/10
Aus_auto <- getData('GADM', country="AUS", level=0)
