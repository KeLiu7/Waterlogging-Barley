library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)
library(maptools)
library(sf)
library(rjson)
library(cowplot)
library(ggpubr)
library(scatterpie)
library(stats)
library(viridis)
library(gghalves)
library(agricolae)
library(plyr)
# devtools::install_github("stefanedwards/lemon")
library(lemon)
wd<-"F:/Apsim_prj/GCMs/GCMs6"
setwd(wd)
# Sys.sleep(1800)

# Combin Result -----------------------------------------------------------
library(stringr)

all.files<-list.files(path = ".",
                      pattern = "*[0e]_S[13]_V[TS].csv$",
                      full.names = T,include.dirs = T,recursive = T)

all.data<-NULL
for (file in all.files) {
  
  name1<-read.csv(file = file,header = T,sep = ",",skip = 3,nrows = 1)
  data1<-read.csv(file = file,header = F,sep = ",",skip = 5,na.strings = "?")
  names(data1)<-names(name1)
  data1<-data1[,c("HarvestingYear","HarvestingDay","zadok_stage","stage","StageName","SowingYear","sowing_date","SowingDay","SowingDate","SowingVar","emergence_das","emergence_date","end_of_juvenile_das","end_of_juvenile_date","floral_initiation_das","floral_initiation_date","flowering_das","flowering_date","maturity_das","maturity_date","germinationTTTarget","end_of_juvenileTTTarget","floral_initiationTTTarget","floweringTTTarget","maturityTTTarget","harvest_ripeTTTarget","TTAftersowing","yield","biomass","grain_no","grain_wt","barley_incrop_rain","barley_incrop_radn","barley_incrop_Tmax","barley_incrop_Tmin","watertable","TheRun","BarleyType","NStressZ3","NodayZ3","FertSup","MnTP_Pto_JV1","MnTP_Pto_JV2","MnTP_Pto_FIN","MnTP_Pto_FWR","MnTP_Pto_GF1","MnTP_Pto_GF2","MnTP_Co2_JV1","MnTP_Co2_JV2","MnTP_Co2_FIN","MnTP_Co2_FWR","MnTP_Co2_GF1","MnTP_Co2_GF2","MnOX_Pto_JV1","MnOX_Pto_JV2","MnOX_Pto_FIN","MnOX_Pto_FWR","MnOX_Pto_GF1","MnOX_Pto_GF2","NoD_GS_n_stress_expan","NoD_GS_n_stress_grain","NoD_GS_n_stress_pheno","NoD_GS_n_stress_photo","NoD_TP_Pto_JV1","NoD_TP_Pto_JV2","NoD_TP_Pto_FIN","NoD_TP_Pto_FWR","NoD_TP_Pto_GF1","NoD_TP_Pto_GF2","NoD_TP_Co2_JV1","NoD_TP_Co2_JV2","NoD_TP_Co2_FIN","NoD_TP_Co2_FWR","NoD_TP_Co2_GF1","NoD_TP_Co2_GF2","NoD_OX_Pto_JV1","NoD_OX_Pto_JV2","NoD_OX_Pto_FIN","NoD_OX_Pto_FWR","NoD_OX_Pto_GF1","NoD_OX_Pto_GF2","Days_JV1","Days_JV2","Days_FIN","Days_FWR","Days_GF1","Days_GF2","Nodays_GS","MnGS_n_stress_expan","MnGS_n_stress_grain","MnGS_n_stress_pheno","MnGS_n_stress_photo","SWSowing","CUM_outflow_lat","CUM_runoff","CUM_es","CUM_drain","SWHarvesting","GSR","EPA","CO2","TAV","AMP","IrrAMT")]
  
  data1$Site<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,5]
  data1$GCM<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,7]
  data1$WS<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,8]
  data1$years<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,9]
  data1$sdate<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,10]
  data1$Type<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,11]
  data1<-data1[,c(105:110,1:104)]
  all.data<-rbind(data1,all.data)
  
}
write.csv(x = all.data,file = "combin_result.csv",row.names = F)

# com.res<-read.delim(file = "combin.result",header = T)
# setDT(com.res)
# # names(com.res)
# 
# com.res1<-com.res[com.res$HarvestingYear!="()",]
# com.res1<- com.res1%>% mutate_each_(funs(as.numeric), c(7:10,12:15,17:42,45:116))
# # com.res1$Date<-as.Date(x = as.numeric(com.res1$HarvestingDay),origin = paste0(com.res1$HarvestingYear,"-1-1"))





# Yield loss Map ----------------------------------------------------------
com.res1<-all.data
setDT(com.res1)
yield1<-com.res1[com.res1$zadok_stage==100,c(1:6,12,43,44,34)]
yield.vs<-yield1[Type=="VS",]
names(yield.vs)[c(1,2)]<-c("site","Gcm")
yield.vs1<-reshape2::dcast(data = yield.vs,
      formula = site+Gcm+years+sdate+SowingYear+TheRun+BarleyType~WS+Type,
      value.var = "yield")
yield.vs1<-na.omit(yield.vs1)
yield.vs1$yield_lose<-yield.vs1$TT_VS-yield.vs1$TS_VS
# write.csv(x = yield.vs1,"yield_lose.csv")
yield.vs1[yield.vs1$yield_lose<0,"TT_VS"]<-yield.vs1[yield.vs1$yield_lose<0,"TS_VS"]
# is.na(yield.vs1$yield_lose)
yield.vs1$yield_lose<-yield.vs1$TT_VS-yield.vs1$TS_VS
yield.vs1$yield_losep<-(yield.vs1$TT_VS-yield.vs1$TS_VS)/yield.vs1$TT_VS*100

yield.change1<-yield.vs1[yield.vs1$years=="Baseline",]
yield.change1<-yield.change1[,c(-3,-9:-11)]
names(yield.change1)[7]<-"BaseYield"
yield.change11<-yield.change1[,c(-4,-5)] %>% group_by(site,Gcm,sdate,BarleyType) %>% summarise_all("mean",na.rm=T)
yield.change2<-merge(yield.change11,yield.vs1,all=T)
yield.change2$yieldchange<-(yield.change2$TS_VS-yield.change2$BaseYield)/yield.change2$BaseYield*100


p<-ggplot(data = yield2,
       aes(x=Type ,y=yield,group=Type,fill=Type))+
  # facet_grid(.~BarleyType+sdate+years,scales = "free_x")+
  geom_violin(alpha=0.2)+
  geom_boxplot(alpha=0.5,width=0.2 ,outlier.shape = NA)+
  # geom_jitter(color=mycolor[2*i],alpha=0.5,width=.12,size=3.2 )+
  scale_y_continuous(name = "Yield (kg ha-1)")+
  scale_fill_manual(values = c("#e9b805","#3e90e2"))+
  # coord_cartesian(ylim = c(-30,100))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(filename = "VS_VT_yield_box_viol.pdf",plot = p,width = 2.5,height = 4)


yield2[,c(5,8)] %>% group_by(Type) %>% summarise_all(.funs = function(x){y=quantile(x,probs = c(0.1,0.25,0.5,0.75,0.9),na.rm=T);return(y)})







write.csv(x = yield.change2,"yield_lose_change.csv")

yield.change2<-read.csv(file = "yield_lose_change.csv",header = T,row.names = 1)
# names(yield.vs2)
yield.vs2<-yield.change2[,c(1:4,6,5,9:13)] %>% group_by(site,Gcm,years,sdate,BarleyType,) %>% summarise_all("mean",na.rm=T)
yield.vs21<-yield.vs2[,-2]%>% group_by(site,years,sdate,BarleyType,) %>% summarise_all("median",na.rm=T)
yield.vs21[yield.vs21$site=="UkBu","site"]<-"UKBu"
# loca<-read.csv(file = "LAT&Long.csv",header = T)
loca<-read_excel(path = "LAT&Long.xlsx")
names(loca)[1]<-"Country"
setDT(loca)
loca1<-read_excel(path = "Pai_LAT_LON.xlsx")
names(loca1)[1]<-"site"
names(loca1)[4]<-"BarleyType"

yield.vs22<-read_excel(path = "Fig2.xlsx")
yield.vs3<-merge(x = yield.vs22,y = loca,by.x="site",by.y="Abbre")
yield.vs3<-merge(x = yield.vs3,y = loca1,by=c("site","BarleyType"))
# unique(yield.vs3$BarleyType)
write.csv(x = yield.vs3,file = "yield_lose_sum_percentage.csv")

yield.vs4<-yield.vs3[yield.vs3$years %in% c("2050","2090"),]
yield.vs4$type_sd<-paste0(yield.vs4$BarleyType,"_",yield.vs4$sdate,"_",yield.vs4$years)
yield.vs4$type_sd<-factor(x = yield.vs4$type_sd,levels=c("spring_S1_2050", "spring_S1_2090", "spring_S3_2050", "spring_S3_2090", "winter_S1_2050", "winter_S1_2090", "winter_S3_2050", "winter_S3_2090"))
# unique(yield.vs4$type_sd)
# spam<-read.csv(file = "spam2010V2r0_global_A_TH.csv",header = T)
# barley_a<-spam[,c(5,6,13)]
# write.csv(barley_a,file = "barley_a.csv")
# barley_a<-read.csv(file = "barley_a.csv",header = T,row.names = 1)

range(yield.vs4$yieldchange)
barley_a<-read_excel("BARLEY.xlsx")
barley_a1<-barley_a

mycolor<-c("#f6e39b","#e9b805","#f5d8af","#e19019","#fcbfb0","#f7471d","#eec0c0","#c82e2e","#9ec7f0","#3e90e2","#a2a2d9","#4646b3","#9fd8ad","#7eb535","#99c199","#348434")

nn<-c("spring_S1_2050", "spring_S1_2090", "spring_S3_2050", "spring_S3_2090", "winter_S1_2050", "winter_S1_2090", "winter_S3_2050", "winter_S3_2090")
mapplot<-NULL
box.p<-NULL
box.c<-NULL
i=1
for (i in 1:8) {
  plot.d<-yield.vs4[yield.vs4$type_sd==nn[i],]
  mapplot[[nn[i]]]<-ggplot()+
    # facet_grid(BarleyType+sdate~years)+
    borders("world", size= 0.05,colour="grey70")+
    geom_raster(data = barley_a1,aes(x=x,y=y),fill="#348434",alpha=0.7)+
    geom_point(data=plot.d,aes(x=Long, y=Lat),
               pch=21,size=2,color="black",fill="grey20")+
    geom_segment(data=plot.d,aes(x=Long, y=Lat,xend=X1,yend=Y1),color="black")+
    geom_scatterpie(data=plot.d, 
                    aes(x=X1, y=Y1, group=site,
                        r=(TT_VS-300)^(1/2)*0.12), 
                    cols = c("TS_VS","yield_lose"),alpha=0.9
    )+
    geom_scatterpie_legend(radius =(c(1000,3000,6000)-300)^(1/2)*0.12,
                           x = -130,y = -20,n=3,
                           labeller=function(x) round(c(1000,3000,6000),digits = 0))+
    scale_x_continuous(breaks = seq(-180,180,30),name = "")+
    scale_y_continuous(breaks = seq(-40,80,20),name = "")+
    scale_color_viridis_c(limits=c(-30,100),begin = 0.05,end = 0.95,direction = -1,
                          na.value = "#460d68")+
    # scale_color_gradientn(colours = viridis(n = 12,begin = 0,end = 1,option = "C"),
    #                       values = c(-30,120),breaks=seq(-30,120,30))+
    # scale_color_manual(values =mycolor[c(2*i-1,2*i)])+
    scale_fill_manual(values = mycolor[c(2*i-1,2*i)])+
    # scale_fill_gradient(low = "darkgreen",high = "darkgreen")+
    guides(fill = "none")+
    coord_quickmap(
      xlim = c(-180,180),ylim = c(-60,90),
      clip = "on",
      expand = F
    )+
    theme_bw()+
    theme(
      # legend.position = c(0.8,0.8),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_text(size = 16)
      # legend.spacing = 
    )
  
  box.p[[nn[i]]] <- ggplot(data = plot.d,
                          aes(x=sdate ,y=yield_losep))+
    # facet_grid(.~BarleyType+sdate+years,scales = "free_x")+
    geom_boxplot(fill=mycolor[2*i],alpha=0.5,outlier.shape = NA)+
    geom_jitter(color=mycolor[2*i],alpha=0.5,width=.12,size=3.2 )+
    # geom_violin(aes(fill=sdate),alpha=0.5)+
    scale_y_continuous(name = "",breaks = seq(-30,90,30))+
    coord_cartesian(ylim = c(-30,100))+
    theme_bw()+
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank()
    )
  # names(plot.d)
  # as.numeric(plot.d$type_sd)
  box.c[[nn[i]]] <- ggplot(data = plot.d,
                          aes(x=type_sd ,y=yieldchange))+
    # facet_grid(.~BarleyType+sdate+years,scales = "free_x")+
    # geom_boxplot(fill=mycolor[2*i],alpha=0.5,outlier.shape = NA)+
    geom_half_violin(color=mycolor[2*i-1],
                     fill=mycolor[2*i-1],side = "r",
                     alpha=0.5,na.rm = T)+
    geom_point(color=mycolor[2*i],alpha=0.5,size=3.2,
               position = position_jitter(width = .07))+
    # geom_violin(aes(fill=sdate),alpha=0.5)+
    scale_y_continuous(name = "",breaks = seq(-30,90,30),
                       limits = c(-30,100)
                       )+
    coord_cartesian(ylim = c(-30,100))+
    theme_bw()+
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank()
    )
  
}
# 
# p<-ggarrange(mapplot[["spring_S1_2050"]],box.p[["spring_S1_2050"]],mapplot[["spring_S1_2090"]],box.p[["spring_S1_2090"]],
#           mapplot[["spring_S3_2050"]],box.p[["spring_S3_2050"]],mapplot[["spring_S3_2090"]],box.p[["spring_S3_2090"]],
#           mapplot[["winter_S1_2050"]],box.p[["winter_S1_2050"]],mapplot[["winter_S1_2090"]],box.p[["winter_S1_2090"]],
#           mapplot[["winter_S3_2050"]],box.p[["winter_S3_2050"]],mapplot[["winter_S3_2090"]],box.p[["winter_S3_2090"]],
#           ncol = 4,nrow = 4,widths = rep(x = c(0.95,0.05),8),
#           align = "hv"
#           # common.legend = T,
#           # legend = "right",
#           # labels = LETTERS[1:8]
# )
# 
# ggsave(filename ="Yieldlose_Map_box.pdf",plot = p,width = 26,height = 20)

p1.1<-ggarrange(mapplot[["spring_S1_2050"]],
                mapplot[["winter_S1_2050"]],
                ncol = 1,nrow = 2,
                # widths = rep(x = c(0.95,0.05),8),
                align = "hv"
                # common.legend = T,
                # legend = "right",
                # labels = LETTERS[1:8]
)

ggsave(filename ="Yieldlose_Map_median_0322.pdf",plot = p1.1,width = 12,height = 11)

p1.2<-ggarrange(mapplot[["spring_S1_2090"]],mapplot[["spring_S3_2050"]],mapplot[["spring_S3_2090"]],
              mapplot[["winter_S1_2090"]],mapplot[["winter_S3_2050"]],mapplot[["winter_S3_2090"]],
              ncol = 3,nrow = 2,
              # widths = rep(x = c(0.95,0.05),8),
              align = "hv",
              # common.legend = T,
              # legend = "right",
              labels = LETTERS[1:6]
)

ggsave(filename ="Sup_Yieldlose_Map_median_03251.pdf",plot = p1.2,width = 36,height = 11)



p1<-ggarrange(mapplot[["spring_S1_2050"]],mapplot[["spring_S1_2090"]],
             mapplot[["spring_S3_2050"]],mapplot[["spring_S3_2090"]],
             mapplot[["winter_S1_2050"]],mapplot[["winter_S1_2090"]],
             mapplot[["winter_S3_2050"]],mapplot[["winter_S3_2090"]],
             ncol = 2,nrow = 4,
             # widths = rep(x = c(0.95,0.05),8),
             align = "hv"
             # common.legend = T,
             # legend = "right",
             # labels = LETTERS[1:8]
)

ggsave(filename ="Yieldlose_Map_median_0321.pdf",plot = p1,width = 24,height = 22)


p3<-ggarrange(box.p[["spring_S1_2050"]],box.p[["spring_S1_2090"]],
             box.p[["spring_S3_2050"]],box.p[["spring_S3_2090"]],
             box.p[["winter_S1_2050"]],box.p[["winter_S1_2090"]],
             box.p[["winter_S3_2050"]],box.p[["winter_S3_2090"]],
             ncol = 2,nrow = 4,
            # widths = rep(x = c(0.95,0.05),8),
             align = "hv"
             # common.legend = T,
             # legend = "right",
             # labels = LETTERS[1:8]
)

ggsave(filename ="Yieldlose_box.pdf",plot = p3,width = 1.7,height = 12)


p4<-ggarrange(box.c[["spring_S1_2050"]],box.c[["spring_S1_2090"]],
              box.c[["spring_S3_2050"]],box.c[["spring_S3_2090"]],
              box.c[["winter_S1_2050"]],box.c[["winter_S1_2090"]],
              box.c[["winter_S3_2050"]],box.c[["winter_S3_2090"]],
              ncol = 2,nrow = 4,
              # widths = rep(x = c(0.95,0.05),8),
              align = "hv"
              # common.legend = T,
              # legend = "right",
              # labels = LETTERS[1:8]
)

ggsave(filename ="Yieldchange_violin_remove.pdf",plot = p4,width = 1.7,height = 12)

write.csv(x=yield.vs4,file="yield_lose_percentage_summary.csv")



unique(yield.vs4$type_sd)
yield.vs4$yieldchange2<-(yield.vs4$TT_VS-yield.vs4$BaseYield)/yield.vs4$BaseYield*100
yield.vs4$type_sd<-factor(x = yield.vs4$type_sd,levels=c("spring_S1_2050","spring_S3_2050", "spring_S1_2090",  "spring_S3_2090", "winter_S1_2050", "winter_S1_2090", "winter_S3_2050", "winter_S3_2090"))

spcolor<-c("#e9b805","#e19019","#f7471d","#c82e2e")
wrcolor<-c("#3e90e2","#4646b3","#7eb535","#348434")

yc_sp<-ggplot(data = yield.vs4[yield.vs4$BarleyType=="spring",],
       aes(x=type_sd ,y=yieldchange,color=type_sd,fill=type_sd))+
  # facet_grid(.~BarleyType+sdate+years,scales = "free_x")+
  geom_jitter(aes(y=yieldchange2),alpha=0.1,width=.12,size=2 ,color="grey")+
  geom_boxplot(aes(y=yieldchange2),alpha=0.1,outlier.shape = NA,color="grey")+
  geom_jitter(alpha=0.2,width=.12,size=2 )+
  geom_boxplot(alpha=0.4,outlier.shape = NA,width=0.5)+
  # geom_violin(aes(fill=sdate),alpha=0.5)+
  scale_y_continuous(name = "",breaks = seq(-30,90,30))+
  scale_color_manual(values = spcolor)+
  scale_fill_manual(values = spcolor)+
  coord_cartesian(ylim = c(-30,100))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45,hjust = 1,size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

yc_wr<-ggplot(data = yield.vs4[yield.vs4$BarleyType=="winter",],
       aes(x=type_sd ,y=yieldchange,color=type_sd,fill=type_sd))+
  # facet_grid(.~BarleyType+sdate+years,scales = "free_x")+
  geom_jitter(aes(y=yieldchange2),alpha=0.1,width=.12,size=2 ,color="grey")+
  geom_boxplot(aes(y=yieldchange2),alpha=0.1,outlier.shape = NA,color="grey")+
  geom_jitter(alpha=0.2,width=.12,size=2 )+
  geom_boxplot(alpha=0.4,outlier.shape = NA,width=0.5)+
  # geom_violin(aes(fill=sdate),alpha=0.5)+
  scale_y_continuous(name = "",breaks = seq(-30,90,30))+
  scale_color_manual(values = wrcolor)+
  scale_fill_manual(values = wrcolor)+
  coord_cartesian(ylim = c(-30,100))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45,hjust = 1,size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

ggsave(filename = "yieldchange_spring.pdf",plot = yc_sp,width = 2.5,height = 7)
ggsave(filename = "yieldchange_winter.pdf",plot = yc_wr,width = 2.5,height = 7)

# yield_lose boxplot

yield.vs2
head(yield.vs2)
yield.vs2$years<-factor(x = yield.vs2$years,levels = c("Baseline","2050","2090"))

yield.lose1<-yield.vs2[,-2] %>% group_by(site,years,sdate,BarleyType,) %>% summarise_all("mean",na.rm=T)

p<-ggplot(data=yield.lose1,aes(x=years,fill=years,y=yield_lose,color=years))+
  facet_grid(BarleyType~sdate,scales = "free_y")+
  geom_point(alpha=0.4,shape=1,position = position_jitter(width = .2))+
  geom_boxplot(alpha=0.4,width=0.5,outlier.shape = NA)+
  # geom_violin(aes(fill=sdate),alpha=0.5)+
  scale_y_continuous(name = "Yield lose (kg ha-1)")+
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  # coord_cartesian(ylim = c(-30,100))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45,hjust = 1,size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

ggsave(filename = "Yield_lose.pdf",plot = p,width = 3,height = 7)


p<-ggplot(data=yield.vs2[,-2],aes(x=years,fill=years,y=yield_losep,color=years))+
  facet_grid(BarleyType~sdate,scales = "free_y")+
  # geom_point(alpha=0.4,shape=1,position = position_jitter(width = .2))+
  geom_boxplot(alpha=0.4,width=0.5,outlier.shape = NA)+
  # geom_violin(aes(fill=sdate),alpha=0.5)+
  scale_y_continuous(name = "Yield lose (kg ha-1)")+
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  # coord_cartesian(ylim = c(-30,100))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45,hjust = 1,size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

ggsave(filename = "Yield_losep_v.pdf",plot = p,width = 3,height = 7)

  yield.lose1[yield.lose1$yield_losep!=0,c(-1,-3)] %>% group_by(BarleyType,years) %>% summarise_all("median",na.rm=T)
  yield.lose1[yield.lose1$yield_losep!=0,c(-1,-3)] %>% group_by(BarleyType,years) %>% summarise_all("mean",na.rm=T)
  
  yield.vs2[,c(-1,-2,-4)] %>% group_by(BarleyType,years) %>% summarise_all("mean",na.rm=T)
  yield.vs2[,c(-1,-2,-4)] %>% group_by(BarleyType,years) %>% summarise_all("median",na.rm=T)
  

  yield.vs2[yield.vs2$yield_losep!=0,c(-1,-2,-4)] %>% group_by(BarleyType,years) %>% summarise_all("mean",na.rm=T)
  yield.vs2[yield.vs2$yield_losep!=0,c(-1,-2,-4)] %>% group_by(BarleyType,years) %>% summarise_all("median",na.rm=T)
  
yield.lose2<-read.csv(file="yield.lose4.csv",header=T)
yield.lose2$years<-factor(x = yield.lose2$years,levels = c("Baseline","2050","2090"))

p<-ggplot(data=yield.lose2,aes(x=years,fill=years,y=yield_lose,color=years))+
  facet_grid(BarleyType~sdate,scales = "free_y")+
  # geom_point(alpha=0.4,shape=1,position = position_jitter(width = .2))+
  geom_boxplot(alpha=0.4,width=0.5,outlier.shape = NA,coef=1.5)+
  # geom_violin(aes(fill=sdate),alpha=0.5)+
  scale_y_continuous(name = "Yield lose (kg ha-1)")+
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"))+
  # coord_cartesian(ylim = c(-30,100))+
  theme_bw()+
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45,hjust = 1,size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

ggsave(filename = "Yield_lose5.pdf",plot = p,width = 3,height = 7)





# KMeans plot -------------------------------------------------------------

# library(factoextra)
# names(com.res1)[c(1:6,12,43,44,grep(pattern = "NoD_OX_Pto",x = names(com.res1)))]
nn<-c(1,2,4,5,12,34,44,grep(pattern = "NoD_OX_Pto",x = names(com.res1)),grep(pattern = "Days_",x = names(com.res1)))
com.res1<-as.data.frame(com.res1)

wl.d<-com.res1[com.res1$zadok_stage==100 & com.res1$WS=="TS" & com.res1$Type=="VS",nn]

wl.d$Days_All<-wl.d$Days_JV1+wl.d$Days_JV2+wl.d$Days_FIN+wl.d$Days_FWR+wl.d$Days_GF1+wl.d$Days_GF2
wl.d$JV1<-wl.d$NoD_OX_Pto_JV1/wl.d$Days_JV1*100
wl.d$JV2<-wl.d$NoD_OX_Pto_JV2/wl.d$Days_JV2*100
wl.d$FIN<-wl.d$NoD_OX_Pto_FIN/wl.d$Days_FIN*100
wl.d$FWR<-wl.d$NoD_OX_Pto_FWR/wl.d$Days_FWR*100
wl.d$GF1<-wl.d$NoD_OX_Pto_GF1/wl.d$Days_GF1*100
wl.d$GF2<-wl.d$NoD_OX_Pto_GF2/wl.d$Days_GF2*100

wl.d$JV1_a<-wl.d$NoD_OX_Pto_JV1/wl.d$Days_All*100
wl.d$JV2_a<-wl.d$NoD_OX_Pto_JV2/wl.d$Days_All*100
wl.d$FIN_a<-wl.d$NoD_OX_Pto_FIN/wl.d$Days_All*100
wl.d$FWR_a<-wl.d$NoD_OX_Pto_FWR/wl.d$Days_All*100
wl.d$GF1_a<-wl.d$NoD_OX_Pto_GF1/wl.d$Days_All*100
wl.d$GF2_a<-wl.d$NoD_OX_Pto_GF2/wl.d$Days_All*100

write.csv(x = wl.d,file = "WLstress.csv")

wl.d<-read.csv(file = "WLstress.csv",header = T,row.names = 1)

# stage
# wl.d1<-wl.d[,c(1:6,20:25)]
# all
wl.d1<-wl.d[,c(1:7,27:32)]
# days
# wl.d1<-wl.d[,c(1:12)]
wl.d1$country<-substr(x = wl.d1[[1]],start = 0,stop = 2)
wl.d1[wl.d1$country=="Uk","country"]<-"UK"
ss<-c("Baseline","2050","2090")
all_kmeans<-NULL
set.seed(1234)
names(wl.d1)[8:13]<-c("JV1" ,"JV2" ,"FIN" ,"FWR" ,"GF1" ,"GF2")
write.csv(file = "WLd1.csv",x = wl.d1)
km.data<-NULL
km.all<-NULL
wl.pw<-NULL
i="2050"
km.alldata<-NULL
for (btype in unique(wl.d1$BarleyType)) {
  wl.btype<-wl.d1[wl.d1$BarleyType==btype,]
  
  for (i in unique(wl.btype$years)) {
    
    set.seed(1234)

    vs.d2<-wl.btype[wl.btype$years==i,]
    all_km<-kmeans(x = vs.d2[,8:13],4,nstart = 16)
    vs.d2$cluster <- as.factor(all_km$cluster)
    vs.d3<- vs.d2[,c(-1:-5,-7,-14)] %>% group_by(cluster) %>% summarise_all("mean")
    
    a1 <-nrow(vs.d2[vs.d2$cluster == 1,])/nrow(vs.d2)*100
    b1 <-nrow(vs.d2[vs.d2$cluster == 2,])/nrow(vs.d2)*100
    c1 <-nrow(vs.d2[vs.d2$cluster == 3,])/nrow(vs.d2)*100
    d1 <-nrow(vs.d2[vs.d2$cluster == 4,])/nrow(vs.d2)*100
    # e1 <-nrow(vs.d2[vs.d2$cluster == 5,])/nrow(vs.d2)*100
    
    vs.d3[vs.d3$cluster==1,"ratio"]<-a1
    vs.d3[vs.d3$cluster==2,"ratio"]<-b1
    vs.d3[vs.d3$cluster==3,"ratio"]<-c1
    vs.d3[vs.d3$cluster==4,"ratio"]<-d1
    # vs.d3[vs.d3$cluster==5,"ratio"]<-e1
    vs.d3[,"between_totss"]<-all_km[["betweenss"]]/all_km[["totss"]]*100
    setDT(vs.d3)
    vs.d3[,V_sum:=JV1+JV2+FIN+FWR+GF1+GF2]
    setorder(x = vs.d3,V_sum,-ratio)
    vs.d3$cluster1<-c(1:4)
    # vs.d3$cluster1<-c(1:5)
    
    vs.d3$country="world"
    vs.d3$years=i
    vs.d3$GCM="all"
    vs.d3$BarleyType=btype
    km.data<-rbind(vs.d3,km.data)
    
    bind1<-vs.d3[,c("cluster","cluster1")]
    vs.d2<-merge(vs.d2,bind1,all=T,by="cluster")
    km.alldata<-rbind(vs.d2,km.alldata)
    
    # for (sdate in unique(vs.d2$sdate)) {
    #   wl.sdata<-vs.d2[vs.d2$sdate==sdate,]
    #   
    #   for (gcm in unique(wl.sdata$GCM)) {
    #     wl.gcm<-wl.sdata[wl.sdata$GCM==gcm,]
    #     set.seed(1234)
    #     
    #     all_km<-kmeans(x = wl.gcm[,8:13],4,nstart = 16)
    #     wl.gcm$cluster <- as.factor(all_km$cluster)
    #     vs.d3<- wl.gcm[,c(-1:-5,-7,-14)] %>% group_by(cluster) %>% summarise_all("mean")
    #     
    #     a1 <-nrow(wl.gcm[wl.gcm$cluster == 1,])/nrow(wl.gcm)*100
    #     b1 <-nrow(wl.gcm[wl.gcm$cluster == 2,])/nrow(wl.gcm)*100
    #     c1 <-nrow(wl.gcm[wl.gcm$cluster == 3,])/nrow(wl.gcm)*100
    #     d1 <-nrow(wl.gcm[wl.gcm$cluster == 4,])/nrow(wl.gcm)*100
    #     # e1 <-nrow(wl.gcm[wl.gcm$cluster == 5,])/nrow(wl.gcm)*100
    #     
    #     vs.d3[vs.d3$cluster==1,"ratio"]<-a1
    #     vs.d3[vs.d3$cluster==2,"ratio"]<-b1
    #     vs.d3[vs.d3$cluster==3,"ratio"]<-c1
    #     vs.d3[vs.d3$cluster==4,"ratio"]<-d1
    #     # vs.d3[vs.d3$cluster==5,"ratio"]<-e1
    #     vs.d3[,"between_totss"]<-all_km[["betweenss"]]/all_km[["totss"]]*100
    #     setDT(vs.d3)
    #     vs.d3[,V_sum:=JV1+JV2+FIN+FWR+GF1+GF2]
    #     setorder(x = vs.d3,V_sum,-ratio)
    #     vs.d3$cluster1<-c(1:4)
    #     # vs.d3$cluster1<-c(1:5)
    #     
    #     vs.d3$country=paste0("world_",sdate)
    #     vs.d3$years=i
    #     vs.d3$GCM=gcm
    #     vs.d3$BarleyType=btype
    #     km.data<-rbind(vs.d3,km.data)
    #     
    #   }
    # }
    # 

  }
}
write.csv(x = km.alldata,file = "KMeansData_cl4_all.csv")
write.csv(x = km.data,file = "KMeansData_cl4.csv")
km.data<-read.csv(file = "KMeansData_cl4.csv",header = T,row.names = 1)

km.alldata<-read.csv(file = "KMeansData_cl4_all.csv",header = T,row.names = 1)

km.alldata[km.alldata$cluster1==4 & km.alldata$BarleyType=="winter" & km.alldata$years=="2090","cluster1"]<-5
km.alldata[km.alldata$cluster1==3 & km.alldata$BarleyType=="winter" & km.alldata$years=="2090","cluster1"]<-4
km.alldata[km.alldata$cluster1==5 & km.alldata$BarleyType=="winter" & km.alldata$years=="2090","cluster1"]<-3
km.alldata$cluster1<-as.factor(km.alldata$cluster1)
km.alldata$years<-factor(x = km.alldata$years,levels = c("Baseline","2050","2090"))

names(km.alldata)
km.meandata<-km.alldata[,c(-1,-6)] %>% group_by(cluster1,Site,GCM,years,sdate,BarleyType) %>% summarise_all("mean",na.rm=T)
km.meandata$cluster1<-as.factor(km.meandata$cluster1)
km.meandata$years<-factor(x = km.meandata$years,levels = c("Baseline","2050","2090"))

p<-ggplot(data = km.alldata,aes(x =cluster1 ,y=yield,group=cluster1))+
  # facet_rep_wrap(BarleyType~cluster1,ncol=4,scales='free_y', repeat.tick.labels = 'left')+
  facet_grid(BarleyType~years)+
  # geom_point(aes(color=cluster1),alpha=0.02,size=1,
  #            position = position_jitter(width = .2))+
  geom_boxplot(aes(fill=cluster1),alpha=0.7,outlier.shape = NA)+
  scale_y_continuous(name = "Yield (kg/ha)")+
  scale_fill_manual(values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"),
                    name="Stress")+
  scale_color_manual(values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"))+
  guides(colour="none")+
  coord_cartesian(ylim = c(0,7500))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust = 1,angle = 45),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.spacing=unit(.1,units = "cm")
  )
# p
ggsave(filename = "WL_type_yield03242.pdf",plot = p,height = 7,width = 4)

kmss_summary<-NULL
for (btype in unique(wl.d1$BarleyType)) {
  wl.btype<-wl.d1[wl.d1$BarleyType==btype,]
  
  for (i in unique(wl.btype$years)) {
    
    set.seed(1234)
    vs.d2<-wl.btype[wl.btype$years==i,]
    
    for (nk in c(2:10)) {
      km.tmp<-kmeans(x = vs.d2[,8:13],centers=nk)
      
      km.d1<-data.frame(
        BarleyType=btype,
        years=i,
        N_cls=nk,
        TotalSS=km.tmp$totss,
        BetweenSS=km.tmp$betweenss,
        BofT=km.tmp$betweenss/km.tmp$totss*100
      )
      kmss_summary<-rbind(kmss_summary,km.d1)
    }
  }}

kmss_summary$years<-factor(x = kmss_summary$years,levels = c("Baseline" ,"2090","2050"))

p<-ggplot(data = kmss_summary,aes(x=N_cls,y=BofT))+
  facet_grid(.~BarleyType)+
  geom_line(aes(color=years))+
  xlab("No. of clusters")+
  ylab("Variance account for (%)")+
  scale_color_manual(values = c("#40b8ef","#f39188","#fdeb83"),name="Years")+
  scale_y_continuous(limits = c(20,100),breaks = seq(20,100,20))+
  theme_bw()+
  theme(
    # legend.position = "none",
    # axis.title.x = element_blank(),
    # axis.text.x = element_text(hjust = 1,angle = 45),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    # panel.spacing=unit(.1,units = "cm")
  )
  
ggsave(filename = "Kmeans_Variance_account.pdf",plot = p,width = 7,height = 4)
write.csv(x = kmss_summary,file = "Kmeans_Variance_account.csv",row.names = F)
gcm_ratio<-NULL

for (y in unique(km.alldata$years) ) {
  data1<-km.alldata[km.alldata$years==y,]
  for (g in unique(data1$GCM)) {
    data1.1<-data1[data1$GCM==g,]
    data1.t<-data.frame(
      years=y,
      GCM=g,
      fq=nrow(data1.1[data1.1$cluster1!="1",])/nrow(data1.1)*100
    )
    gcm_ratio<-rbind(gcm_ratio,data1.t)
  }
}

gcm_ratio<- gcm_ratio%>% setDT() %>% setorder(years,fq)

gcm_ratio$GCM<-factor(gcm_ratio$GCM,levels = gcm_ratio[years=="2090",GCM])
gcm_ratio$years<-factor(gcm_ratio$years,levels = c("Baseline" ,"2090","2050"))

p<-ggplot(data = gcm_ratio[gcm_ratio$years %in% c("2090","2050"),],
          aes(x=GCM,y=fq,group=years,color=years))+
  # facet_grid(years~.)+
  geom_line()+
  # xlab("No. of clusters")+
  ylab("Proportion of severe \nwaterlogging stress (%)")+
  scale_color_manual(values = c("#f39188","#fdeb83"),name="Years")+
  # scale_y_continuous(limits = c(20,100),breaks = seq(20,100,20))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust = 1,angle = 45),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    # panel.spacing=unit(.1,units = "cm")
  )
ggsave(filename = "Proportion_of_severe_waterlogging_stress.pdf",plot = p,width = 9,height = 4)
write.csv(x = gcm_ratio,file = "Proportion_of_severe_waterlogging_stress.csv",row.names = F)
gcm_ratio<-read.csv(file = "Proportion_of_severe_waterlogging_stress.csv",header = T)


my_quantile <- function(x, probs) {
  tibble(x = quantile(x, probs), probs = probs)
}
km.alldata1<-km.alldata[,c("years","BarleyType","cluster1","yield")] %>% group_by(years,BarleyType,cluster1) %>% dplyr::summarise(.,y_min=min(yield),y_max=max(yield),y_mean=mean(yield),y_q10=quantile(yield,prob =c(0.1)),y_q25=quantile(yield,prob =c(0.25)),y_q50=quantile(yield,prob =c(0.5)),y_q75=quantile(yield,prob =c(0.75)),y_q90=quantile(yield,prob =c(0.9)))
write.csv(x = km.alldata1,file = "Yield_summarise_by_km.csv")

setDT(km.data)
km.data1<-melt(data = km.data,
     id.vars = c("country","BarleyType","years","GCM","ratio", "V_sum","cluster1"),
     measure =c("JV1", "JV2", "FIN","FWR", "GF1","GF2"))

unique(km.data1$BarleyType)
km.data1$BarleyType<-factor(x = km.data1$BarleyType,levels = c("spring","winter"))
km.data1$country<-factor(x = km.data1$country,levels = c("world","world_S1","world_S3"))
km.data1$variable<-factor(x = km.data1$variable,
                         levels = c("JV1","JV2","FIN","FWR","GF1","GF2"))
km.data1$years<-factor(x = km.data1$years,levels = c("Baseline","2050","2090"))
km.data1$cluster1<-as.factor(km.data1$cluster1)

p<-ggplot(data = km.data1, aes(variable,value,group=cluster1,colour = cluster1))+
  facet_grid(BarleyType+country+GCM~years)+
  geom_line(size = 1,)+
  # geom_area(aes(fill=cluster),alpha=0.2,outline.type = "upper",size=0.2)+
  # coord_cartesian(ylim = c(0,10))+
  scale_x_discrete(name ="Grow stage")+
  scale_y_continuous(name ="Intensity of water stress",
                     expand = c(0.1,0.01))+
  guides(fill="none")+
  theme_bw() +
  scale_color_discrete(name="Frequencies")+
  theme(panel.grid.major = element_blank(),
        # legend.position = c(0.8,0.7),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
  theme(axis.text.x = element_text(size = 12,color = "black"))+
  theme(axis.text.y = element_text(size=12,color="black"))

ggsave(filename = "WLplot_GCMs.pdf",plot = p,height = 200,width = 15,limitsize = FALSE)


km.world<-km.data1[km.data1$country=="world",]
km.p<-NULL
for (btype in unique(km.world$BarleyType)) {
  km.btype<-km.world[km.world$BarleyType==btype,]
  for (year1 in unique(km.btype$years)) {
    km.year<-km.btype[km.btype$years==year1,]
    
    a <- sprintf("stress1 = %1.1f%%",km.year[km.year$cluster1==1,"ratio"][1])
    b <- sprintf("stress2 = %1.1f%%",km.year[km.year$cluster1==2,"ratio"][1])
    c <- sprintf("stress3 = %1.1f%%",km.year[km.year$cluster1==3,"ratio"][1])
    d <- sprintf("stress4 = %1.1f%%",km.year[km.year$cluster1==4,"ratio"][1])
    # e <- sprintf("stress5 = %1.1f%%",km.year[km.year$cluster1==5,"ratio"][1])
    
    km.p[[paste0(btype,"_",year1)]] <- ggplot(data = km.year,aes(variable,value,group=cluster1,colour = cluster1))+
      geom_line(size = 1,)+
      # geom_area(aes(fill=cluster),alpha=0.2,outline.type = "upper",size=0.2)+
      # coord_cartesian(ylim = c(0,10))+
      scale_x_discrete(name ="Grow stage")+
      scale_y_continuous(name ="Intensity of water stress",
                         expand = c(0.1,0.01),limits = c(0,50))+
      guides(fill="none")+
      theme_bw() +
      scale_color_discrete(name="Frequencies",label=c(a, b, c,d
                                                      # ,e
      ))+
      theme(panel.grid.major = element_blank(),
            legend.position = c(0.8,0.7),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),
            legend.background = element_rect(fill = "transparent"),
            legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
      theme(axis.text.x = element_text(size = 12,color = "black"))+
      theme(axis.text.y = element_text(size=12,color="black"))
    
    
    
  }
}

p4<-ggarrange(km.p[["spring_Baseline"]],km.p[["spring_2050"]],km.p[["spring_2090"]],
              km.p[["winter_Baseline"]],km.p[["winter_2050"]],km.p[["winter_2090"]],
              ncol = 3,nrow = 2,
              # widths = rep(x = c(0.95,0.05),8),
              align = "hv"
              # common.legend = T,
              # legend = "right",
              ,labels = LETTERS[1:6]
)

ggsave(filename = "WaterStress_world_all_4.pdf",plot = p4,
       height = 8,width = 15)


km.all1<-km.data1[!km.data1$country=="world" ,c(1:5,7)]
km.all2<-unique(km.all1)
km.all2$years<-factor(x = km.all2$years,levels = c("Baseline","2050","2090"))
km.all2$cluster1<-as.factor(km.all2$cluster1)

p<-ggplot(data = km.all2,aes(x = years,y=ratio,group=years))+
  facet_rep_wrap(BarleyType~cluster1,ncol=4,scales='free_y', repeat.tick.labels = 'left')+
  # facet_grid(BarleyType~cluster1)+
  geom_point(aes(color=cluster1),alpha=0.2,size=1,
             position = position_jitter(width = .2))+
  geom_boxplot(aes(fill=cluster1),alpha=0.7,outlier.shape = NA)+
  scale_y_continuous(name = "Frequency (%)")+
  scale_fill_manual(values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"),
                    name="Stress")+
  scale_color_manual(values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"))+
  guides(colour="none")+
  # coord_cartesian(ylim = c(0,7500))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust = 1,angle = 45),
    axis.text.y = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.spacing=unit(.1,units = "cm")
  )
p
ggsave(filename = "WL_type_freq0322.pdf",plot = p,height = 8,width = 4)











all.aov<-NULL
for (btype in unique(km.all2$BarleyType)) {
  km.bt<-km.all2[km.all2$BarleyType==btype,]
  for (country1 in unique(km.bt$country)) {
    km.country<-km.bt[km.bt$country==country1,]
    for (variable in unique(km.country$cluster1)) {
      km.var<-km.country[km.country$cluster1==variable,]
      
      aov(formula = ratio~years,data = km.var)->fit
      LSD.test(fit,trt = "years",p.adj = "none")->out
      out[["groups"]]->out1
      out1$years<-row.names(out1)
      names(out1)[1]<-"mean"
      sum.var<-km.var[,-4] %>% group_by(country,BarleyType,years,cluster1) %>% summarise_all(.funs = c("mean","sd"))
      sum.var<-merge(out1,sum.var)
      all.aov<-rbind(sum.var,all.aov)
      
    }
  }
}


all.aov$country<-factor(x = all.aov$country,levels = c("world_S1","world_S3"))
all.aov$cluster1<-factor(x = all.aov$cluster1,
                         levels = c(1,2,3,4),labels = paste0("Stress_",c(1:4)))
all.aov$years<-factor(x = all.aov$years,levels = c("Baseline","2050","2090"))
all.aov$BarleyType<-factor(x = all.aov$BarleyType,levels = c("spring","winter"))

aov.p<-ggplot(data = all.aov,aes(x=cluster1,y=mean,group=years))+
  facet_grid(.~BarleyType+country)+
  geom_col(aes(fill=years),alpha=0.9,
           position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), 
                position = position_dodge(width = 0.9),
                width=0.5)+
  geom_text(aes(y=mean+sd+5,label=groups),position = position_dodge(width = 0.9))+
  xlab("")+
  ylab("Frequencies")+
  scale_fill_manual(values = c("#40b8ef","#f39188","#fdeb83"),name="Years")+
  theme_bw()+
  theme(
    legend.position = c(0.92,0.7),
    # axis.title = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(filename = "Stress_Frequencies_ANOVA.pdf",plot = aov.p,width = 15,height = 4)


# Country Kmeans cyl-plot -------------------------------------------------


wl.allcountry<-wl.d1[wl.d1$country!="AU",]
km.countrys<-NULL
for (btype in unique(wl.allcountry$BarleyType)) {
  wl.btype<-wl.allcountry[wl.allcountry$BarleyType==btype,]
  
  for (i in unique(wl.btype$years)) {
    vs.d2<-wl.btype[wl.btype$years==i,]
    
    for (country in unique(vs.d2$country)) {
      wl.country<-vs.d2[vs.d2$country==country,]
      set.seed(1234)
      
      all_km<-kmeans(x = wl.country[,7:12],4,nstart = 16)
      wl.country$cluster <- as.factor(all_km$cluster)
      vs.d3<- wl.country[,c(-1:-6,-13)] %>% group_by(cluster) %>% summarise_all("mean")
      
      a1 <-nrow(wl.country[wl.country$cluster == 1,])/nrow(wl.country)*100
      b1 <-nrow(wl.country[wl.country$cluster == 2,])/nrow(wl.country)*100
      c1 <-nrow(wl.country[wl.country$cluster == 3,])/nrow(wl.country)*100
      d1 <-nrow(wl.country[wl.country$cluster == 4,])/nrow(wl.country)*100
      # e1 <-nrow(wl.country[wl.country$cluster == 5,])/nrow(wl.country)*100
      
      vs.d3[vs.d3$cluster==1,"ratio"]<-a1
      vs.d3[vs.d3$cluster==2,"ratio"]<-b1
      vs.d3[vs.d3$cluster==3,"ratio"]<-c1
      vs.d3[vs.d3$cluster==4,"ratio"]<-d1
      setDT(vs.d3)
      vs.d3[,V_sum:=JV1+JV2+FIN+FWR+GF1+GF2]
      setorder(x = vs.d3,V_sum,-ratio)
      vs.d3$cluster1<-c(1:4)
      
      vs.d3$country=country
      vs.d3$years=i
      vs.d3$GCM="all"
      vs.d3$BarleyType=btype
      km.countrys<-rbind(vs.d3,km.countrys)
    }
    
}}

write.csv(x = km.countrys,file = "KMeansCountrys.csv")

km.countrys<-read.csv(file = "KMeansCountrys.csv",header = T,row.names = 1)
setDT(km.countrys)
km.countrys1<-melt(data = km.countrys[,-1],
               id.vars = c("country","BarleyType","years","cluster1","ratio", "V_sum"),
               measure =c("JV1", "JV2", "FIN","FWR", "GF1","GF2"))

unique(km.countrys1$BarleyType)
km.countrys1$BarleyType<-factor(x = km.countrys1$BarleyType,levels = c("spring","winter"))
# km.countrys1$country<-factor(x = km.countrys1$country,levels = c("world","world_S1","world_S3"))
km.countrys1$variable<-factor(x = km.countrys1$variable,
                          levels = c("JV1","JV2","FIN","FWR","GF1","GF2"))
km.countrys1$years<-factor(x = km.countrys1$years,levels = c("Baseline","2050","2090"))
km.countrys1$cluster1<-as.factor(km.countrys1$cluster1)

p<-ggplot(data = km.countrys1, aes(variable,value,group=cluster1,colour = cluster1))+
  facet_grid(BarleyType+country~years)+
  geom_line(size = 1,)+
  # geom_area(aes(fill=cluster),alpha=0.2,outline.type = "upper",size=0.2)+
  # coord_cartesian(ylim = c(0,10))+
  scale_x_discrete(name ="Grow stage")+
  scale_y_continuous(name ="Intensity of water stress",
                     expand = c(0.1,0.01))+
  guides(fill="none")+
  theme_bw() +
  scale_color_discrete(name="Stress Type")+
  theme(panel.grid.major = element_blank(),
        # legend.position = c(0.8,0.7),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
  theme(axis.text.x = element_text(size = 12,color = "black"))+
  theme(axis.text.y = element_text(size=12,color="black"))

ggsave(filename = "WL_countrys_line.pdf",plot = p,height = 50,width = 15,limitsize = FALSE)

km.countrys<-read.csv(file = "KMeansCountrys(1).csv",header = T,row.names = 1)
names(km.countrys)
km.countrys1<-km.countrys[,-1] %>% group_by(cluster1,country,years,GCM,BarleyType) %>% summarise_all("sum",na.rm=T)
km.countrys1$cluster1<-as.factor(km.countrys1$cluster1)
if (!dir.exists("./circle")) {
  dir.create("circle")
}

i="AR"
for (i in unique(km.countrys1$country)) {
  data.country<-km.countrys1[km.countrys1$country==i,]
  for (btype in unique(data.country$BarleyType)) {
    data1<-data.country[data.country$BarleyType==btype,c(1,3,12)]
    data1$years<- factor(data1$years,levels =c("Baseline","2050","2090"))
    data1<- ddply(data1,"years", transform, label_y = cumsum(ratio) - 0.5*ratio)
    
    p1<-ggplot(data = data1,aes(x = years,y=ratio,group=cluster1))+
      geom_col(aes(fill=cluster1),position = "stack",size=.5,width = .7)+
      geom_label(aes(label=sprintf("%1.0f",ratio),y=100-label_y,fill=cluster1),size=5)+
      # labs(title=i)+
      # geom_text(aes(label=c("baseline\n\n\n"),y=100,x=c("baseline")))+
      # geom_text(aes(label=c("2059\n\n\n"),y=100,x=c("2059")))+
      # geom_text(aes(label=c("2099\n\n\n"),y=100,x=c("2099")))+
      geom_text(aes(label=paste0(i,"\n\n\n"),y=100,x=c("2090")),size=5)+
      coord_polar(theta="y")+
      scale_x_discrete(expand = c(0.2,0.5))+
      scale_fill_manual(values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"))+
      theme_bw()+
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none"
      )
    # p1
    ggsave(filename = paste0(wd,"/circle/",i,"_",btype,".pdf"),plot = p1,width = 3,height=3)
    
  }
}


km.countrys1<-melt(data = km.countrys1,
                   id.vars = c("country","BarleyType","years","cluster1","ratio", "V_sum"),
                   measure =c("JV1", "JV2", "FIN","FWR", "GF1","GF2"))

unique(km.countrys1$BarleyType)
km.countrys1$BarleyType<-factor(x = km.countrys1$BarleyType,levels = c("spring","winter"))
# km.countrys1$country<-factor(x = km.countrys1$country,levels = c("world","world_S1","world_S3"))
km.countrys1$variable<-factor(x = km.countrys1$variable,
                              levels = c("JV1","JV2","FIN","FWR","GF1","GF2"))
km.countrys1$years<-factor(x = km.countrys1$years,levels = c("Baseline","2050","2090"))
km.countrys1$cluster1<-as.factor(km.countrys1$cluster1)

km.countrys2<-km.countrys1[km.countrys1$variable=="GF1",c(1:5,7)] %>% unique()
km.countrys2[km.countrys2$cluster1==1,"lab_y"]<-40
km.countrys2[km.countrys2$cluster1==2,"lab_y"]<-30
km.countrys2[km.countrys2$cluster1==3,"lab_y"]<-20
km.countrys2[km.countrys2$cluster1==4,"lab_y"]<-10

p<-ggplot(data = km.countrys1[km.countrys1$BarleyType=="spring",], 
          aes(variable,value,group=cluster1,colour = cluster1))+
  facet_grid(country~years)+
  geom_line(size = 1,)+
  geom_text(data = km.countrys2[km.countrys2$BarleyType=="spring",],
            aes(x=variable,y=lab_y,group=cluster1,label=paste0("Stress ",cluster1," = ",ratio,"%")),color="black")+
  # geom_area(aes(fill=cluster),alpha=0.2,outline.type = "upper",size=0.2)+
  # coord_cartesian(ylim = c(0,10))+
  scale_x_discrete(name ="Grow stage")+
  scale_y_continuous(name ="Intensity of water stress",
                     expand = c(0.1,0.01))+
  # guides(fill="none")+
  theme_bw() +
  scale_color_manual(name="Stress Type",
                     values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"))+
  theme(panel.grid.major = element_blank(),
        # legend.position = c(0.8,0.7),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
  theme(axis.text.x = element_text(size = 12,color = "black"))+
  theme(axis.text.y = element_text(size=12,color="black"))
ggsave(filename ="spring_countrys_line.pdf",plot = p,width = 10,height=20)


p<-ggplot(data = km.countrys1[km.countrys1$BarleyType=="winter",], 
          aes(variable,value,group=cluster1,colour = cluster1))+
  facet_grid(country~years)+
  geom_line(size = 1,)+
  geom_text(data = km.countrys2[km.countrys2$BarleyType=="winter",],
            aes(x=variable,y=lab_y,group=cluster1,label=paste0("Stress ",cluster1," = ",ratio,"%")),color="black")+
  # geom_area(aes(fill=cluster),alpha=0.2,outline.type = "upper",size=0.2)+
  # coord_cartesian(ylim = c(0,10))+
  scale_x_discrete(name ="Grow stage")+
  scale_y_continuous(name ="Intensity of water stress",
                     expand = c(0.1,0.01))+
  # guides(fill="none")+
  theme_bw() +
  scale_color_manual(name="Stress Type",
                     values = c("#aeccd5","#efbc60","#ef6c60","#00a0e9"))+
  theme(panel.grid.major = element_blank(),
        # legend.position = c(0.8,0.7),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)))+
  theme(axis.text.x = element_text(size = 12,color = "black"))+
  theme(axis.text.y = element_text(size=12,color="black"))
ggsave(filename ="winter_countrys_line.pdf",plot = p,width = 10,height=20)



# Yield change map --------------------------------------------------------

yield2<-yield1[yield1$WS=="TS",c(-3,-8)]
setDT(yield2)

yield2.gcm<-yield2[,-6] %>% group_by(Site,GCM,years,sdate,BarleyType,Type) %>% summarise_all("mean",na.rm=T)

yield2.sd<-yield2.gcm[,-2] %>% group_by(Site,years,sdate,BarleyType,Type) %>% summarise_all("mean",na.rm=T)
setDT(yield2.sd)
yield.bastesd<-yield2.sd[,.SD[which.max(yield)],by=.(Site,years,BarleyType)]
yield3<-dcast(data = yield2.sd,Site+years+BarleyType~sdate+Type,
              value.var="yield")
yield3[,`:=`(diff_vs=abs(S1_VS-S3_VS),diff_all=max(c(abs(S1_VS-S3_VS),abs(S1_VS-S1_VT),abs(S3_VS-S3_VT),abs(S1_VS-S3_VT),abs(S1_VT-S3_VS)),na.rm=T)),by=.(Site,years,BarleyType)]

yield.baste1<-merge(yield.bastesd[,c(-5,-6)],yield3[,c(-4:-7)],by=c("Site","years","BarleyType"))
write.csv(x = yield.baste1,file = "Yield_baste.csv")
yield.baste1<-read.csv(file = "Yield_baste.csv",header = T,row.names = 1)
yield.baste2<-yield.baste1[yield.baste1$years %in% c("2050","2090"),]
names(loca1)[1]<-"Site"
yield.baste2[yield.baste2$Site=="UkBu","Site"]<-"UKBu"
yield.baste3<-merge(x = yield.baste2,y = loca[,c(3:5)],by.x="Site",by.y="Abbre",all=T)
yield.baste3<-merge(x = yield.baste3,y = loca1,by=c("Site","BarleyType"),all=T)
yield.baste3<-yield.baste3[!is.na(yield.baste3$BarleyType) & !is.na(yield.baste3$years),]
yield.baste3$BarleyType<-factor(x = yield.baste3$BarleyType,levels = c("spring","winter"))
yield.baste3$years<-factor(x = yield.baste3$years,levels = c("2050","2090"))
yield.baste3$sdate<-factor(x = yield.baste3$sdate,levels = c("S1","S3"))

p<-ggplot(data=yield.baste3)+
  facet_grid(BarleyType~years)+
  borders("world", size= 0.05,colour="grey70")+
  geom_raster(data = barley_a1,aes(x=x,y=y),fill="grey40",alpha=0.7)+
  geom_segment(data=yield.baste3,
               aes(x=Long, y=Lat,xend=X1,yend=Y1),
               color="black",size=0.2)+
  geom_point(data=yield.baste3,aes(x=Long, y=Lat,fill=sdate),
             pch=25,color="black",stroke =0.1)+
  geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_all,fill=sdate),
             pch=21,alpha=0.6,color="black",stroke =0.02)+
  geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_vs,fill=sdate),
             pch=21,alpha=0.9,color="black",stroke =0.02)+
  scale_x_continuous(breaks = seq(-180,180,30),name = "")+
  scale_y_continuous(breaks = seq(-40,80,20),name = "")+
  scale_color_manual(values =c("#f7b34d","#548235"))+
  scale_fill_manual(values = c("#f7b34d","#548235"))+
  # scale_fill_gradient(low = "darkgreen",high = "darkgreen")+
  # guides(fill = "none")+
  coord_quickmap(
    xlim = c(-180,180),ylim = c(-60,90),
    clip = "on",
    expand = F
  )+
  theme_bw()+
  theme(
    # legend.position = c(0.8,0.8),
    legend.title = element_blank(),
    panel.grid = element_blank()
    # legend.spacing = 
  )

ggsave(filename = "Yield_change_by_S&V.pdf",plot = p,width = 9,height = 4)
write.csv(x = yield.baste3,file = "Yield_change_by_S&V.csv")


# met box plot ------------------------------------------------------------


met1<-com.res1[com.res1$zadok_stage==100,c(1:6,12,44,38:41)]
names(met1)[9:12]<-c("rain","radn","Tmax","Tmin")

met2<-met1[,-7] %>% group_by(Site,GCM,WS,years,sdate,Type,BarleyType) %>% summarise_all("mean",na.rm=T)
# met3<-met2[,-2] %>% group_by(Site,WS,years,sdate,Type,BarleyType) %>% summarise_all("mean",na.rm=T)

met3<-met2[met2$WS=="TS" & met2$Type=="VS",]
write.csv(x = met3,file = "met_data.csv")

met3<-read.csv(file = "met_data.csv",header = T,row.names = 1)
met3$years<-factor(x = met3$years,levels = c("Baseline","2050","2090"),labels = c("1980","2050","2090"))
met3$BarleyType<-factor(met3$BarleyType,levels = c("spring", "winter"))
met3$group<-paste0(met3$Site,"_",met3$years)

p<-ggplot(data = met3,aes(x = Site,y=rain,group=group))+
  facet_grid(sdate~BarleyType,scales = "free_x",space="free_x")+
  geom_point(aes(color=years),alpha=0.2,
             position = position_dodge(width = 1))+
  geom_boxplot(aes(fill=years),position = position_dodge(width = 1),
               alpha=0.7,outlier.shape = NA)+
  scale_y_continuous(name = "Rainfull in crop (mm)")+
  scale_fill_manual(values = c("#efbc60","#ef6c60","#00a0e9"),
                    name="Years")+
  scale_color_manual(values = c("#efbc60","#ef6c60","#00a0e9"))+
  guides(colour="none")+
  # coord_cartesian(ylim = c(0,7500))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18,color = "black"),
    axis.text.x = element_text(size = 14,angle = 45,hjust = 1,color = "black"),
    axis.text.y = element_text(size = 14,color = "black"),
    strip.text = element_text(size = 14,color = "black"),
    panel.grid = element_blank()
  )


ggsave(filename = "Rainfall_incrop.pdf",plot = p,width = 15,height = 6)


p<-ggplot(data = met3,aes(x = Site,y=radn,group=group))+
  facet_grid(sdate~BarleyType,scales = "free_x",space="free_x")+
  geom_point(aes(color=years),alpha=0.2,
             position = position_dodge(width = 1))+
  geom_boxplot(aes(fill=years),position = position_dodge(width = 1),
               alpha=0.7,outlier.shape = NA)+
  scale_y_continuous(name = "Radn in crop (MJ/m2)")+
  scale_fill_manual(values = c("#efbc60","#ef6c60","#00a0e9"),
                    name="Years")+
  scale_color_manual(values = c("#efbc60","#ef6c60","#00a0e9"))+
  guides(colour="none")+
  # coord_cartesian(ylim = c(0,7500))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18,color = "black"),
    axis.text.x = element_text(size = 14,angle = 45,hjust = 1,color = "black"),
    axis.text.y = element_text(size = 14,color = "black"),
    strip.text = element_text(size = 14,color = "black"),
    panel.grid = element_blank()
  )


ggsave(filename = "Radn_incrop.pdf",plot = p,width = 15,height = 6)

p<-ggplot(data = met3,aes(x = Site,y=Tmax,group=group))+
  facet_grid(sdate~BarleyType,scales = "free_x",space="free_x")+
  geom_point(aes(color=years),alpha=0.2,
             position = position_dodge(width = 1))+
  geom_boxplot(aes(fill=years),position = position_dodge(width = 1),
               alpha=0.7,outlier.shape = NA)+
  scale_y_continuous(name = "Average Tmax in crop (°C)")+
  scale_fill_manual(values = c("#efbc60","#ef6c60","#00a0e9"),
                    name="Years")+
  scale_color_manual(values = c("#efbc60","#ef6c60","#00a0e9"))+
  guides(colour="none")+
  # coord_cartesian(ylim = c(0,7500))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18,color = "black"),
    axis.text.x = element_text(size = 14,angle = 45,hjust = 1,color = "black"),
    axis.text.y = element_text(size = 14,color = "black"),
    strip.text = element_text(size = 14,color = "black"),
    panel.grid = element_blank()
  )


ggsave(filename = "Tmax_incrop.pdf",plot = p,width = 15,height = 6)

p<-ggplot(data = met3,aes(x = Site,y=Tmin,group=group))+
  facet_grid(sdate~BarleyType,scales = "free_x",space="free_x")+
  geom_point(aes(color=years),alpha=0.2,
             position = position_dodge(width = 1))+
  geom_boxplot(aes(fill=years),position = position_dodge(width = 1),
               alpha=0.7,outlier.shape = NA)+
  scale_y_continuous(name = "Average Tmin in crop (°C)")+
  scale_fill_manual(values = c("#efbc60","#ef6c60","#00a0e9"),
                    name="Years")+
  scale_color_manual(values = c("#efbc60","#ef6c60","#00a0e9"))+
  guides(colour="none")+
  # coord_cartesian(ylim = c(0,7500))+
  theme_bw()+
  theme(
    # legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 18,color = "black"),
    axis.text.x = element_text(size = 14,angle = 45,hjust = 1,color = "black"),
    axis.text.y = element_text(size = 14,color = "black"),
    strip.text = element_text(size = 14,color = "black"),
    panel.grid = element_blank()
  )

ggsave(filename = "Tmin_incrop.pdf",plot = p,width = 15,height = 6)



met_q90<-met1[met1$WS=="TS",-3] %>% group_by(Site,years,sdate,BarleyType) %>%summarise_at(.vars = vars(rain),.funs = function(x){y=quantile(x,probs = 0.9,na.rm = T);return(y)})

met_rain<-met1[met1$WS=="TS" & met1$Type=="VS",c(-3,-6)] %>% group_by(Site,GCM,years,BarleyType,SowingYear) %>%summarise_at(.vars = vars(rain),.funs = mean,na.rm=T)

rain90<-met_rain %>% group_by(Site,years,BarleyType) %>% filter(rain>=quantile(rain,probs = 0.9,na.rm = T)) 
rain90_1<-rain90[,-6]
setDT(rain90_1)
rain_select_yield<-yield2[rain90_1,on=.(Site=Site,GCM=GCM,years=years,SowingYear=SowingYear,BarleyType=BarleyType),roll=T]

rain_select_yield1<-dcast(rain_select_yield,Site+GCM+years+SowingYear+BarleyType~sdate+Type,value.var="yield")

rain_select_yield1[,`:=`(diff_vs=abs(S1_VS-S3_VS),diff_all=max(c(abs(S1_VS-S3_VS),abs(S1_VS-S1_VT),abs(S3_VS-S3_VT),abs(S1_VS-S3_VT),abs(S1_VT-S3_VS)),na.rm=T)),by=.(Site,GCM,years,BarleyType,SowingYear)]
rain_select_yield2<-rain_select_yield1[,c(-2,-4)] %>% group_by(Site,years,BarleyType) %>%summarise_all("mean",na.rm=T)

yield.baste1<-merge(yield.bastesd[,c(-5,-6)],rain_select_yield2[,c(-4:-7)],by=c("Site","years","BarleyType"))
# write.csv(x = yield.baste1,file = "Yield_baste.csv")
# yield.baste1<-read.csv(file = "Yield_baste.csv",header = T,row.names = 1)
yield.baste2<-yield.baste1[yield.baste1$years %in% c("2050","2090"),]
names(loca1)[1]<-"Site"
yield.baste2[yield.baste2$Site=="UkBu","Site"]<-"UKBu"
yield.baste3<-merge(x = yield.baste2,y = loca[,c(3:5)],by.x="Site",by.y="Abbre",all=T)
yield.baste3<-merge(x = yield.baste3,y = loca1,by=c("Site","BarleyType"),all=T)
yield.baste3<-yield.baste3[!is.na(yield.baste3$BarleyType) & !is.na(yield.baste3$years),]
yield.baste3$BarleyType<-factor(x = yield.baste3$BarleyType,levels = c("spring","winter"))
yield.baste3$years<-factor(x = yield.baste3$years,levels = c("2050","2090"))
yield.baste3$sdate<-factor(x = yield.baste3$sdate,levels = c("S1","S3"))

p<-ggplot(data=yield.baste3)+
  facet_grid(BarleyType~years)+
  borders("world", size= 0.05,colour="grey70")+
  geom_raster(data = barley_a1,aes(x=x,y=y),fill="#348434",alpha=0.7)+
  geom_segment(data=yield.baste3,
               aes(x=Long, y=Lat,xend=X1,yend=Y1),
               color="black",size=0.2)+
  geom_point(data=yield.baste3,aes(x=Long, y=Lat,fill=sdate),
             pch=25,color="black",stroke =0.1)+
  geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_all,fill=sdate),
             pch=21,alpha=0.6,color="black",stroke =0.02)+
  geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_vs,fill=sdate),
             pch=21,alpha=0.9,color="black",stroke =0.02)+
  scale_x_continuous(breaks = seq(-180,180,30),name = "")+
  scale_y_continuous(breaks = seq(-40,80,20),name = "")+
  scale_color_manual(values =c("#f7b34d","#548235"))+
  scale_fill_manual(values = c("#f7b34d","#548235"))+
  # scale_fill_gradient(low = "darkgreen",high = "darkgreen")+
  # guides(fill = "none")+
  coord_quickmap(
    xlim = c(-180,180),ylim = c(-60,90),
    clip = "on",
    expand = F
  )+
  theme_bw()+
  theme(
    # legend.position = c(0.8,0.8),
    legend.title = element_blank(),
    panel.grid = element_blank()
    # legend.spacing = 
  )

ggsave(filename = "Yield_change_by_S&V0323.pdf",plot = p,width = 9,height = 4)


rain_select_yield3<-dcast(rain_select_yield,Site+GCM+years+SowingYear+BarleyType+sdate~Type,value.var="yield")

rain_select_yield3[,benefit:=(VT-VS)]
  rain_select_yield4<-rain_select_yield3[,c(-2,-4)] %>% group_by(Site,years,BarleyType,sdate) %>%summarise_all(.funs = list(mean= function(x) {mean(x,na.rm = T)},se=function(x){sd(x,na.rm = T)/sum(!is.na(x))}))
# rain_select_yield4$benefit_cv<-rain_select_yield4$benefit_sd/rain_select_yield4$benefit_mean*100
rain_select_yield4$class<-cut(x = rain_select_yield4$benefit_mean,breaks=c(-200,0,500,1000,1500,2000,3000),
    labels=c("-200-0","0-500","500-1000","1000-1500","1500-2000",">2000"),
    right=F)
rain_select_yield4$class_se<-cut(x = rain_select_yield4$benefit_se,breaks=c(0,5,10,15,20,40),
                              labels=c("0-5","5-10","10-15","15-20",">20"),
                              right=F)

names(loca1)[1]<-"Site"
rain_select_yield4[rain_select_yield4$Site=="UkBu","Site"]<-"UKBu"
rain_select_yield4<-merge(x = rain_select_yield4,y = loca[,c(3:5)],by.x="Site",by.y="Abbre",all=T)
rain_select_yield4<-merge(x = rain_select_yield4,y = loca1,by=c("Site","BarleyType"),all=T)

rain_select_yield5<-rain_select_yield4[rain_select_yield4$years %in% c("2050"),]

rain_select_yield5<-rain_select_yield5[!is.na(rain_select_yield5$BarleyType) & !is.na(rain_select_yield5$years),]
rain_select_yield5$BarleyType<-factor(x = rain_select_yield5$BarleyType,levels = c("spring","winter"))
# rain_select_yield5$years<-factor(x = rain_select_yield5$years,levels = c("2050","2090"))
rain_select_yield5$sdate<-factor(x = rain_select_yield5$sdate,levels = c("S1","S3"))

p<-ggplot(data=rain_select_yield5)+
  facet_grid(BarleyType~sdate)+
  borders("world", size= 0.05,colour="grey70")+
  geom_raster(data = barley_a1,aes(x=x,y=y),fill="#348434",alpha=0.7)+
  geom_segment(data=rain_select_yield5,
               aes(x=Long, y=Lat,xend=X1,yend=Y1),
               color="black",size=0.2)+
  geom_point(data=rain_select_yield5,aes(x=Long, y=Lat),
             pch=21,size=0.5,color="black",fill="grey20")+
  geom_point(data=rain_select_yield5,aes(x=X1, y=Y1,fill=class,size=benefit_cv),
             pch=21,alpha=0.9,color="black",stroke =0.02)+
  # geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_vs,fill=sdate),
  #            pch=21,alpha=0.9,color="black",stroke =0.02)+
  scale_x_continuous(breaks = seq(-180,180,30),name = "")+
  scale_y_continuous(breaks = seq(-40,80,20),name = "")+
  # scale_color_manual(values =c("#f7b34d","#548235"))+
  scale_fill_manual(values = c("#3288bd","#ffffbf","#fee08b","#fdae61","#f46d43","#d53e4f"))+
  # scale_fill_gradient(low = "darkgreen",high = "darkgreen")+
  # guides(fill = "none")+
  coord_quickmap(
    xlim = c(-180,180),ylim = c(-60,90),
    clip = "on",
    expand = F
  )+
  theme_bw()+
  theme(
    # legend.position = c(0.8,0.8),
    legend.title = element_blank(),
    panel.grid = element_blank()
    # legend.spacing = 
  )

ggsave(filename = "Yield_benefit_0323.pdf",plot = p,width = 9,height = 4)



rain_select_yield5<-rain_select_yield4[rain_select_yield4$years %in% c("2050"),]

rain_select_yield5<-rain_select_yield5[!is.na(rain_select_yield5$BarleyType) & !is.na(rain_select_yield5$years),]
rain_select_yield5$BarleyType<-factor(x = rain_select_yield5$BarleyType,levels = c("spring","winter"))
# rain_select_yield5$years<-factor(x = rain_select_yield5$years,levels = c("2050","2090"))
rain_select_yield5$sdate<-factor(x = rain_select_yield5$sdate,levels = c("S1","S3"))

p<-ggplot(data=rain_select_yield5)+
  facet_grid(BarleyType~sdate)+
  borders("world", size= 0.05,colour="grey70")+
  geom_raster(data = barley_a1,aes(x=x,y=y),fill="#348434",alpha=0.7)+
  geom_segment(data=rain_select_yield5,
               aes(x=Long, y=Lat,xend=X1,yend=Y1),
               color="black",size=0.2)+
  geom_point(data=rain_select_yield5,aes(x=Long, y=Lat),
             pch=21,size=0.5,color="black",fill="grey20")+
  geom_point(data=rain_select_yield5,aes(x=X1, y=Y1,fill=class,size=class_se),
             pch=21,alpha=0.9,color="black",stroke =0.02)+
  # geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_vs,fill=sdate),
  #            pch=21,alpha=0.9,color="black",stroke =0.02)+
  scale_x_continuous(breaks = seq(-180,180,30),name = "")+
  scale_y_continuous(breaks = seq(-40,80,20),name = "")+
  # scale_color_manual(values =c("#f7b34d","#548235"))+
  scale_fill_manual(values = c("#3288bd","#ffffbf","#fee08b","#fdae61","#f46d43","#d53e4f"),name = "Yeild benefit")+
  scale_size_manual(values = seq(2,5,0.7),name = "SEM")+
  # scale_fill_gradient(low = "darkgreen",high = "darkgreen")+
  # guides(fill = "none")+
  coord_quickmap(
    xlim = c(-180,180),ylim = c(-60,90),
    clip = "on",
    expand = F
  )+
  theme_bw()+
  theme(
    # legend.position = c(0.8,0.8),
    # legend.title = element_blank(),
    panel.grid = element_blank()
    # legend.spacing = 
  )

ggsave(filename = "Yield_benefit_SEM_50_0420.pdf",plot = p,width = 9,height = 4)



p<-ggplot(data=rain_select_yield5)+
  facet_grid(BarleyType~sdate)+
  borders("world", size= 0.05,colour="grey70")+
  geom_raster(data = barley_a1,aes(x=x,y=y),fill="#348434",alpha=0.7)+
  geom_segment(data=rain_select_yield5,
               aes(x=Long, y=Lat,xend=X1,yend=Y1),
               color="black",size=0.2)+
  geom_point(data=rain_select_yield5,aes(x=Long, y=Lat),
             pch=21,size=0.5,color="black",fill="grey20")+
  geom_point(data=rain_select_yield5,aes(x=X1, y=Y1,fill=class,size=benefit_se),
             pch=21,alpha=0.9,color="black",stroke =0.02)+
  # geom_point(data=yield.baste3,aes(x=X1, y=Y1,size=diff_vs,fill=sdate),
  #            pch=21,alpha=0.9,color="black",stroke =0.02)+
  scale_x_continuous(breaks = seq(-180,180,30),name = "")+
  scale_y_continuous(breaks = seq(-40,80,20),name = "")+
  # scale_color_manual(values =c("#f7b34d","#548235"))+
  scale_fill_manual(values = c("#3288bd","#ffffbf","#fee08b","#fdae61","#f46d43","#d53e4f"),name = "Yeild benefit")+
  scale_size(trans = 'reverse',name = "SEM")+
  # scale_fill_gradient(low = "darkgreen",high = "darkgreen")+
  # guides(fill = "none")+
  coord_quickmap(
    xlim = c(-180,180),ylim = c(-60,90),
    clip = "on",
    expand = F
  )+
  theme_bw()+
  theme(
    # legend.position = c(0.8,0.8),
    # legend.title = element_blank(),
    panel.grid = element_blank()
    # legend.spacing = 
  )

ggsave(filename = "Yield_benefit80_04181.pdf",plot = p,width = 9,height = 4)

write.csv(x = rain_select_yield4,file = "Yield_benefit_0417.csv")


rain_select_yield3<-dcast(rain_select_yield,Site+GCM+years+SowingYear+BarleyType+sdate~Type,value.var="yield")

rain_select_yield3[,benefit:=(VT-VS)]
rain_select_yield4<-rain_select_yield3[,c(-2,-4)] %>% group_by(Site,years,BarleyType,sdate) %>%summarise_all(.funs = list(mean= function(x) {mean(x,na.rm = T)},se=function(x){sd(x,na.rm = T)}))
# rain_select_yield4$benefit_cv<-rain_select_yield4$benefit_sd/rain_select_yield4$benefit_mean*100
rain_select_yield4$class<-cut(x = rain_select_yield4$benefit_mean,breaks=c(-200,0,500,1000,1500,2000,3000),
                              labels=c("-200-0","0-500","500-1000","1000-1500","1500-2000",">2000"),
                              right=F)
rain_select_yield4$class_se<-cut(x = rain_select_yield4$benefit_se,breaks=c(0,200,400,600,800,1000,2000),
                                 labels=c("0-200","200-400","400-600","600-800","800-1000",">1000"),
                                 right=F)

names(loca1)[1]<-"Site"
rain_select_yield4[rain_select_yield4$Site=="UkBu","Site"]<-"UKBu"
rain_select_yield4<-merge(x = rain_select_yield4,y = loca[,c(3:5)],by.x="Site",by.y="Abbre",all=T)
rain_select_yield4<-merge(x = rain_select_yield4,y = loca1,by=c("Site","BarleyType"),all=T)

rain_select_yield5<-rain_select_yield4[rain_select_yield4$years %in% c("2050"),]

rain_select_yield5<-rain_select_yield5[!is.na(rain_select_yield5$BarleyType) & !is.na(rain_select_yield5$years),]
rain_select_yield5$BarleyType<-factor(x = rain_select_yield5$BarleyType,levels = c("spring","winter"))
# rain_select_yield5$years<-factor(x = rain_select_yield5$years,levels = c("2050","2090"))
rain_select_yield5$sdate<-factor(x = rain_select_yield5$sdate,levels = c("S1","S3"))


# combin baseline result --------------------------------------------------

library(stringr)

all.files<-list.files(path = ".",
                      pattern = "*_BS_Baseline_S[13]_VS.csv$",
                      full.names = T,include.dirs = T,recursive = T)

all.data<-NULL
for (file in all.files) {
  
  name1<-read.csv(file = file,header = T,sep = ",",skip = 3,nrows = 1)
  data1<-read.csv(file = file,header = F,sep = ",",skip = 5,na.strings = "?")
  names(data1)<-names(name1)
  data1<-data1[,c("HarvestingYear","HarvestingDay","zadok_stage","stage","StageName","SowingYear","sowing_date","SowingDay","SowingDate","SowingVar","emergence_das","emergence_date","end_of_juvenile_das","end_of_juvenile_date","floral_initiation_das","floral_initiation_date","flowering_das","flowering_date","maturity_das","maturity_date","germinationTTTarget","end_of_juvenileTTTarget","floral_initiationTTTarget","floweringTTTarget","maturityTTTarget","harvest_ripeTTTarget","TTAftersowing","yield","biomass","grain_no","grain_wt","barley_incrop_rain","barley_incrop_radn","barley_incrop_Tmax","barley_incrop_Tmin","watertable","TheRun","BarleyType","NStressZ3","NodayZ3","FertSup","MnTP_Pto_JV1","MnTP_Pto_JV2","MnTP_Pto_FIN","MnTP_Pto_FWR","MnTP_Pto_GF1","MnTP_Pto_GF2","MnTP_Co2_JV1","MnTP_Co2_JV2","MnTP_Co2_FIN","MnTP_Co2_FWR","MnTP_Co2_GF1","MnTP_Co2_GF2","MnOX_Pto_JV1","MnOX_Pto_JV2","MnOX_Pto_FIN","MnOX_Pto_FWR","MnOX_Pto_GF1","MnOX_Pto_GF2","NoD_GS_n_stress_expan","NoD_GS_n_stress_grain","NoD_GS_n_stress_pheno","NoD_GS_n_stress_photo","NoD_TP_Pto_JV1","NoD_TP_Pto_JV2","NoD_TP_Pto_FIN","NoD_TP_Pto_FWR","NoD_TP_Pto_GF1","NoD_TP_Pto_GF2","NoD_TP_Co2_JV1","NoD_TP_Co2_JV2","NoD_TP_Co2_FIN","NoD_TP_Co2_FWR","NoD_TP_Co2_GF1","NoD_TP_Co2_GF2","NoD_OX_Pto_JV1","NoD_OX_Pto_JV2","NoD_OX_Pto_FIN","NoD_OX_Pto_FWR","NoD_OX_Pto_GF1","NoD_OX_Pto_GF2","Days_JV1","Days_JV2","Days_FIN","Days_FWR","Days_GF1","Days_GF2","Nodays_GS","MnGS_n_stress_expan","MnGS_n_stress_grain","MnGS_n_stress_pheno","MnGS_n_stress_photo","SWSowing","CUM_outflow_lat","CUM_runoff","CUM_es","CUM_drain","SWHarvesting","GSR","EPA","CO2","TAV","AMP","IrrAMT")]
  
  data1$Site<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,5]
  data1$GCM<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,7]
  data1$WS<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,8]
  data1$years<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,9]
  data1$sdate<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,10]
  data1$Type<-str_split_fixed(string = file,pattern = "[_/.]",n = 12)[1,11]
  data1<-data1[,c(105:110,1:104)]
  all.data<-rbind(data1,all.data)
  
}
write.csv(x = all.data,file = "combin_baseline_result.csv",row.names = F)

baseline1<-all.data[all.data$zadok_stage==100,]
write.csv(x = baseline1,file = "combin_baseline_result1.csv",row.names = F)
















