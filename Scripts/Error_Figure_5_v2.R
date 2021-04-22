library(rhdf5)
library(zoo)
library(ggplot2)
library(hydroTSM)
library(reshape2)
library(hydroGOF)
library(ggpubr)
library(ggridges)


# -----------------------------------------------Error paper ----------- Figure 5

#---------------- list of dams
# 1- Observed 2- GW correcred 3- No GW correction 
col_E<-c("gray40", "darkviolet", "springgreen4")
col_E<-c("grey12", "darkcyan", "darkred", "darkorange")

plot_list_dam = list()
plot_list_dam_2 = list()
plot_list_dam_22 = list()

plot_list_dam_3 = list()
plot_list_dam.B=list()

param_list<-c('kerndelta', 'wheeler', 'westkern', 'belridge', 'berrenda', 'semitropic', 'rosedale', 'buenavista', 'cawelo', 'henrymiller', 'losthills', 
              'saucelito', 'delano', 'lowertule', 'kerntulare', 'lindsay', 'exeter', 'lindmore', 'porterville', 'teapot', 'terra', 'sosanjoaquin', 
              'shaffer', 'northkern', 'dudleyridge')

Project_list<-c("SWP", "SWP", "SWP", "SWP", "SWP", "SWP", "SWP", "SWP", "SWP", "SWP", "SWP", "CVP", 
               "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "CVP", "SWP")

for(i_dist in 1: length(Project_list)){
  param_1<-rep(Project_list[i_dist], 1000)
  if(i_dist==1){
    param_2=param_1
  } else{
    param_2<-append(x = param_2, values = param_1)
    
  }
}

param_list_title<-c('Kern Delta', 'Wheeler', 'West Kern', 'Bel Ridge', 'Berrenda', 'Semitropic', 'Rose Dale', 'Buena Vista', 'Cawelo', 'Henry Miller', 'Lost Hills', 
                    'Saucelito', 'Delano', 'Lower Tule', 'Kern Tulare', 'Lindsay', 'Exeter', 'Lindmore', 'Porter Ville', 'Tea Pot', 'Terra', 'Sosan Joaquin', 
                    'Shaffer', 'North Kern', 'Dudley Ridge')

scenarios<-c("CDEC", "CGW",  "NGW", "BC")
folder_name_scenario<-c("CDEC_20062026", "WRF_GW_corr", "WRF", "Bias_Corrected")

for (i_scr in 1: length(scenarios)) {
  
  df_financial<-data.frame(matrix(0,ncol =100, nrow = 10))
  
  results_folder<-"output/"

  for (i_dist in 1: length(param_list)){
    file_address<-paste(results_folder,folder_name_scenario[i_scr], "/",param_list[i_dist], "_revenues.csv",  sep = "")
    
    if(file.exists(file_address)){
      CDEC_F<-read.csv(file_address, sep=",", header = T)[1:10,3:102]
      names(CDEC_F)<-paste(param_list[i_dist],"_scr_",seq(1,100), sep = "")
      if(i_dist==1){
        df_financial=CDEC_F
      } else {
        df_financial=cbind(df_financial, CDEC_F)
      }
    }
    
    
  } # i_dist
  
  write.table(df_financial, paste("/Financial/",scenarios[i_scr], ".txt", sep = "" ))
  
  
} # i_scr


district_rev<-read.table("/Paper Figures/Figure_5/InputDistRev.txt", header = T)


# ----- WRF
CDEC<-read.table("/Financial/CDEC.txt", header = T)
CGW<-read.table("/Financial/CGW.txt", header = T)[1:10,]
NGW<-read.table("/Financial/NGW.txt", header = T)[1:10,]
BC<-read.table("/Financial/BC.txt", header = T)[1:10,]

CGW_error_abs=abs(CGW-CDEC)
NGW_error_abs=abs(NGW-CDEC)
BC_error_abs=abs(BC-CDEC)

total_error_CGW<-data.frame(matrix(ncol = 100, nrow = 10))
total_error_NGW<-data.frame(matrix(ncol = 100, nrow = 10))
total_error_BC<-data.frame(matrix(ncol = 100, nrow = 10))

for (i_scr in 1:100){
  cols=c(i_scr, seq(1, 24)*100+i_scr)
  total_error_CGW[,i_scr]=rowSums(CGW_error_abs[,cols])
  total_error_NGW[,i_scr]=rowSums(NGW_error_abs[,cols])
  total_error_BC[,i_scr]=rowSums(BC_error_abs[,cols])
}

total_error_CGW_df=as.data.frame(total_error_CGW)
total_error_NGW_df=as.data.frame(total_error_NGW)
total_error_BC_df=as.data.frame(total_error_BC)
names(total_error_CGW_df)<-paste("price_scenario_", seq(1,100), sep = "")
print(paste("Maximum annual error CGW =", max(total_error_CGW_df), "USD"))
print(paste("Maximum annual error NGW =", max(total_error_NGW_df), "USD"))
print(paste("Maximum annual error BC =", max(total_error_BC_df), "USD"))


CGW_error=(CGW-CDEC)/CDEC*100
NGW_error=(NGW-CDEC)/CDEC*100
BC_error=(BC-CDEC)/CDEC*100

mlt_df_diff<-cbind(CGW=melt(CGW_error),NGW=melt(NGW_error)[,2], BC=melt(BC_error)[,2])
names(mlt_df_diff)<-c("Object", "CGW", "NGW", "BC")
mlt_df_diff[,"Year"]=rep(seq(2007,2016), 2500)
mlt_df_diff[,"Project"]=param_2


for (i_dist in 1: length(param_list)){
  
  df_3<-data.frame(
    Year=seq(2007,2016),
    CDEC[1:10,(1+(i_dist-1)*100):(i_dist*100)],
    CGW[,(1+(i_dist-1)*100):(i_dist*100)],
    NGW[,(1+(i_dist-1)*100):(i_dist*100)],
    BC[,(1+(i_dist-1)*100):(i_dist*100)]
  )
  
  if(i_dist==6){
    write.csv(df_3, "/Paper Figures/Figure_5/semitropic1.csv")
  }
  
  names(df_3)<-c("Year", rep("CFEWS-HIS", 100), rep("CGW", 100), rep("NGW", 100), rep("BC", 100))
  
  
  df_4<-data.frame(
    Year=seq(2007,2016),
    CDEC_average=apply(CDEC[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, mean),
    CDEC_max=apply(CDEC[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, max),
    CDEC_min=apply(CDEC[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, min),
    CGW_average=apply(CGW[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, mean),
    CGW_max=apply(CGW[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, max),
    CGW_min=apply(CGW[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, min),
    NGW_average=apply(NGW[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, mean),
    NGW_max=apply(NGW[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, max),
    NGW_min=apply(NGW[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, min),
    BC_average=apply(BC[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, mean),
    BC_max=apply(BC[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, max),
    BC_min=apply(BC[1:10,(1+(i_dist-1)*100):(i_dist*100)], MARGIN = 1, min)
  )
  
  if(i_dist==1){
    df_4_v2<-df_4
    df_4_v2[,"District"]=param_list[i_dist]
    df_4_acc<-df_4_v2
  } else{
    df_4_v2<-df_4
    df_4_v2[,"District"]=param_list[i_dist]
    df_4_acc<-rbind(df_4_acc,df_4_v2)
  }

  
  mlt_df_3<- melt(id.vars = "Year", df_3[,1:length(df_3[1,])])
  mlt_df_3[,"Scenario"]=c(rep("CFEWS-HIS", 1000), rep("CGW", 1000), rep("NGW", 1000), rep("BC", 1000))
  mlt_df_3[,"Year2"]=c(rep(seq(2007, 2016), 400))
  
  
  plt_F1<-ggplot(mlt_df_3, aes(x=as.factor(Year), y=value, fill=Scenario)) + geom_boxplot()+ theme_bw()+ #geom_line(size=1.1) +geom_point(size=2) +theme_bw() +
    scale_fill_manual(values = col_E) + xlab("") + 
    ylab("Total Irrigation District Revenue, \n (Million Dollar)")+
    labs(title = paste("Irrigation District -- ", param_list_title[i_dist]))
  
  plot_list_dam[[i_dist]] = plt_F1
  
  mlt_df4<-cbind(value_average=melt(df_4, id.vars = "Year", measure.vars = c("CDEC_average", "CGW_average", "NGW_average", "BC_average")),
                 value_min=melt(df_4, id.vars = "Year", measure.vars = c("CDEC_min", "CGW_min", "NGW_min", "BC_min"))[,3],
                 value_max=melt(df_4, id.vars = "Year", measure.vars = c("CDEC_max", "CGW_max", "NGW_max", "BC_max"))[,3])
  names(mlt_df4)<-c("Year", "Scenario", "Average", "Min", "Max")
    
  plt_F1.B<-ggplot(mlt_df4, aes(x=Year, y=Average, color=Scenario)) +
    geom_ribbon(aes(ymin=Min, ymax=Max, fill=Scenario), alpha=0.2)+
    geom_line(size=1.1) +geom_point(size=2) +theme_bw() +
    scale_colour_manual(values = col_E) + xlab("") + 
    scale_fill_manual(values = col_E) + xlab("") +
    ylab("Total Irrigation District Revenue, \n (Million Dollar)")+
    labs(title = paste("Irrigation District -- ", param_list_title[i_dist]))
  
  plot_list_dam.B[[i_dist]] = plt_F1.B
  
  if(i_dist==6){
    
    p5_3.B<-ggplot(mlt_df4, aes(x=Year, y=Average, color=Scenario)) +
      geom_ribbon(aes(ymin=Min, ymax=Max, fill=Scenario), alpha=0.2)+
      geom_line(size=1.1) +geom_point(size=2) +theme_bw() +
      scale_colour_manual(values = col_E) + xlab("") + 
      scale_fill_manual(values = col_E) + xlab("") +
      ylab("Total Irrigation District Revenue, \n (Million Dollar)")+
      labs(title = paste("c. Irrigation District -- ", param_list_title[i_dist]))+
      theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
      theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
      theme(title =element_text(size=12, face='bold'))
    
  } else if(i_dist==24){
    p5_4<-ggplot(mlt_df_3, aes(x=Year, y=value, colour=variable)) + geom_line(size=1.1) +geom_point(size=2) +theme_bw() +
      scale_color_manual(values = col_E) + xlab("") + ylab("Total Irrigation District Revenue (M-USD)")+
      labs(title = paste("d. Irrigation District -- ", param_list_title[i_dist]))+
      theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
      theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
      theme(title =element_text(size=12, face='bold'))
    
    p5_4.B<-ggplot(mlt_df4, aes(x=Year, y=Average, color=Scenario)) +
      geom_ribbon(aes(ymin=Min, ymax=Max, fill=Scenario), alpha=0.2)+
      geom_line(size=1.1) +geom_point(size=2) +theme_bw() +
      scale_colour_manual(values = col_E) + xlab("") + 
      scale_fill_manual(values = col_E) + xlab("") +
      ylab("Total Irrigation District Revenue, \n (Million Dollar)")+
      labs(title = paste("d. Irrigation District -- ", param_list_title[i_dist]))+
      theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
      theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
      theme(title =element_text(size=12, face='bold'))
  }
  
  mlt_df_3$Scenario <- factor(mlt_df_3$Scenario,levels = c("CFEWS-HIS", "CGW", "NGW", "BC"))
  
  plt_rg1<- ggplot(mlt_df_3, aes(x=value, y=Scenario, fill=Scenario)) + 
    geom_density_ridges_gradient( scale = 3, rel_min_height = 0.01, alpha=.6, color="white") +
    #scale_fill_viridis_c(name = "Revenue. [M-USD]", option = "C") + 
    labs(title = paste("Irrigation District -- ", param_list_title[i_dist])) +
    ylab("") + xlab("Total Irrigation District Revenue, \n (Million Dollar)") +theme_bw() + scale_fill_manual(values =col_E ) +
    theme_ridges( center_axis_labels = T) + theme(legend.title = element_blank()) +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          #axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  if(i_dist==6){
    p5_5<- ggplot(mlt_df_3, aes(x=value, y=Scenario, fill=Scenario)) + 
      geom_density_ridges_gradient( scale = 3, rel_min_height = 0.01, alpha=.6, color="white") +
      #scale_fill_viridis_c(name = "Revenue. [M-USD]", option = "C") + 
      labs(title = paste("e. Irrigation District -- ", param_list_title[i_dist])) +
      ylab("") + xlab("Total Irrigation District Revenue, \n (Million Dollar)") +theme_bw() + scale_fill_manual(values =col_E ) +
      rremove("y.text") + annotate("text", label="BC", x = -3, y = 4.5) +
      annotate("text", label="NGW", x = -3, y = 3.5) + annotate("text", label="CGW", x =-3, y = 2.5) +
      annotate("text", label="CFEWS-HIS", x = -2, y = 1.5) + theme(plot.title = element_text(face = "bold"))+
      theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
      theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            #axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
      theme(title =element_text(size=12, face='bold'))+ xlim(-7, 60)
  } else if(i_dist==24){
    p5_55<- ggplot(mlt_df_3, aes(x=value, y=Scenario, fill=Scenario)) + 
      geom_density_ridges_gradient( scale = 3, rel_min_height = 0.01, alpha=.6, color="white") +
      #scale_fill_viridis_c(name = "Revenue. [M-USD]", option = "C") + 
      labs(title = paste("f. Irrigation District -- ", param_list_title[i_dist])) +
      ylab("") + xlab("Total Irrigation District Revenue, \n (Million Dollar)") +theme_bw() + scale_fill_manual(values =col_E ) +
      rremove("y.text") + annotate("text", label="BC", x = -8, y = 4.5) +
      annotate("text", label="NGW", x = -8, y = 3.5) + annotate("text", label="CGW", x = -8, y = 2.5) +
      annotate("text", label="CFEWS-HIS", x = -7, y = 1.5)+ theme(plot.title = element_text(face = "bold"))+
      theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
      theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
            #axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
      theme(title =element_text(size=12, face='bold')) + xlim(-10, 45)
    }
    
 
  
  plot_list_dam_3[[i_dist]] = plt_rg1
  
} # i_dist


plt_1<-ggarrange(plotlist = plot_list_dam[1:8], ncol = 2, nrow = 4, common.legend = T)
plt_2<-ggarrange(plotlist = plot_list_dam[9:16], ncol = 2, nrow = 4, common.legend = T)
plt_3<-ggarrange(plotlist = plot_list_dam[17:26], ncol = 2, nrow = 5, common.legend = T)

plt_1.2<-ggarrange(plotlist = plot_list_dam_3[1:8], ncol = 2, nrow = 4, common.legend = T)
plt_2.2<-ggarrange(plotlist = plot_list_dam_3[9:16], ncol = 2, nrow = 4, common.legend = T)
plt_3.2<-ggarrange(plotlist = plot_list_dam_3[17:26], ncol = 2, nrow = 5, common.legend = T)

plt_1.3<-ggarrange(plotlist = plot_list_dam.B[1:8], ncol = 2, nrow = 4, common.legend = T)
plt_2.3<-ggarrange(plotlist = plot_list_dam.B[9:16], ncol = 2, nrow = 4, common.legend = T)
plt_3.3<-ggarrange(plotlist = plot_list_dam.B[17:26], ncol = 2, nrow = 5, common.legend = T)


ggsave("/Financial_results_1.png", plt_1, width = 12, height = 12)
ggsave("/Financial_results_2.png", plt_2, width = 12, height = 12)
ggsave("/Financial_results_3.png", plt_3, width = 12, height = 12)

ggsave("/Financial_results_rg1.png", plt_1.2, width = 12, height = 12)
ggsave("/Financial_results_rg2.png", plt_2.2, width = 12, height = 12)
ggsave("/Financial_results_rg3.png", plt_3.2, width = 12, height = 12)


ggsave("/Financial_results_ribbon_1.png", plt_1.3, width = 12, height = 12)
ggsave("/Financial_results_ribbon_2.png", plt_2.3, width = 12, height = 12)
ggsave("/Financial_results_ribbon_3.png", plt_3.3, width = 12, height = 12)


# ----------------------------------------------------------------------------
#--------------------------------------------------------------------- Bad Years

# ----- WRF
CDEC<-read.table("/Financial/CDEC.txt", header = T)
Wet<-read.table("/Financial/Wet.txt", header = T)
Dry<-read.table("/Financial/Dry.txt", header = T)


mlt_district_rev_2<-melt(mlt_df_diff, id.vars = c("Project"), measure.vars = c("CGW", "NGW", "BC"))
levels(mlt_district_rev_2$variable) <- sub("Error_", "", levels(mlt_district_rev_2$variable))

p5_6v2<-ggplot(mlt_district_rev_2,aes(x=variable, y=value, fill=variable) ) + geom_violin()+ geom_boxplot(width=0.1, fill="white", alpha=0.8) +
  scale_fill_manual(values = c("darkcyan", "darkred", "darkorange" )) + theme_bw() +facet_grid(cols = vars(Project))+
  labs(title="a. Mean Error in Revenue of Irrigation Districts",  x="", y="Mean Error (%)") + #geom_jitter(position=position_jitter(0.2)) +
  theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+ ylim(-100,100)+
  theme(title =element_text(size=12, face='bold'))

ggsave("/Error_districts_boxplot.png", p5_6v2)

df_4_acc_agg<-aggregate(df_4_acc[,2:13], by = list(df_4_acc$Year), sum)
df_4_acc_agg[,"Error_CGW"]=(df_4_acc_agg$CGW_average-df_4_acc_agg$CDEC_average)/df_4_acc_agg$CDEC_average*100
df_4_acc_agg[,"Error_NGW"]=(df_4_acc_agg$NGW_average-df_4_acc_agg$CDEC_average)/df_4_acc_agg$CDEC_average*100
df_4_acc_agg[,"Error_BC"]=(df_4_acc_agg$BC_average-df_4_acc_agg$CDEC_average)/df_4_acc_agg$CDEC_average*100


mlt_ann<-melt(df_4_acc_agg, id.vars = "Group.1", measure.vars = c("Error_CGW", "Error_NGW", "Error_BC"))
names(mlt_ann)<-c("Year", "variable", "value")

p5_8<-ggplot() + geom_bar(aes(x=mlt_ann$Year, y=mlt_ann$value, fill=mlt_ann$variable), stat = "identity",position=position_dodge(), width = .6) +
  theme_bw() + scale_fill_manual(values = c("darkcyan", "darkred", "darkorange"))+
  geom_line(aes(x=df_4_acc_agg$Group.1, y=(df_4_acc_agg$CDEC_average)/3), size=1) + geom_point(aes(x=df_4_acc_agg$Group.1, y=(df_4_acc_agg$CDEC_average)/3), size=3) +
  scale_y_continuous(sec.axis = sec_axis(~ . *3, name = "Revenue (Million USD)")) +
  theme(legend.position = "") + labs(x="", y="Mean Error (%)", title = "b. Annual Mean Error and Total Basin-Wide Revenue") +
  theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
  theme(title =element_text(size=12, face='bold'))

ggsave("/Error_annual.png", p5_8)


plt_l<-ggarrange(p5_6v2, p5_8, p5_3.B, p5_4.B ,p5_5, p5_55, ncol = 2, nrow = 3, legend = "none")
ggsave("/financial_plot.png", plt_l, width = 12, height = 12)
ggsave("/financial_plot.svg", plt_l, device = "svg", width = 12, height = 12)

