library(rhdf5)
library(zoo)
library(ggplot2)
library(hydroTSM)
library(reshape2)
library(hydroGOF)
library(ggpubr)
library(ggridges)


#----------------------------- Error paper -------------- FIGURE - 3

AF_CFS<-504.17
CFS_M3S<-0.028316847
AF_M3<-1233.48

col_eval<-c("grey12", "darkcyan", "darkred", "darkorange", "blue1")


dates<-read.table("Dates.txt", header = T)
#---------------- list of dams
# 1- Observed 2- GW correcred 3- No GW correction 
col_E<-c("grey12", "darkcyan", "darkred", "darkorange")

plot_list_dam=list()
plot_list_1 = list()
plot_list_2 = list()
plot_list_3 = list()
plot_list_4 = list()
plot_list_5 = list()
plot_list_6 = list()

param_list<-c("delta_HRO_pump", "delta_TRP_pump")

param_list_title<-c("SWP", "CVP")

Mlist = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

input_obs<-read.csv("cord-data.csv")

TRP_obs<-input_obs$TRP_pump  #CVP
HRO_obs<-input_obs$HRO_pump  #SWP


#---------- Dam Plots

hdf_CDEC<-h5read("CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")
hdf_CDEC_short<-h5read("CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")
hdf_list_CGW<-h5read("WRF_GW_corr/pWRF_GW_corr/results_pWRF_GW_corr.hdf5", "sWRF_GW_corr")
hdf_list_NGW<-h5read("WRF/pWRF/results_pWRF.hdf5", "sWRF")
hdf_list_BC<-h5read("Bias_Corrected/pBias_Corrected/results_pBias_Corrected.hdf5", "sBias_Corrected")

ColName_CDEC<-h5readAttributes("CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")$column
ColName_CDEC_short<-h5readAttributes("CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")$column
ColName_CGW<-h5readAttributes("WRF_GW_corr/pWRF_GW_corr/results_pWRF_GW_corr.hdf5", "sWRF_GW_corr")$column
ColName_NGW<-h5readAttributes("WRF/pWRF/results_pWRF.hdf5", "sWRF")$column
ColName_BC<-h5readAttributes("Bias_Corrected/pBias_Corrected/results_pBias_Corrected.hdf5", "sBias_Corrected")$column

for(i_dam in 1:length(param_list)) {

  col_CDEC<-which(ColName_CDEC_short==paste(param_list[i_dam], sep=""))
  CDEC<-hdf_CDEC_short[col_CDEC,0:3653]
  
  col_CGW<-which(ColName_CGW==paste(param_list[i_dam], sep=""))
  CGW<-hdf_list_CGW[col_CGW,]
  
  col_NGW<-which(ColName_NGW==paste(param_list[i_dam], sep=""))
  NGW<-hdf_list_NGW[col_NGW,]
  
  col_BC<-which(ColName_BC==paste(param_list[i_dam], sep=""))
  BC<-hdf_list_BC[col_BC,]
  
  inds <- seq(as.Date("1996-10-1"), as.Date("2016-09-30"), by = "day")
  inds_WRF <- seq(as.Date("2006-10-1"), as.Date("2016-09-30"), by = "day")
  
  if(i_dam==1){
    observed_pumping=HRO_obs[3653:7305]
      
  } else{
    observed_pumping=TRP_obs[3653:7305]
  }
  
  df_1<-data.frame(
    date<-inds_WRF,
    year<-dates$Year,
    month<-dates$Month,
    CDEC<-CDEC,
    CGW<-CGW,
    NGW<-NGW,
    BC<-BC,
    Diff_CDEC<-CDEC-observed_pumping/AF_CFS,
    Diff_CGW<-CGW-observed_pumping/AF_CFS,
    Diff_NGW<-NGW-observed_pumping/AF_CFS,
    Diff_BC<-BC-observed_pumping/AF_CFS,
    observed_pumping<-observed_pumping/AF_CFS
  )
  
  names(df_1)<-c("Date", "Year", "Month","Observed", "GW_Corrected", "No_GW_Correction", "Bias_Corrected", "Diff_CDEC", "Diff_CGW", "Diff_NGW", "Diff_BC" , "Observed_Pumping") 
  
  mlt_df1<-melt(df_1, id.vars = "Date", measure.vars = c("Observed", "GW_Corrected", "No_GW_Correction", "Bias_Corrected"))
  
  plt_ds<- ggplot(mlt_df1, aes(x=as.Date(Date), y=value, colour=variable)) + geom_line(size=1) + theme_bw() +
    labs(title = paste("Pumping From Delta  -", param_list_title[i_dam]), x="", y= "Pumping Rate (TAF/Day)") +
    scale_color_manual(values = col_E) + theme(legend.title = element_blank()) #+ scale_x_date(date_breaks = "2 years")#, date_minor_breaks = "2 years")
  
  plot_list_dam[[i_dam]] = plt_ds
  
  
  #---------- zoom
  
  zoom_S<-df_1[2500:3100,]
  
  zm_mlt_df1<-melt(zoom_S, id.vars = "Date", measure.vars = c("Observed", "GW_Corrected", "No_GW_Correction", "Bias_Corrected"))
  
  plt_ds_z<- ggplot(zm_mlt_df1, aes(x=as.Date(Date), y=value**AFD_to_CMS*1000, colour=variable)) + geom_line(size=1) + theme_bw() +
    labs(title = paste("Pumping from Delta to", param_list_title[i_dam]), x="", y= "Pumping Rate (Cubic meter per second)") +
    scale_color_manual(values = col_E) + theme(legend.title = element_blank()) #+ scale_x_date(date_breaks = "2 years")#, date_minor_breaks = "2 years")
  
  ggsave(paste("/Pumping/daily_pumping_", param_list_title[i_dam], ".png", sep=""),plt_ds_z)
  obs_r.plt_id<-c("c.", "c.")
  obs_r.plt<-ggplot(df_1[2285:3653,], aes(x =Diff_CDEC*AFD_to_CMS*1000 , y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient(scale = 3, size = 0.3) + 
    scale_y_continuous(breaks=seq(2013,2016,by = 1), limits = c(2013,2018.6))+
    #scale_x_continuous(limits = c(0,quantile(df_flow$Observed, .99)*AFD_to_CMS))+
    labs(title=paste(obs_r.plt_id[i_dam], "CFEWS-HIS Minus Observed, ", param_list_title[i_dam]), x = "Pumping Rate (Cubic meter per second)", y="") +
    scale_fill_gradient(low = "grey", high = "black") +theme_bw()+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(paste("/Pumping/ridge_obs_", param_list_title[i_dam], ".png", sep=""), obs_r.plt)
  
  plot_list_1[[i_dam]] = obs_r.plt

  CGW_e_r.plt_id<-c("e.", "e.")
  CGW_e_r.plt<-ggplot(df_1[2285:3653,], aes(x = Diff_CGW*AFD_to_CMS*1000, y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) +
    scale_y_continuous(breaks=seq(2013,2016,by = 1), limits = c(2013,2018.4))+
    labs(title=paste(CGW_e_r.plt_id[i_dam], "GW Corrected Minus Observed,", param_list_title[i_dam]), x = "Pumping Rate (Cubic meter per second)", y="") +
    scale_fill_gradient(low = "grey", high = "darkcyan") +theme_bw() +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(paste("/Pumping/E_ridge_CGW_", param_list_title[i_dam], ".png", sep=""), CGW_e_r.plt)
  
  plot_list_2[[i_dam]] = CGW_e_r.plt
  
  NGW_e_r.plt_id<-c("d.", "d.")
  NGW_e_r.plt<- ggplot(df_1[2285:3653,], aes(x = Diff_NGW*AFD_to_CMS*1000, y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    scale_y_continuous(breaks=seq(2013,2016,by = 1), limits = c(2013,2018.3))+
    labs(title=paste(NGW_e_r.plt_id[i_dam], " No GW Correction Minus Observed, ", param_list_title[i_dam], sep = ""), x = "Pumping Rate (Cubic meter per second)", y="") +
    scale_fill_gradient(low = "grey", high = "darkred") +theme_bw() +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(paste("/Pumping/E_ridge_NGW_", param_list_title[i_dam], ".png", sep=""), NGW_e_r.plt)
  
  plot_list_3[[i_dam]] = NGW_e_r.plt
  
  plot_list_2[[i_dam]] = CGW_e_r.plt
  
  bc_e_r.plt_id<-c("f.", "f.")
  BC_e_r.plt<- ggplot(df_1[2285:3653,], aes(x = Diff_BC*AFD_to_CMS*1000, y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    scale_y_continuous(breaks=seq(2013,2016,by = 1), limits = c(2013,2019))+
    labs(title=paste(bc_e_r.plt_id[i_dam], " Bias corrected Minus Observed, ", param_list_title[i_dam], sep = ""), x = "Pumping Rate (Cubic meter per second)", y="") +
    scale_fill_gradient(low = "grey", high = "darkorange") +theme_bw()+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(paste("/Pumping/E_ridge_BC_", param_list_title[i_dam], ".png", sep=""), BC_e_r.plt)
  
  plot_list_6[[i_dam]] = BC_e_r.plt
  
  y_agg_df_1<-aggregate(df_1, by=list(df_1$Year), mean)
  
  plt_y_agg_df_1_id<-c("b.", "b.")
  mlt_y_agg_df_1<-melt(y_agg_df_1, id.vars = "Group.1", measure.vars = c("Observed", "GW_Corrected", "No_GW_Correction", "Bias_Corrected", "Observed_Pumping"))
  
  mlt_y_agg_df_1$variable<-c(rep("CFEWS-HIS", 10), rep("CGW", 10), rep("NGW", 10), rep("BC", 10), rep("Observed", 10))
  mlt_y_agg_df_1$variable = factor(mlt_y_agg_df_1$variable, levels = c("CFEWS-HIS","CGW","NGW","BC","Observed"))
  
  
  plt_y_agg_df_1<-ggplot(mlt_y_agg_df_1, aes(x=Group.1, y=value*AFD_to_CMS*1000, colour=variable)) +geom_line(size=1) + geom_point(size=3) +scale_color_manual(values =col_eval)+ #col_E)+
    labs(title=paste(plt_y_agg_df_1_id[i_dam],"Average Annual Pumping to", param_list_title[i_dam]), y = "Pumping Rate (Cubic meter per second)", x="")  + theme_pubr() +
    theme(legend.title = element_blank())+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  ggsave(paste("/Pumping/Annual_Pumping_", param_list_title[i_dam], ".png", sep=""), plt_y_agg_df_1) 
  
  plot_list_4[[i_dam]] = plt_y_agg_df_1
  
  m_agg_df_1<-aggregate(df_1, by=list(df_1$Month), mean)
  
  plt_m_agg_df_id<-c("a.", "a.")
  mlt_m_agg_df_1<-melt(m_agg_df_1, id.vars = "Group.1", measure.vars = c("Observed", "GW_Corrected", "No_GW_Correction", "Bias_Corrected", "Observed_Pumping"))
  
  mlt_m_agg_df_1$variable<-c(rep("CFEWS-HIS", 12), rep("CGW", 12), rep("NGW", 12), rep("BC", 12), rep("Observed", 12))
  mlt_m_agg_df_1$variable = factor(mlt_m_agg_df_1$variable, levels = c("CFEWS-HIS","CGW","NGW","BC","Observed"))
  
  plt_m_agg_df_1<-ggplot( mlt_m_agg_df_1, aes(x=Group.1, y=value*AFD_to_CMS*1000, colour=variable)) +geom_line(size=1) + geom_point(size=3) +scale_color_manual(values =col_eval)+ #col_E)+
    labs(title=paste(plt_m_agg_df_id[i_dam], "Average Monthly Pumping to", param_list_title[i_dam]), y = "Pumping Rate (Cubic meter per second)", x="") + theme(legend.title = element_blank())+
    scale_x_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist) + theme_pubr() +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(paste("/Pumping/Monthly_Pumping_", param_list_title[i_dam], ".png", sep=""), plt_m_agg_df_1)
  
  plot_list_5[[i_dam]] = plt_m_agg_df_1
  
  #---------------------------------------------------------

} #i_dam
# 
 # plt_c_2<-ggarrange(plotlist = c(plot_list_1[1], plot_list_2[1], plot_list_3[1], plot_list_1[2], plot_list_2[2], plot_list_3[2]), 
 #                    ncol = 3, nrow =2, common.legend = T)
 
 # plt_c_2<-ggarrange(plotlist = c(plot_list_1[1], plot_list_1[2], plot_list_3[1], plot_list_3[2], plot_list_2[1],  plot_list_2[2]), 
 #                    ncol = 2, nrow =3, common.legend = T)
 
 plt_c_2<-ggarrange(plotlist = c(plot_list_1[1], plot_list_1[2], plot_list_3[1], plot_list_3[2], plot_list_2[1],  plot_list_2[2],  
                                 plot_list_6[1],  plot_list_6[2],
                    plot_list_4[1], plot_list_4[2], plot_list_5[1], plot_list_5[2]), ncol = 4, nrow =3, common.legend = T, legend = "none")
 
 plt_c_2.1=ggarrange(plotlist = c(plot_list_5[1], plot_list_4[1] , plot_list_1[1], plot_list_3[1], plot_list_2[1], plot_list_6[1]) , ncol = 2, nrow =3, common.legend = T, legend = "top")
 ggsave("/Pumping/DeltaPump_SWP.png", plt_c_2.1, width = 12, height = 12)
 ggsave("/Pumping/DeltaPump_SWP.svg", plt_c_2.1, device = "svg", width = 12, height = 12)
 
 plt_c_2.2=ggarrange(plotlist = c(plot_list_5[2], plot_list_4[2],   plot_list_1[2], plot_list_3[2], plot_list_2[2], plot_list_6[2]) , ncol = 2, nrow =3, common.legend = T, legend = "top")
 ggsave("/Pumping/DeltaPump_CVP.png", plt_c_2.2, width = 12, height = 12)
 ggsave("/Pumping/DeltaPump_CVP.svg", plt_c_2.2, device = "svg", width = 12, height = 12)
 

