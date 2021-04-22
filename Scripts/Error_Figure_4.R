library(rhdf5)
library(zoo)
library(ggplot2)
library(hydroTSM)
library(reshape2)
library(hydroGOF)
library(ggpubr)
library(ggridges)

# ---------------------------------------------------Error paper ------------ Figure 4

col_E_1<-c("grey12", "darkorange", "darkcyan", "darkred")

#------------------
#---------------------- hdf accumulation function

hdf_sum_col<-function(input_file, input_rows){
  accum=0
  for(i in 1:length(input_rows)){
    accum<-accum+input_file[input_rows[i],]
  }
  return(accum)
}

#---------------- list of dams
# 1- Observed 2- GW correcred 3- No GW correction 
col_E<-c("grey12", "darkcyan", "darkred", "darkorange")

plot_list_1 = list()
plot_list_2 = list()
plot_list_3 = list()
plot_list_4 = list()
plot_list_5 = list()
plot_list_6 = list()
plot_list_7 = list()
plot_list_8 = list()


param_list<-c("buenavista", "wheeler", "henrymiller", "wonderful")

param_list_title<-c("BuenaVista-SWP", "Wheeler-SWP", "Henry Miller-SWP", "Wonderful-SWP")

Mlist = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

dates<-read.table("Dates.txt", header = T)

#---------- Dam Plots

hdf_CDEC<-h5read("/CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")
hdf_CDEC_short<-h5read("/CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")
hdf_list_CGW<-h5read("/WRF_GW_corr/pWRF_GW_corr/results_pWRF_GW_corr.hdf5", "sWRF_GW_corr")
hdf_list_NGW<-h5read("/WRF/pWRF/results_pWRF.hdf5", "sWRF")
hdf_list_BC<-h5read("/Bias_Corrected/pBias_Corrected/results_pBias_Corrected.hdf5", "sBias_Corrected")

ColName_CDEC<-h5readAttributes("/CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")$column
ColName_CDEC_short<-h5readAttributes("/CDEC_20062026/pCDEC_20062026/results_pCDEC_20062026.hdf5", "sCDEC_20062026")$column
ColName_CGW<-h5readAttributes("/WRF_GW_corr/pWRF_GW_corr/results_pWRF_GW_corr.hdf5", "sWRF_GW_corr")$column
ColName_NGW<-h5readAttributes("/WRF/pWRF/results_pWRF.hdf5", "sWRF")$column
ColName_BC<-h5readAttributes("/Bias_Corrected/pBias_Corrected/results_pBias_Corrected.hdf5", "sBias_Corrected")$column

GW_GOFs_CGW<-data.frame(matrix(ncol =length(param_list), nrow = 20 ))
GW_GOFs_NGW<-data.frame(matrix(ncol =length(param_list), nrow = 20 ))
GW_GOFs_BC<-data.frame(matrix(ncol =length(param_list), nrow = 20 ))

list_metrics=c("ME", "MAE", "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD" , "NSE", "mNSE", "rNSE", "d", 
               "md", "rd",  "cp",  "r",  "R2", "bR2", "KGE", "VE")

for(i_dam in 1:length(param_list)) {
  

  rows_scr_CDEC<-grep(pattern = param_list[i_dam], ColName_CDEC_short)
  col_CDEC<-rows_scr_CDEC[c(grep(ColName_CDEC_short[rows_scr_CDEC], pattern = "_recover_banked") , grep(ColName_CDEC_short[rows_scr_CDEC], pattern = "_exchanged_GW"))]
  CDEC<-hdf_sum_col(hdf_CDEC_short[,0:3653], col_CDEC)
  
  col_CDEC_recharge<-rows_scr_CDEC[c(grep(ColName_CDEC_short[rows_scr_CDEC], pattern = "recharge") , grep(ColName_CDEC_short[rows_scr_CDEC], pattern = "flood"))]
  col_remove1<-c(grep(ColName_CDEC_short[col_CDEC_recharge],  pattern = "irrigation"), grep(ColName_CDEC_short[col_CDEC_recharge],  pattern = "cap"))
  
  if(!length(col_remove1)==0){
    CDEC_recharge<-hdf_sum_col(hdf_CDEC_short[,0:3653], col_CDEC_recharge[-col_remove1])
  } else{
    CDEC_recharge<-hdf_sum_col(hdf_CDEC_short[,0:3653], col_CDEC_recharge)
    
  }
  
  rows_scr_CGW<-grep(pattern = param_list[i_dam], ColName_CGW)
  col_CGW<-rows_scr_CGW[c(grep(ColName_CGW[rows_scr_CGW], pattern = "_recover_banked") , grep(ColName_CGW[rows_scr_CGW], pattern = "_exchanged_GW"))]
  CGW<-hdf_sum_col(hdf_list_CGW, col_CGW)
  
  col_CGW_recharge<-rows_scr_CGW[c(grep(ColName_CGW[rows_scr_CGW], pattern = "recharge") , grep(ColName_CGW[rows_scr_CGW], pattern = "flood"))]
  col_remove2<-c(grep(ColName_CGW[col_CGW_recharge],  pattern = "irrigation"), grep(ColName_CGW[col_CGW_recharge],  pattern = "cap"))
  if(!length(col_remove2)==0){
    CGW_recharge<-hdf_sum_col(hdf_list_CGW, col_CGW_recharge[-col_remove2])
  } else{
    CGW_recharge<-hdf_sum_col(hdf_list_CGW, col_CGW_recharge)
    
  }
  
  
  rows_scr_NGW<-grep(pattern = param_list[i_dam], ColName_NGW)
  col_NGW<-rows_scr_NGW[c(grep(ColName_NGW[rows_scr_NGW], pattern = "_recover_banked") , grep(ColName_NGW[rows_scr_NGW], pattern = "_exchanged_GW"))]
  NGW<-hdf_sum_col(hdf_list_NGW, col_NGW)
  
  col_NGW_recharge<-rows_scr_NGW[c(grep(ColName_NGW[rows_scr_NGW], pattern = "recharge") , grep(ColName_NGW[rows_scr_NGW], pattern = "flood"))]
  col_remove3<-c(grep(ColName_NGW[col_NGW_recharge],  pattern = "irrigation"), grep(ColName_NGW[col_NGW_recharge],  pattern = "cap"))
  
  if(!length(col_remove3)==0){
    NGW_recharge<-hdf_sum_col(hdf_list_NGW, col_NGW_recharge[-col_remove3])
  } else{
    NGW_recharge<-hdf_sum_col(hdf_list_NGW, col_NGW_recharge)
    
  }
  
  
  rows_scr_BC<-grep(pattern = param_list[i_dam], ColName_BC)
  col_BC<-rows_scr_BC[c(grep(ColName_BC[rows_scr_BC], pattern = "_recover_banked") , grep(ColName_BC[rows_scr_BC], pattern = "_exchanged_GW"))]
  BC<-hdf_sum_col(hdf_list_BC, col_BC)
  
  col_BC_recharge<-rows_scr_BC[c(grep(ColName_BC[rows_scr_BC], pattern = "recharge") , grep(ColName_BC[rows_scr_BC], pattern = "flood"))]
  col_remove4<-c(grep(ColName_BC[col_BC_recharge],  pattern = "irrigation"), grep(ColName_BC[col_BC_recharge],  pattern = "cap"))
  
  if(!length(col_remove4)==0){
    BC_recharge<-hdf_sum_col(hdf_list_BC, col_BC_recharge[-col_remove4])
  } else{
    BC_recharge<-hdf_sum_col(hdf_list_BC, col_BC_recharge)
    
  }
  

  inds <- seq(as.Date("1996-10-1"), as.Date("2016-09-30"), by = "day")
  inds_WRF <- seq(as.Date("2006-10-1"), as.Date("2016-09-30"), by = "day")
  
  
  df_1<-data.frame(
    date<-inds_WRF,
    year<-dates$Year,
    month<-dates$Month,
    CDEC<-CDEC,
    CGW<-CGW,
    NGW<-NGW,
    BC<-BC,
    Diff_CGW<-CGW-CDEC,
    Diff_NGW<-NGW-CDEC,
    Diff_BC<-BC-CDEC,
    CDEC_recharge=CDEC_recharge,
    NGW_recharge=NGW_recharge,
    CGW_recharge=CGW_recharge,
    BC_recharge=BC_recharge
  )
  
  names(df_1)<-c("Date", "Year", "Month","Baseline", "GW_Corrected", "No_GW_Correction", "Bias_Corrected", "Diff_CGW", "Diff_NGW", "Diff_BC" , "CDEC_recharge", "NGW_recharge", "CGW_recharge", "BC_recharge") 
  
  mlt_df1<-melt(df_1, id.vars = "Date", measure.vars = c("Baseline", "GW_Corrected", "No_GW_Correction", "Bias_Corrected"))
  
  plt_ds<- ggplot(mlt_df1, aes(x=as.Date(Date), y=value, colour=variable)) + geom_line(size=1) + theme_bw() +
    labs(title = paste("Extraction from Banks -", param_list_title[i_dam]), x="", y= "Extracted Water (AF/Day)") +
    scale_color_manual(values = col_E) + theme(legend.title = element_blank()) #+ scale_x_date(date_breaks = "2 years")#, date_minor_breaks = "2 years")
  
  #plot_list_dam[[i_dam]] = plt_ds
  
  
  #---------- zoom
  
  zoom_S<-df_1[2500:3100,]
  
  zm_mlt_df1<-melt(zoom_S, id.vars = "Date", measure.vars = c("Baseline", "GW_Corrected", "No_GW_Correction","Bias_Corrected"))
  
  plt_ds_z<- ggplot(zm_mlt_df1, aes(x=as.Date(Date), y=value, colour=variable)) + geom_line(size=1) + theme_bw() +
    labs(title = paste("Extraction from Banks -", param_list_title[i_dam]), x="", y= "Extracted Water (AF/Day)") +
    scale_color_manual(values = col_E) + theme(legend.title = element_blank()) #+ scale_x_date(date_breaks = "2 years")#, date_minor_breaks = "2 years")
  
  ggsave(paste("daily_Ext_", param_list_title[i_dam], ".png", sep=""),plt_ds_z)
  
  obs_r.plt<-ggplot(df_1, aes(x =Baseline , y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient(scale = 3, size = 0.3) + 
    scale_y_continuous(breaks=seq(2006,2016,by = 1), limits = c(2006,2019))+
    labs(title=paste("Extraction from Banks -", param_list_title[i_dam]), x = "Extracted Water (AF/Day)", y="") +
    scale_fill_gradient(low = "grey", high = "black") +theme_bw()
  
  ggsave(paste("ridge_obs_", param_list_title[i_dam], ".png", sep=""), obs_r.plt)
  
  plot_list_1[[i_dam]] = obs_r.plt


  CGW_e_r.plt<-ggplot(df_1, aes(x = Diff_CGW, y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) +
    scale_y_continuous(breaks=seq(2006,2016,by = 1), limits = c(2006,2019))+
    labs(title=paste("GW Corrected Minus Baseline,", param_list_title[i_dam]), x = "Extracted Water (AF/Day)", y="") +
    scale_fill_gradient(low = "grey", high = "darkcyan") +theme_bw()
  
  ggsave(paste("E_ridge_CGW_", param_list_title[i_dam], ".png", sep=""), CGW_e_r.plt)
  
  plot_list_2[[i_dam]] = CGW_e_r.plt
  
  NGW_e_r.plt<- ggplot(df_1, aes(x = Diff_NGW, y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    scale_y_continuous(breaks=seq(2006,2016,by = 1), limits = c(2006,2019))+
    labs(title=paste("No GW Correction Minus Baseline,", param_list_title[i_dam]), x = "Extracted Water (AF/Day)", y="") +
    scale_fill_gradient(low = "grey", high = "darkred") +theme_bw() 
  
  ggsave(paste("E_ridge_NGW_", param_list_title[i_dam], ".png", sep=""), NGW_e_r.plt)
  
  plot_list_3[[i_dam]] = NGW_e_r.plt
  
  BC_e_r.plt<- ggplot(df_1, aes(x = Diff_BC, y = Year, group=Year, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    scale_y_continuous(breaks=seq(2006,2016,by = 1), limits = c(2006,2019))+
    labs(title=paste("No GW Correction Minus Baseline,", param_list_title[i_dam]), x = "Extracted Water (AF/Day)", y="") +
    scale_fill_gradient(low = "grey", high = "darkorange") +theme_bw() 
  
  ggsave(paste("E_ridge_BC_", param_list_title[i_dam], ".png", sep=""), BC_e_r.plt)
  
  plot_list_7[[i_dam]] = BC_e_r.plt
  
  y_agg_df_1<-aggregate(df_1, by=list(df_1$Year), mean)
  
  plt_y_agg_df_1_id<-c("a.", "a.", "b.", "b.")
  mlt_y_agg_df_1<-melt(y_agg_df_1, id.vars = "Group.1", measure.vars = c("Baseline", "GW_Corrected", "No_GW_Correction", "Bias_Corrected"))
  
  levels(mlt_y_agg_df_1$variable) <- sub("Baseline", "Baseline", levels(mlt_y_agg_df_1$variable))
  
  plt_y_agg_df_1<-ggplot( mlt_y_agg_df_1, aes(x=Group.1, y=value, colour=variable)) +geom_line(size=1) + geom_point(size=3) +scale_color_manual(values =col_E)+
    labs(title=paste(plt_y_agg_df_1_id[i_dam],"Annual Extraction from Banks -", param_list_title[i_dam]), y = "Extracted Water (AF/Day)", x="")  + theme_pubr()+
    theme(legend.title = element_blank())
  ggsave(paste("Annual_Ext_", param_list_title[i_dam], ".png", sep=""), plt_y_agg_df_1)
  
  plot_list_4[[i_dam]] = plt_y_agg_df_1
  
  m_agg_df_1<-aggregate(df_1, by=list(df_1$Month), mean)
  
  plt_m_agg_df_1_id<-c("e.", "e.", "f.", "f.")
  mlt_m_agg_df_1<-melt(m_agg_df_1, id.vars = "Group.1", measure.vars = c("Baseline", "GW_Corrected", "No_GW_Correction",  "Bias_Corrected"))
  plt_m_agg_df_1<-ggplot(mlt_m_agg_df_1, aes(x=Group.1, y=value, colour=variable)) +geom_line(size=1) + geom_point(size=3) +scale_color_manual(values =col_E)+
    labs(title=paste(plt_m_agg_df_1_id[i_dam], "Monthly Extraction from Banks -", param_list_title[i_dam]), y = "Extracted Water (AF/Day)", x="") + theme(legend.title = element_blank())+
    scale_x_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist) + theme_pubr()
  ggsave(paste("Monthly_Ext_", param_list_title[i_dam], ".png", sep=""), plt_m_agg_df_1)
  
  plot_list_5[[i_dam]] = plt_m_agg_df_1
  
  #---------------------------------box plot
  
  plt_bxp_22_id<-c("e.", "e.","f.", "f.")
  agg_df_1<-aggregate(df_1[,4:10], by=list(df_1[,2]), max)
  Emlt_M_df_flow<-melt(agg_df_1, id.vars = "Group.1",  measure.vars = c("Baseline", "GW_Corrected", "No_GW_Correction", "Bias_Corrected"))
  
  plt_bxp_22<-ggplot(Emlt_M_df_flow, aes(x= Group.1, y=value*1233.48*1000/10^6, fill=variable)) + geom_col(position=position_dodge()) +  theme_bw()+
    scale_fill_manual(values = col_E_1) +#theme(axis.text.x=element_blank()) + 
    labs(x="", y="Extracted Water (Million Cubic Meters)", title=paste(plt_bxp_22_id[i_dam], "Annual Extraction from Banks -", param_list_title[i_dam])) +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  ggsave(filename = paste( "Boxplot_", param_list_title[i_dam], ".png", sep = ""), plt_bxp_22)
  
  plot_list_6[[i_dam]] = plt_bxp_22
  
  GW_GOFs_CGW[,i_dam]=gof(sim = df_1$GW_Corrected, obs = df_1$Baseline)
  GW_GOFs_NGW[,i_dam]=gof(sim = df_1$No_GW_Correction, obs = df_1$Baseline)
  GW_GOFs_BC[,i_dam]=gof(sim = df_1$Bias_Corrected, obs = df_1$Baseline)
  
  # recharge
  
  plt_recharge<-c("c.", "c.","d.", "d.")
  agg_df_1<-aggregate(df_1[,4:14], by=list(df_1[,2]), max)
  Emlt_M_df_recharge<-melt(agg_df_1, id.vars = "Group.1",  measure.vars = c("CDEC_recharge", "NGW_recharge", "CGW_recharge", "BC_recharge"))
  
  plt_bxp_recharge<-ggplot(Emlt_M_df_recharge, aes(x= Group.1, y=value*1233.48*1000/10^6, fill=variable)) + geom_col(position=position_dodge()) +  theme_bw()+
    scale_fill_manual(values = col_E_1) +#theme(axis.text.x=element_blank()) + 
    labs(x="", y="Water Recharge (Million Cubic Meters)", title=paste(plt_recharge[i_dam], "Annual Recharge into Banks -", param_list_title[i_dam])) +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  ggsave(filename = paste( "Recharge_Boxplot_", param_list_title[i_dam], ".png", sep = ""), plt_bxp_22)
  
  plot_list_8[[i_dam]] = plt_bxp_recharge

} #i_dam


 plt_c_2<-ggarrange(plotlist = c(plot_list_1[1], plot_list_1[2], plot_list_3[1], plot_list_3[2], plot_list_2[1],  plot_list_2[2],  
                    plot_list_4[1], plot_list_4[2], plot_list_5[1], plot_list_5[2]), ncol = 2, nrow =5, common.legend = T)

 plt_c_3<-ggarrange(plotlist = c(plot_list_4[1], plot_list_4[3], plot_list_6[1], plot_list_6[3], plot_list_5[1],  plot_list_5[3]  
                                 ), ncol = 2, nrow =3, common.legend = T)
 
 plt_c_4<-ggarrange(plotlist = c(plot_list_gwb[1], plot_list_gwb[2], plot_list_gwb[3],  plot_list_gwb[4] , plot_list_6[1], plot_list_6[4]), 
                    ncol = 2, nrow =3, common.legend = T)
 
 plt_c_5<-ggarrange(plotlist = c(plot_list_storage[1], plot_list_storage[2], plot_list_8[1],  plot_list_8[4] , plot_list_6[1], plot_list_6[4]), 
                    ncol = 2, nrow =3, common.legend = T) # final plot
 
ggsave("BankExt_combined.png", plt_c_2, width = 12, height = 15)
ggsave("BankExt_combined_2.png", plt_c_3, width = 12, height = 12.5)
ggsave("BankExt_combined_3.png", plt_c_4, width = 12, height = 12.5)
ggsave("BankExt_combined_5.png", plt_c_5, width = 12, height = 12.5)
ggsave("BankExt_combined_5.svg",device =svg , plt_c_5, width = 12, height = 12.5)


GW_GOFs_CGW=cbind(list_metrics, GW_GOFs_CGW)
names(GW_GOFs_CGW)=c("list_metrics",param_list)
GW_GOFs_NGW=cbind(list_metrics, GW_GOFs_NGW)
names(GW_GOFs_NGW)=c("list_metrics",param_list)
GW_GOFs_BC=cbind(list_metrics, GW_GOFs_BC)
names(GW_GOFs_BC)=c("list_metrics",param_list)


# cols_scr_berr<-grep(pattern = "_exchanged_GW", ColName_input)
cols_scr_all<-grep(pattern = "wonderful", ColName_input)
cols_scr_berr<-grep(pattern = "exchanged_GW", ColName_input[cols_scr_all])



