

########-------------------------------------- Figure 2

library(zoo)
library(hydroTSM)
library(ggplot2)
library(reshape)
library(reshape2)
library(grDevices)
library(sp)
library(hydroGOF)
library(hydroTSM)
library(ggridges)
library(ggpubr)
library(cowplot)


AF_CFS=504.17
AFD_to_CMS=0.014276


plot_number<-c("a", "b", "c", "d", "e", "f", "g", "h")

dates<-read.table("Dates.txt", header = T)

dam_list_1<-c("DNP_fnf", "FOL_fnf", "ISB_fnf", "MIL_fnf", "NML_fnf", "ORO_fnf", "PFT_fnf", "SHA_fnf", "YRS_fnf")
dam_list_title1<-c("Donpedro", "Folsom", "Isabella", "Millerton", "Newmelones" , "Oroville", "Pine Flat",  "Shasta",  "Yuba")
dam_list_title2<-c("Donpedro", "Folsom", "Isabella", "Millerton", "Newmelones" , "Oroville", "PineFlat",  "Shasta",  "Yuba")


flow_CDEC=read.csv("cord-data.csv", header = T, sep = ",")[3653:7305,18:32] #18:32
flow_CGW=70.0457*read.csv("WRF_GWCorrect_20062016.csv", header = T, sep = ",") [1:3653,]
flow_NGW=read.csv("WRF_20062016.csv", header = T, sep = ",")[1:3653,]
flow_BC=read.csv("bias_corrected_to_CALFEWs.csv", header = T, sep = ",")[1:3653,]


inds <- seq(as.Date("1996-10-1"), as.Date("2016-09-30"), by = "day")
inds_WRF <- seq(as.Date("2006-10-1"), as.Date("2016-09-30"), by = "day")

Mlist = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

plot_list_0=list()
plot_list_1 = list()
plot_list_2 = list()
plot_list_3 = list()
plot_list_4 = list()
plot_list_5 = list()
plot_list_6 = list()
plot_list_7 = list()
plot_list_8 = list()
plot_list_9 = list()
plot_list_10 = list()

df_streamflow<-data.frame(matrix(nrow = length(flow_CDEC[,1]), ncol = (length(dam_list_title1)*4+4)))
names(df_streamflow)<-c("Date", "Month", "Year", "WaterYear", paste(dam_list_title2, c("_CDEC"), sep=""), paste(dam_list_title2, c("_NGW"),  sep=""), 
                        paste(dam_list_title2, c("_CGW"), sep=""),paste( dam_list_title2, c("_BC"), sep=""))
                        
df_streamflow[,]

for (i_dam in 1:length(dam_list_1)) {
  col_num_CDEC<-which(names(flow_CDEC)==dam_list_1[i_dam])
  col_num_CGW<-which(names(flow_CGW)==dam_list_1[i_dam]) 
  col_num_NGW<-which(names(flow_NGW)==dam_list_1[i_dam])
  col_num_BC<-which(names(flow_BC)==dam_list_1[i_dam])
  
  flow_CDEC[flow_CDEC[,col_num_CDEC]==0, col_num_CDEC]=1
  
  df_flow<-data.frame(
    Date<-inds_WRF,
    Month<-dates$Month,
    Day<-dates$Day,
    Year<-dates$Water_jy,
    WaterYear<-dates$Year,
    CDEC=flow_CDEC[,col_num_CDEC],
    CGW=flow_CGW[, col_num_CGW],
    NGW=flow_NGW[, col_num_NGW] *70.0457,
    BC=flow_BC[, col_num_BC] *70.0457,
    ECGW=(flow_CGW[, col_num_CGW]-flow_CDEC[,col_num_CDEC])/flow_CDEC[,col_num_CDEC]*100,
    ENGW=(flow_NGW[, col_num_NGW] *70.0457-flow_CDEC[,col_num_CDEC])/flow_CDEC[,col_num_CDEC]*100,
    EBC=(flow_BC[, col_num_BC] *70.0457-flow_CDEC[,col_num_CDEC])/flow_CDEC[,col_num_CDEC]*100
  )
  names(df_flow)<-c("Date", "Month", "Day", "Year", "WaterYear",  "CDEC", "GW_Corrected", "NoGW_Correction", "Bias_Corrected", "Error_GW_Corrected", "Error_No_GW_Correction", "Error_Bias_Corrected")
  
  
  if(i_dam==1){
    df_streamflow[,1:4]=df_flow[,c(1,2,4,5)]
  }
  
  df_streamflow[,paste(dam_list_title2[i_dam], "_CDEC", sep = "")]=df_flow$CDEC
  df_streamflow[,paste(dam_list_title2[i_dam],"_NGW", sep = "")]=df_flow$NoGW_Correction
  df_streamflow[,paste(dam_list_title2[i_dam],"_CGW",  sep = "")]=df_flow$GW_Corrected
  df_streamflow[,paste(dam_list_title2[i_dam],"_BC",  sep = "")]=df_flow$Bias_Corrected
  
  
  
  df_error=df_flow[df_flow$CDEC>quantile(df_flow$CDEC, 0.99),]
  df_error_min=df_flow[df_flow$CDEC<quantile(df_flow$CDEC, 0.05),]
  
  # ---------- a- daily flow 
  Emlt_M_df_flow2<-melt(df_flow[1200:1300,], id.vars = "Date",  measure.vars = c("CDEC", "GW_Corrected", "NoGW_Correction", "Bias_Corrected"))
  
  plt_daily_1<-ggplot(Emlt_M_df_flow2, aes(x=as.Date(Date), y=value*AFD_to_CMS, colour=variable )) +geom_line(size=1) + theme_bw() +
    scale_color_manual(values = c("grey12", "darkcyan", "darkred", "darkorange")) +
    labs(title= paste(plot_number[i_dam],". Daily inflow to", dam_list_title1[i_dam]), x = "", y="Streamflow (Cubic meter per second)") + theme(legend.title = element_blank())
  ggsave(filename = paste( "/daily/", dam_list_title1[i_dam], ".png", sep = ""), plt_daily_1)
  
  plot_list_0[[i_dam]] = plt_daily_1
  
  
  #------- a- monthly
  M_df_flow=aggregate(df_flow, by = list(df_flow[,2]), mean)
  
  mlt_M_df_flow<-melt(M_df_flow, id.vars = "Month", measure.vars = c("CDEC", "NoGW_Correction", "GW_Corrected", "Bias_Corrected"))
  
  mlt_M_df_flow$variable<-c(rep("CDEC", 12), rep("NGW", 12),  rep("CGW", 12), rep("BC", 12))
  mlt_M_df_flow$variable = factor(mlt_M_df_flow$variable, levels = c("CDEC","NGW","CGW","BC"))
  
  plt_m_m<-ggplot(mlt_M_df_flow, aes(x=Month, y=value*AFD_to_CMS, colour=variable)) +geom_line(size=1)+ geom_point(size=3) +
    theme_pubr()+  labs(title= paste("a. Monthly average inflow to", dam_list_title1[i_dam]), x = "", y="Streamflow (Cubic meter per second)") +
    scale_x_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist) +theme(legend.title = element_blank())+
    scale_color_manual(values = c("blue1", "darkred", "darkcyan", "darkorange"))+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(filename = paste( "/monthly/", dam_list_title1[i_dam], ".png", sep = ""), plt_m_m)
  
  plot_list_1[[i_dam]] = plt_m_m
  
  #--------- c- annual
  Y_df_flow=aggregate(df_flow, by = list(df_flow[,4]), mean)
  
  mlt_Y_df_flow<-melt(Y_df_flow, id.vars = "Year", measure.vars = c("CDEC", "NoGW_Correction", "GW_Corrected", "Bias_Corrected"))
  plt_y_m<-ggplot(mlt_Y_df_flow, aes(x=Year, y=value*AFD_to_CMS, colour=variable)) +geom_line(size=1)+ geom_point(size=3) +
    theme_pubr()+  labs(title= paste("b. Annual average inflow to", dam_list_title1[i_dam]), x = "", y="Streamflow (Cubic meter per second)") +
    #scale_x_continuous(breaks=seq(1,12, by = 1), limits = c(1,12), labels = Mlist) +
    scale_color_manual(values = c("blue1", "darkred", "darkcyan", "darkorange")) + theme(legend.title = element_blank()) +
    theme(legend.title = element_blank())+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(filename = paste( "/annual/", dam_list_title1[i_dam], ".png", sep = ""), plt_y_m)
  
  plot_list_2[[i_dam]] = plt_y_m
  
  

  

  #---------------------- PDF flow CDEC
  
  rmlt_Y_df_flow<-melt(df_flow, id.vars = "Month", measure.vars = c("CDEC", "NoGW_Correction", "GW_Corrected",  "Bias_Corrected"))
  
  plt_cdec_fpdf<-ggplot(df_flow, aes(x = log(CDEC*AFD_to_CMS+1), y = Month, group=Month, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,14),  expand = c(0,0)) +
    #scale_x_continuous(limits = c(0,quantile(df_flow$CDEC, .99)*AFD_to_CMS))+
    scale_x_continuous(limits = c(0,8), expand = c(0,0))+
    #scale_x_continuous(trans = "log10")+
    labs(title=paste("c. Streamflow PDF (CDEC) at", dam_list_title1[i_dam]), x = "Log10[Streamflow (Cubic meter per second)]", y="Month") +
    scale_fill_gradient(low = "grey", high = "blue1") + theme_bw() +
    theme(panel.grid.major.y  = element_line(colour="grey12", size=0.5))+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold')) #+theme_ridges()
  

  
  ggsave(filename = paste( "/PDF_flow/CDEC_", dam_list_title1[i_dam], ".png", sep = ""), plt_cdec_fpdf)
  
  plot_list_4[[i_dam]] = plt_cdec_fpdf
  
  
  #--------------- PDF flow NO groundwater correction
  
  plt_NGW_fpdf<-ggplot(df_flow, aes(x = log(NoGW_Correction*AFD_to_CMS+1), y = Month, group=Month, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,14), expand = c(0,0)) +
    #scale_x_continuous(limits = c(0,quantile(df_flow$CDEC, .99)*AFD_to_CMS))+
    scale_x_continuous(limits = c(0,8), expand = c(0,0))+
    #scale_x_continuous(trans = "log10")+
    labs(title=paste("d. Streamflow PDF (NGW) at", dam_list_title1[i_dam]), x = "Log10[Streamflow (Cubic meter per second)]", y="Month") +
    scale_fill_gradient(low = "grey", high = "darkred") +theme_bw()+
    theme(panel.grid.major.y  = element_line(colour="grey12", size=0.5))+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold'))
  
  ggsave(filename = paste( "/PDF_flow/NGW_", dam_list_title1[i_dam], ".png", sep = ""), plt_NGW_fpdf)
  
  plot_list_6[[i_dam]] = plt_NGW_fpdf
  
  
    #------------ PDF flow groundwater corrected
  
  plt_CGW_fpdf<-ggplot(df_flow, aes(x = log(GW_Corrected*AFD_to_CMS+1), y = Month, group=Month,  fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,14), expand = c(0,0)) +
    #scale_x_continuous(limits = c(0,quantile(df_flow$CDEC, .99)*AFD_to_CMS))+
    scale_x_continuous(limits = c(0,8), expand = c(0,0))+
    labs(title=paste("e. Streamflow PDF (CGW) at", dam_list_title1[i_dam]), x = "Log10[Streamflow (Cubic meter per second)]", y="Month") +
    scale_fill_gradient(low = "grey", high = "darkcyan") +theme_bw() + 
    theme(panel.grid.major.y  = element_line(colour="grey12", size=0.5)) +
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold')) #+theme_ridges()#+scale_x_continuous(trans =log10_trans())
  
  ggsave(filename = paste( "/PDF_flow/CGW_", dam_list_title1[i_dam], ".png", sep = ""), plt_CGW_fpdf)
  
  plot_list_5[[i_dam]] = plt_CGW_fpdf
  
  # pdf bias corrected- actual flow

  plt_BC_fpdf<-ggplot(df_flow, aes(x = log(Bias_Corrected*AFD_to_CMS+1), y = Month, group=Month, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,15), expand = c(0,0)) +
    #scale_x_continuous(limits = c(0,quantile(df_flow$CDEC, .99)*AFD_to_CMS))+
    #scale_x_continuous(trans = "log10")+
    scale_x_continuous(limits = c(0,8), expand = c(0,0))+
    labs(title=paste("f. Streamflow PDF (BC) at", dam_list_title1[i_dam]), x = "Log10[Streamflow (Cubic meter per second)]", y="Month") +
    scale_fill_gradient(low = "grey", high = "darkorange") +theme_bw() +
    theme(panel.grid.major.y  = element_line(colour="grey12", size=0.5))+
    theme(legend.text = element_text(color = "black", size = 14)) +theme(legend.key.size = unit(1, "cm"))+
    theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = .5, vjust = 0, face = "plain"))+
    theme(title =element_text(size=12, face='bold')) #+theme_ridges() #+ scale_x_log10()
  
  ggsave(filename = paste( "/PDF_flow/BC_", dam_list_title1[i_dam], ".png", sep = ""), plt_BC_fpdf)
  
  plot_list_9[[i_dam]] = plt_BC_fpdf
  
  
  # PDF Mean Error NO groundwater correction
  
  plt_NGW_errpdf<-ggplot(df_error, aes(x = Error_No_GW_Correction, y = Month, group=Month, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,14), expand = c(0,0)) +
    #scale_x_continuous(limits = c(quantile(df_flow$Error_No_GW_Correction, .99)*AFD_to_CMS,quantile(df_flow$Error_No_GW_Correction, 1)*AFD_to_CMS))+
    labs(title=paste("g. Mean Error PDF (NGW) at", dam_list_title1[i_dam]), x = "Mean Error (%)", y="Month") +
    scale_fill_gradient(low = "grey", high = "darkred") +theme_bw() +
    theme(panel.grid.major.y  = element_line(colour="grey12", size=0.5)) 
  
  ggsave(filename = paste( "/PDF Error/NGW_", dam_list_title1[i_dam], ".png", sep = ""), plt_NGW_errpdf)
  
  plot_list_8[[i_dam]] = plt_NGW_errpdf
  
  # PDF Mean Error groundwater corrected
  
  plt_CGW_errpdf<-ggplot(df_error, aes(x = Error_GW_Corrected, y = Month, group=Month, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,14)) +
    #scale_x_continuous(limits = c(-200, 1000)) +# 
    #scale_x_continuous(limits = c(quantile(df_flow$Error_GW_Corrected, 0)*AFD_to_CMS,quantile(df_flow$Error_GW_Corrected, 1)*AFD_to_CMS))+
    labs(title=paste("h. Mean Error PDF (GW-Corrected) at", dam_list_title1[i_dam]), x = "Mean Error (%)", y="Month") +
    scale_fill_gradient(low = "grey", high = "darkcyan") +theme_bw() +
    theme(panel.grid.major.y  = element_line(colour="grey12", size=0.5)) 
  
  ggsave(filename = paste( "/PDF Error/CGW_", dam_list_title1[i_dam], ".png", sep = ""), plt_CGW_errpdf)
  
  plot_list_7[[i_dam]] = plt_CGW_errpdf
  
  # pdf error bias corrected flow

  plt_BC_errpdf<-ggplot(df_error, aes(x = Error_Bias_Corrected, y = Month, group=Month, fill = ..x..), alpha=0.4)+
    geom_density_ridges_gradient( scale = 3, size = 0.3) + 
    #scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist)+
    scale_y_continuous(breaks=seq(1,12,by = 1), limits = c(1,14)) +
    #scale_x_continuous(limits = c(quantile(df_flow$Error_Bias_Corrected, 0)*AFD_to_CMS,quantile(df_flow$Error_Bias_Corrected, 1)*AFD_to_CMS))+
    labs(title=paste("i. Mean Error PDF (Bias_Corrected) at", dam_list_title1[i_dam]), x = "Mean Error (%)", y="Month") +
    scale_fill_gradient(low = "grey", high = "darkorange") +theme_bw()
  
  ggsave(filename = paste( "/PDF Error/BC_", dam_list_title1[i_dam], ".png", sep = ""), plt_BC_errpdf)
  
  plot_list_10[[i_dam]] = plt_BC_errpdf  

  
  #  boxplot
  Emlt_M_df_flow<-melt(df_error, id.vars = c("Year"),  measure.vars = c("GW_Corrected", "NoGW_Correction", "Bias_Corrected"))
  
  plt_bxp_1<-ggplot(Emlt_M_df_flow, aes(x= variable, y=value, fill=variable)) + geom_boxplot() +facet_grid(~Year, scales = "free") +  theme_bw()+
    scale_fill_manual(values = c("darkcyan", "darkred", "darkorange")) +theme(axis.text.x=element_blank()) + theme(legend.title = element_blank())+
    labs(x="", y="Flow (Cubic meter per second)")
  ggsave(filename = paste( "/box_plot/NGW_", dam_list_title1[i_dam], ".png", sep = ""), plt_bxp_1)
  
  Error_mlt_M_df_flow<-melt(df_error, id.vars = "Year",  measure.vars = c("Error_GW_Corrected", "Error_No_GW_Correction", "Error_Bias_Corrected"))
 # MIN_Emlt_M_df_flow<-melt(df_error_min, id.vars = "Year",  measure.vars = c("Error_GW_Corrected", "Error_No_GW_Correction", "Error_Bias_Corrected"))
  
  plt_bxp_2<-ggplot(Error_mlt_M_df_flow, aes(x= variable, y=value, fill=variable)) + geom_boxplot() + theme_bw()+ #facet_grid(~Year, scales = "free") +  theme_bw()+
    scale_fill_manual(values = c("darkcyan", "darkred", "darkorange")) +theme(axis.text.x=element_blank()) + theme(legend.title = element_blank())+
    labs(x="", y="Flow (Cubic meter per second)")
  
  plt_error_pdf<-ggplot(Error_mlt_M_df_flow, aes(x= value,  fill=variable)) + geom_density(alpha=0.7) + theme_bw()+ #facet_grid(~Year, scales = "free") +  theme_bw()+
    scale_fill_manual(values = c("darkcyan", "darkred", "darkorange")) +#theme(axis.text.x=element_blank()) + theme(legend.title = element_blank())+
    labs(y="Probability density", x="Mean Error (%)") +theme(legend.position = "none")+
    labs(title=paste("g. PDF of mean error during top 99-th percentile flow at", dam_list_title1[i_dam])) 

  
  ggsave(filename = paste( "/box_plot/NGW_", dam_list_title1[i_dam], ".png", sep = ""), plt_bxp_1)
  
  
  #------- monthly Error
  
  M_df_flow=aggregate(df_flow, by = list(df_flow[,2]), mean)
  
  Emlt_M_df_flow<-melt(M_df_flow, id.vars = "Month", measure.vars = c( "Error_No_GW_Correction", "Error_GW_Corrected", "Error_Bias_Corrected"))
  plt_m_E<-ggplot(Emlt_M_df_flow, aes(x=Month, y=value, colour=variable)) +geom_line(size=1)+ geom_point(size=3) +
    theme_pubr()+  labs(title= paste("b. Monthly Mean Error at", dam_list_title1[i_dam]), x = "", y="Mean Error (%)") +
    scale_x_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist) + theme(legend.title = element_blank())+
    scale_color_manual(values = c( "darkred", "darkcyan", "darkorange")) + geom_hline(yintercept = 0 , color="grey12", linetype=2) +
    ylim(min(Emlt_M_df_flow$value), max(Emlt_M_df_flow$value))
  
  ggsave(filename = paste( "/monthly_error/", dam_list_title1[i_dam], ".png", sep = ""), plt_m_E)
  
  plot_list_3[[i_dam]] = plt_m_E


 combined_leg<-ggdraw() + draw_image("~/papers/Error_Proopagation_I/FinalFigures/Paper Figures/legend_to_R_v2.png", width = 0.6, height = 0.6, x = 0.2, y=0.25)

  
  comb_1<-ggarrange(ggarrange(plt_m_m, plt_y_m,
                              plt_cdec_fpdf, plt_NGW_fpdf, plt_CGW_fpdf, plt_BC_fpdf , nrow = 3, ncol = 2, common.legend = T),
                    ggarrange( plt_error_pdf, combined_leg, legend = "none", nrow =1, ncol = 2, widths = c(1.5,1)), nrow = 2, heights = c(3,1),
                    common.legend = T, legend = "none")
  
  ggsave(filename = paste( "/combined/", dam_list_title1[i_dam], ".png", sep = ""), comb_1, width = 12.9, height = 12)
  
  
  comb_2<-ggarrange(plt_m_m, plt_y_m, plt_cdec_fpdf, plt_NGW_fpdf, plt_CGW_fpdf, plt_BC_fpdf , nrow = 3, ncol = 2, common.legend = T)
  
  ggsave(filename = paste( "/combined/", dam_list_title1[i_dam], "_combined_2.png", sep = ""), comb_2, width = 12.9, height = 12)
  ggsave(filename = paste( "/combined/", dam_list_title1[i_dam], "_combined_2.svg", sep = ""), comb_2, device = "svg", width = 12.9, height = 12)
  
} # i_dam



Legend<-ggplot(mlt_Y_df_flow, aes(x=value, fill=variable)) +geom_density(size=1) +
  theme_pubr()+  labs(title= paste("b. Annual inflow to", dam_list_title1[i_dam]), x = "", y="Streamflow (Cubic meter per second)") +
  # scale_x_continuous(breaks=seq(1,12,by = 1), limits = c(1,12), labels = Mlist) +
  scale_fill_manual(values = c("blue", "darkred", "darkcyan", "darkorange")) + theme(legend.title = element_blank()) +
  theme(legend.title = element_blank())+theme(legend.position = "right")

ggsave(filename = paste( "/Legend.png", sep = ""), Legend)


####################################

plt_daily_comb<-ggarrange(plotlist =plot_list_0[1:8], ncol=2, nrow = 4, common.legend = T)
ggsave(filename = paste( "/streamflow_2010.png", sep = ""), plt_daily_comb, height = 14, width = 14)


write.table(df_streamflow, "/streamflow_input_CALFEWs.txt", quote = F, row.names = F)
