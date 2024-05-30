library(ggplot2)
library(pROC)
library(reshape2)
library(psych)
library(ggsignif)
library(pheatmap)
library(RColorBrewer)
library(tidyr)
library(dplyr)

##### Cas12

### ROC plot
Chari2017 <- read.csv('predictedType_Chari2017.csv')

roc_Chari2017 <- roc(true_type~Cas12_BiLSTM+Seq_deepCpf1+Cas12_transformer+
                    Cas12_attention+Cas12_simpleRNN, 
                data=Chari2017,
                aur=TRUE,
                ci=TRUE,
                smooth=F)


ggroc(roc_Chari2017, legacy.axes = TRUE, alpha = 0.5, size = 1.5)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="darkgrey", linetype=4)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))+
  ggtitle('ROC in Chari 2017')+
  ggsci::scale_color_lancet()+
  annotate("text",x=0.75,y=0.1,size = 5, label=paste("AUC of Cas12_SimpleRNN:", round(roc_Chari2017$Cas12_simpleRNN$auc,4)))+
  annotate("text",x=0.75,y=0.2,size = 5,label=paste("AUC of Cas12_Attention:", round(roc_Chari2017$Cas12_attention$auc,4)))+
  annotate("text",x=0.75,y=0.3,size = 5,label=paste("AUC of Cas12_Transformer:", round(roc_Chari2017$Cas12_transformer$auc,4)))+
  annotate("text",x=0.75,y=0.4,size = 5,label=paste("AUC of Seq_deepCpf1:", round(roc_Chari2017$Seq_deepCpf1$auc,4)))+
  annotate("text",x=0.75,y=0.5,size = 5,label=paste("AUC of Cas12_BiLSTM:", round(roc_Chari2017$Cas12_BiLSTM$auc,4)))



### bar plot
spearman <- data.frame('dataset'=c('HT 1-2', 'HT 2', 'HT 3', 'HEK-lenti', 
                                   'HEK-plasmid', 'HCT-plasmid', 'Kleinstiver 2016', 'Chari 2017'),
                       'Cas12_BiLSTM'=c(0.7711, 0.7587, 0.5714, 0.5883, 0.7286, 0.5671, 0.6653, 0.6925),
                       'Seq_deepCpf1'=c(0.7663, 0.7521, 0.5566, 0.5574, 0.6956, 0.5578, 0.6755, 0.6904),
                       'Cas12_Transformer'=c(0.7384, 0.7299, 0.4787, 0.5639, 0.7201, 0.4643, 0.5206, 0.5377),
                       'Cas12_Attention'=c(0.7383, 0.7282, 0.4664, 0.5428, 0.6949, 0.4520, 0.7230, 0.6120),
                       'Cas12_SimpleRNN'=c(0.7336, 0.7344, 0.5016, 0.5123, 0.7073, 0.5157, 0.5715, 0.6698))
spearman$dataset <- factor(spearman$dataset, levels = c('HT 1-2', 'HT 2', 'HT 3', 'HEK-lenti', 
                                                  'HEK-plasmid', 'HCT-plasmid', 'Kleinstiver 2016', 'Chari 2017'))
spearman <- melt(spearman,variable.name="model",value.name = "Spearman.cor")


# Steiger's test
#cor_xy <- cor(Chari2017$y_test, Chari2017$Cas12_BiLSTM, method = "spearman")
#cor_xz <- cor(Chari2017$y_test, Chari2017$Seq_deepCpf1, method = "spearman")

#corTest_Chari2017 <- r.test(n=nrow(Chari2017), r12=cor_xy, r13=cor_xz, 
#                       r23=cor(Chari2017$Cas12_BiLSTM, Chari2017$Seq_deepCpf1, method = "spearman"))

ggplot(spearman, aes(x = dataset,y = Spearman.cor,fill = model))+
  geom_bar(stat ="identity",width = 0.8,position = "dodge", alpha = 0.4)+     
  labs(x = "",y = "Spearman correlation")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))+
  ggsci::scale_fill_lancet()



# heatmap
accuracy <- data.frame('model'=c('Seq_deepCpf1','Cas12_SimpleRNN', 'Cas12_BiLSTM','Cas12_Attention', 
                                 'Cas12_Transformer'),
                       'HT 1-2'=c(0.7879, 0.7771, 0.8003, 0.7678, 0.7833),
                       'HT 2'=c(0.7779, 0.7685, 0.7773, 0.7631, 0.7631),
                       'HT 3'=c(0.7282, 0.6946, 0.7298, 0.6978, 0.6978),
                       'HEK-lenti'=c(0.7297, 0.6892, 0.7162, 0.6757, 0.6757),
                       'HEK-plasmid'=c(0.7455, 0.8182, 0.8182, 0.7455, 0.7818),
                       'HCT-plasmid'=c(0.6364, 0.6970, 0.7273, 0.6970, 0.7273),
                       'Kleinstiver 2016'=c(0.8182, 0.8182, 0.9091, 0.9091, 0.8182),
                       'Chari 2017'=c(0.7778, 0.7778, 0.7778,0.6667, 0.6667))
rownames(accuracy) <- accuracy$model
accuracy <- accuracy[,-1]
pheatmap(accuracy, scale = "none", display_numbers = TRUE, 
         number_format = "%.4f", main = 'Accuracy',
         cluster_row = T, cluster_col = F, angle_col = 45,
         color=brewer.pal(9,"Blues"))



f1 <- data.frame('model'=c('Seq_deepCpf1','Cas12_SimpleRNN', 'Cas12_BiLSTM','Cas12_Attention', 
                           'Cas12_Transformer'),
                       'HT 1-2'=c(0.7350, 0.7215, 0.7505, 0.7099, 0.7292),
                       'HT 2'=c(0.7224, 0.7105, 0.7215, 0.7038, 0.7038),
                       'HT 3'=c(0.6600, 0.6180, 0.6620, 0.6220, 0.6220),
                       'HEK-lenti'=c(0.6610, 0.6102, 0.6441, 0.5932, 0.5932),
                       'HEK-plasmid'=c(0.6818, 0.7727, 0.7727, 0.6818, 0.7273),
                       'HCT-plasmid'=c(0.5385, 0.6154, 0.6538, 0.6154, 0.6538),
                       'Kleinstiver 2016'=c(0.7778, 0.7778, 0.8889, 0.8889, 0.7778),
                       'Chari 2017'=c(0.7143, 0.7143, 0.7143, 0.5714, 0.5714))

rownames(f1) <- f1$model
f1 <- f1[,-1]
pheatmap(f1, scale = "none", display_numbers = TRUE, 
         number_format = "%.4f", main = "F1 score",
         cluster_row = T, cluster_col = F, angle_col = 45,
         color=brewer.pal(9,"Purples"))





##### Cas9
### bar plot
cas9_test <- data.frame('metrics'=c('Spearman', 'Accuracy', 'F1 Score', 'Precision','Recall',
                                   'ROC AUC', 'PR AUC'),
                      'Cas9_Attention' = c(0.7711,0.7897,0.7373,0.7373,0.7373,0.8817,0.8238),
                      'Cas9_SimpleRNN' = c(0.7694,0.7823,0.7281,0.7281,0.7281,0.8807,0.8121),
                      'Cas9_BiLSTM' = c(0.7458,0.7897,0.7373,0.7373,0.7373,0.8682,0.8107),
                      'Cas9_Transformer' = c(0.7354,0.7675,0.7097,0.7097,0.7097,0.8565,0.7781),
                      'DeepCRISPR' = c(-0.0219,0.5277,0.4101,0.4101,0.4101,0.5016,0.4193))
cas9_test$metrics <- factor(cas9_test$metrics, levels = c('Spearman', 'Accuracy', 'F1 Score', 'Precision', 'Recall',
                                                        'ROC AUC', 'PR AUC'))
cas9_test <- melt(cas9_test,variable.name="model",value.name = "Spearman.cor")

ggplot(cas9_test, aes(x = metrics,y = Spearman.cor,fill = model))+
  geom_bar(stat ="identity",width = 0.8,position = "dodge", alpha = 0.6)+     
  labs(x = "",y = "Value of metrics")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))+
  ggsci::scale_fill_npg()



# heatmap
data <- data.frame(
  Metrics = rep(c("Spearman", "Accuracy", "F1 Score", "Precision", "Recall", "ROC AUC", "PR AUC"), each = 5),
  Models = rep(c("DeepCRISPR", "Cas9_SimpleRNN", "Cas9_BiLSTM", "Cas9_Attention", "Cas9_Transformer"), 7),
  Kim_2020 = c(0.0211, 0.4405, 0.4474, 0.4545, 0.4470, 0.5282, 0.6474, 0.6468, 0.6495, 0.6393,
               0.4102, 0.5592, 0.5585, 0.5618, 0.5492, 0.4102, 0.5592, 0.5585, 0.5618, 0.5492,
               0.4102, 0.5592, 0.5585, 0.5618, 0.5492, 0.5091, 0.6917, 0.6940, 0.6979, 0.6914,
               0.4040, 0.5853, 0.5907, 0.6008, 0.5911),
  Wang_2019 = c(0.0747, 0.6141, 0.6355, 0.6129, 0.5613, 0.5467, 0.7180, 0.7286, 0.7174, 0.6938,
                0.4334, 0.6475, 0.6608, 0.6467, 0.6173, 0.4334, 0.6475, 0.6608, 0.6467, 0.6173,
                0.4334, 0.6475, 0.6608, 0.6467, 0.6173, 0.5355, 0.7923, 0.8048, 0.7905, 0.7627,
                0.4259, 0.6927, 0.7094, 0.6830, 0.6543),
  Labuhn_2017 = c(0.0565, 0.2507, 0.2156, 0.2530, 0.2520, 0.5718, 0.6376, 0.6094, 0.6235, 0.6235,
                  0.4647, 0.5471, 0.5118, 0.5294, 0.5294, 0.4647, 0.5471, 0.5118, 0.5294, 0.5294,
                  0.4647, 0.5471, 0.5118, 0.5294, 0.5294, 0.5453, 0.6376, 0.6209, 0.6359, 0.6232,
                  0.4436, 0.4997, 0.5048, 0.5139, 0.4961),
  Chuai_HCT116 = c(0.0426, 0.3867, 0.3836, 0.3861, 0.3633, 0.5282, 0.6494, 0.6485, 0.6471, 0.6372,
                   0.4104, 0.5619, 0.5607, 0.5590, 0.5466, 0.4104, 0.5619, 0.5607, 0.5590, 0.5466,
                   0.4104, 0.5619, 0.5607, 0.5590, 0.5466, 0.5152, 0.6976, 0.6990, 0.7005, 0.6866,
                   0.4142, 0.5589, 0.5609, 0.5641, 0.5502),
  Chuai_HELA = c(0.0498, 0.3353, 0.3291, 0.3302, 0.3048, 0.5386, 0.6393, 0.6378, 0.6356, 0.6302,
                 0.4231, 0.5491, 0.5472, 0.5444, 0.5377, 0.4231, 0.5491, 0.5472, 0.5444, 0.5377,
                 0.4231, 0.5491, 0.5472, 0.5444, 0.5377, 0.5254, 0.6819, 0.6781, 0.6803, 0.6671,
                 0.4197, 0.5415, 0.5400, 0.5409, 0.5280),
  Chuai_HL60 = c(-0.0038, 0.4541, 0.4524, 0.4588, 0.4428, 0.5183, 0.6532, 0.6532, 0.6493, 0.6541,
                 0.3976, 0.5663, 0.5663, 0.5614, 0.5675, 0.3976, 0.5663, 0.5663, 0.5614, 0.5675,
                 0.3976, 0.5663, 0.5663, 0.5614, 0.5675, 0.4934, 0.7146, 0.7143, 0.7175, 0.7081,
                 0.4023, 0.5968, 0.5983, 0.5986, 0.5815)
)



data_long <- melt(data, id.vars = c("Metrics", "Models"), variable.name = "Dataset", value.name = "Value")

ggplot(data_long, aes(x = Dataset, y = Models, fill = Value)) +
  geom_tile() +
  scale_fill_gradientn(colors = brewer.pal(5, "BrBG"))+
  facet_grid(rows = vars(Metrics)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),  # Metric names in facet labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(x = "Dataset", y = "Model", fill = "Score")







##### explore HT 1-1
### log2FC bar plot
cas12_logfc = data.frame('position'=c('1','2','3','4','5','6','7','8','9','10',
                                      '11','12','13','14','15','16','17','18','19','20'),
                         'A'=c(0.8045028057266125,-0.05365531213159671,0.1783910942621539,0.09405397396983578,
                               0.02965327401437608,0.9140909500047414,0.4587300893062414,0.269239555360502,
                               0.41197769952978575,0.3269461696539868,0.28160651187021557,0.26067126846531297,
                               0.29218075149331035,0.35550979789211445,0.2780565964514468,0.31274661831924566,
                               0.2489965724335111,0.2518258419229664,0.40930112972969945,0.13435238662336282),
                         'C'=c(-0.1294199534968123,0.3205362037606905,-0.026069378636256527,-0.17435336066853263,
                               0.23524144623845522,0.006906164652300215,0.19072736525687597,0.43924805287071855,
                               0.30741300575075436,0.2707700546133992,0.23350706852853553,0.17123833338634498,
                               0.2368523560079781,0.2475412393667675,0.17190265088275472,0.10328780841202195,
                               0.21938492589481715,0.18896003113454005,0.03114338290699279,0.4552811312247477),
                         'G'=c(0.6112737778370034,0.180890054882205,0.17582171296316593,0.5089044227430662,
                               0.3282434946195115,0.23437564246640558,0.2576678960839784,0.0068618205022590315,
                               -0.12524010408410624,0.04495778220636927,-0.09216984392786706,0.0682214912086876,
                               0.10867781261577164,-0.025763096065081786,0.09279409567185924,-0.12943181742376264,
                               -0.38918293171314744,-0.08913359423485291,-0.17567967616811953,-0.5849625007211563),
                         'T'=c(-0.7661775153826935,-0.44766360005374123,-0.21827631286421606,-0.2810328045844243,
                               -0.5051301005951944,-0.7697359281148811,-0.6168717486759001,-0.6139785745813254,
                               -0.4344915116886584,-0.4383549934073747,-0.2943837601914947,-0.36791430016154264,
                               -0.46434982642365275,-0.4202498225156664,-0.39627944099148354,-0.18017747342950982,
                               -0.05628913084654387,-0.2688167584278,-0.1282958318375452,-0.10358065952539136))

cas12_logfc$position <- factor(cas12_logfc$position, levels = c('1','2','3','4','5','6','7','8','9','10',
                                                                '11','12','13','14','15','16','17','18','19','20'))
cas12_logfc <- melt(cas12_logfc,variable.name="Nucleotide",value.name = "Log2FC")

ggplot(cas12_logfc, aes(x = position,y = Log2FC,fill = Nucleotide))+
  geom_bar(stat ="identity",width = 0.8,position = "dodge", alpha = 0.6)+     
  labs(x = "Position",y = "Log2 Fold Change",title = 'Cas12 training set: HT 1-1')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  ggsci::scale_fill_aaas()



cas9_logfc = data.frame('position'=c('1','2','3','4','5','6','7','8','9','10',
                                      '11','12','13','14','15','16','17','18','19','20'),
                         'A'=c(0.061587798222831655,0.02766897214395179,0.24643789530309892,-0.21640154707873288,
                               -0.12910760262887844,0.0037816405424267543,-0.14642703907337923,-0.017417053077409404,
                               0.22954086050278447,0.39949664838306564,0.4188462381631421,0.3901628306398146,
                               -0.3392278956031929,0.3694335321054217,0.4517391804916417,0.30268576716794515,
                               0.5039884221885921,-0.6023731842699755,-0.2763108342594679,0.3011362932823108),
                         'C'=c(-0.43451762289714946,0.2307802068291671,-0.32228737643345756,-0.029554894245378906,
                               0.2735990947392459,0.2753047025877901,0.25260703438345566,0.13988617506092896,
                               0.0232162832657781,0.2479275134435855,0.029849927746560145,0.08891455779434178,
                               0.42829939868776634,-0.023756123252608237,-0.10131348651881757,0.3198705308957824,
                               -0.009045139603006954,1.339341025973052,0.7146595390214061,-0.7564642556735723),
                         'G'=c(0.6042907616555778,0.17148046794435368,0.19420334456469698,0.6377499150805914,
                               0.07321439539495479,0.4065058656577269,0.2575314301564069,0.35701654630911844,
                               -0.04644485892321548,-0.10525215469386763,0.24874374828340023,-0.0748085876485376,
                               0.3260071736746321,-0.3952846519731918,0.048129170874279405,0.09930333680173876,
                               0.33244596680136157,0.154187340706926,0.6272423042137203,1.9068905956085185),
                         'T'=c(-0.22408077148921776,-0.4933446794207312,-0.05919579250790008,-0.45216152529838366,
                               -0.25891829736156186,-0.6254080264395658,-0.4625658693614297,-0.5259419869089558,
                               -0.19723234743804768,-0.5721241953919168,-0.7487678957835703,-0.4109331009461073,
                               -0.5849625007211563,0.06413033741971556,-0.3998029855836834,-0.8894416248523016,
                               -0.791569252538136,-1.1120975495999457,-1.122578959194921,-1.5075461267636532))

cas9_logfc$position <- factor(cas9_logfc$position, levels = c('1','2','3','4','5','6','7','8','9','10',
                                                                '11','12','13','14','15','16','17','18','19','20'))
cas9_logfc <- melt(cas9_logfc,variable.name="Nucleotide",value.name = "Log2FC")

ggplot(cas9_logfc, aes(x = position,y = Log2FC,fill = Nucleotide))+
  geom_bar(stat ="identity",width = 0.8,position = "dodge", alpha = 0.6)+     
  labs(x = "Position",y = "Log2 Fold Change",title = 'Cas9 training set: Kim 2019 train')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  ggsci::scale_fill_aaas()





##### MDA-MB-231
mdamb231 <- data.frame('level'=c('60%','70%','80%','90%'),
                       'efficacy'=c(0.5424,0.5922,0.6890,0.8224),
                       'log2FC_repA'=c(0.5758,0.6429,0.7091,0.7744),
                       'log2FC_repB'=c(0.5723,0.6394,0.7060,0.7700))

mdamb231 <- melt(mdamb231,variable.name="Label",value.name = "accuracy")

ggplot(mdamb231, aes(x = level, y = accuracy, group = Label, color = Label)) +
  geom_line(linewidth=1.5, alpha=0.6) +  
  geom_point(size = 4) +
  labs(x = "Percentile rank",  
       y = "Accuracy") +  
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  ggsci::scale_color_jama()








