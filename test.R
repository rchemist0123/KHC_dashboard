temp <- fread('DEMO.csv')
names(temp)
temp[,`:=`(
  DRK_COMBINED = ifelse(drk_freq==0,0,
                        ifelse((SEX=='남성' & drk_freq>=4 & drk_amt>=7)|
                                 (SEX=='여성' & drk_freq>=4 & drk_amt>=5),2,1)) %>% as.factor,
  SMK_COMBINED = ifelse(smk=='한 번도 흡연한 적 없음',0,
                        ifelse(smk=='현재 흡연 중',2,1)) %>% as.factor,
  METs = 2.9*30*(Q_PA_WALK) +
    4*30*(Q_PA_MD) +
    7*20*(Q_PA_VD),
  eGFR = ifelse(SEX=='남성',186.3*(G1E_CRTN^-1.154)*AGE^-0.203,
                186.3*(G1E_CRTN^-1.154)*AGE^-0.203*0.742)
)][,`:=`(
  SMK_COMBINED1 = ifelse(smk=='과거 흡연했으나 현재는 안함',1,0),
  SMK_COMBINED2 = ifelse(smk=='현재 흡연 중',1,0),
  DRK_COMBINED1 = ifelse(DRK_COMBINED==1,1,0),
  DRK_COMBINED2 = ifelse(DRK_COMBINED==2,1,0),
  intercept = 1,
  `METs^2` = I(METs^2)
)]
fwrite(temp,'coef.csv')


# coefficients -----------------------------------------------------------

coef_dt <- data.table(
  features = c("intercept", "AGE", "G1E_TOT_CHOL", "G1E_HDL", "G1E_LDL", "G1E_TG", "G1E_FBS", "G1E_HGB", "G1E_SGOT", "G1E_SGPT", "G1E_GGT", "G1E_BMI", "G1E_WSTC", "G1E_BP_SYS", "G1E_BP_DIA", "EGFR", "SMK_COMBINED1", "SMK_COMBINED2", "DRK_CON", "METS", "Q_FHX_STK1", "Q_FHX_HTN1", "Q_FHX_HTDZ1", "Q_FHX_DM1", "METS^2", "DRK_CON^2"),
  cancer_female = c(-10.12457000, 0.05499333, 0.00543797, -0.00568788, -0.00387045, 0.00011046, -0.00267650, 0.03483473, -0.00839340, 0.00919483, -0.00393927, -0.02903583, 0.01108761, 0.00528403, 0.00197366, 0.00110149, 0.32444240, 0.64509830, 0.03823041, -0.00021911, -0.05226916, 0.05872163, -0.34343990, 0.02171791, -0.00000008, -0.00262891),
  cancer_male = c(-10.48514000, 0.06757215, -0.00050816, 0.00129883, -0.00136791, 0.00113866, 0.00197349, 0.00553421, -0.00742955, 0.00062856, 0.00076736, -0.03696681, 0.01362586, 0.00356091, 0.00717530, 0.00261176, 0.11697020, 0.08052313, 0.03741539, -0.00004039, -0.08220559, -0.02474433, -0.02439255, -0.09270638, -0.00000002, -0.00087530),
  mace_female = c(-8.315452, 0.06948085, 0.00052745, -0.00376614, -0.00031809, 0.00057469, 0.00433205, -0.01714706, -0.00204853, -0.00088569, 0.00337814, 0.00562825, 0.01229225, 0.00577939, 0.00071381, -0.00315217, 0.2822388, 0.3578083, -0.01023213, -0.00016524, 0.1695806, 0.07217782, 0.2454097, 0.02155139, 5e-08, 0.0005027),
  mace_male = c(-8.812422, 0.07082221, 0.00164675, -0.00769624, 0.00067051, 0.0001248, 0.00390164, -0.00231403, 0.0014715, -0.00159856, 0.00103576, 0.00080925, 0.01399629, 0.00827267, -0.00115531, -0.00106256, 0.03947018, 0.3594965, -0.01686932, -0.00020358, 0.1451087, 0.1452406, 0.288875, 0.08745326, 8e-08, 0.00045295),
  death_female = c(-8.744776, 0.113374, -0.00013708, -0.00307692, -0.00226382, -0.00011347, 0.00666073, -0.1596548, 0.01187807, -0.00770291, 0.00722241, -0.05802016, 0.02138791, 0.00065942, 0.00455154, -0.00318986, 0.4915273, 0.8202815, -0.00492613, -0.00026506, -0.1032132, -0.08450503, 0.05717994, -0.04648058, 9e-08, 0.00061417),
  death_male = c(-6.375576, 0.1060652, -0.0013047, -0.00445088, -0.00102801, -0.00038485, 0.0056876, -0.1788216, 0.01714753, -0.00440366, 0.00312303, -0.1099215, 0.02267535, 0.00592475, -0.00061692, 1.006e-05, 0.1009108, 0.6230138, -0.03188874, -0.00039209, 0.00240772, -0.147858, -0.08358185, -0.03407394, 1.4e-07, 0.00095866),
  t2dm_female = c(-15.29293000, 0.03245425, -0.00059291, -0.00834482, -0.00263146, 0.00225816, 0.07741242, -0.00454657, -0.01191026, 0.01904347, 0.00420474, 0.07075631, 0.01992073, 0.00648368, -0.00264542, 0.00173298, 0.00906235, 0.24207010, -0.02732339, 0.00008570, 0.01984814, -0.08518715, -0.24039490, 0.74494350, -0.00000002, 0.00097971),
  t2dm_male = c(-13.36997000, 0.03169934, 0.00082770, -0.00936272, -0.00309144, 0.00070264, 0.06416687, -0.01858025, -0.00047362, 0.00924526, 0.00237248, 0.06350337, 0.01749420, 0.00332683, -0.00017818, 0.00306074, 0.09162591, 0.34729040, -0.01847144, -0.00001680, -0.03432723, -0.10291020, -0.06742553, 0.65111730, 0.00000005, 0.00046902),
  htn_female = c(-12.86864, 0.01341833, 0.00118652, -0.00454883, -0.00191006, 0.00079797, 0.0467524, -0.01071879, -0.00379397, 0.00871227, 0.00191992, 0.04987329, 0.00721437, 0.01668999, 0.01842001, 0.0021447, -0.2427087, 0.1638078, 0.00599669, 9.02e-06, 0.03915896, -0.0333791, -0.1248565, 0.3625788, 3e-08, -0.00016898),
  htn_male = c(-12.66558, 0.02484005, 0.0029624, -0.00840732, -0.00278384, 0.00016813, 0.0381257, 0.02061819, 0.00024451, 0.00435978, 0.00258841, 0.05148543, 0.0072796, 0.01489164, 0.01682554, 0.00169124, 0.1024446, 0.2497767, 0.00034591, -3.651e-05, -0.00950487, 0.08726727, -0.1450262, 0.3564549, 5e-08, 0.00014287),
  metsyn_female = c(-11.18966, 0.02052396, 0.00593441, -0.01509056, -0.00239306, 0.00575791, 0.01430328, 0.01546343, -0.00835866, 0.00883048, 0.00452926, 0.0656829, 0.00980908, 0.01409937, 0.01348156, -0.0012589, -0.1257431, 2.847e-05, 0.03389271, 2.852e-05, 0.0749194, 0.0952325, -0.01405246, 0.1038374, 0.00000005, -0.00073312),
  metsyn_male = cc(-10.15574, 0.00244828, 0.00836285, -0.02887446, -0.00697796, 0.00346963, 0.00993572, 0.00600742, -0.00150814, 0.00265376, 0.00431945, 0.1219352, 0.02864679, 0.00997531, 0.01106051, 0.00020249, 0.01130009, 0.1598978, 0.020863, 6.548e-05, -0.00789291, 0.03480838, -0.08130582, 0.1259316, -1e-08, -0.00045357),
  illness_female = c(-6.79574000, 0.06241202, 0.00014747, -0.00213748, -0.00010382, 0.00069495, 0.00742284, -0.00782944, 0.00465814, 0.00031588, 0.00230979, 0.02337855, 0.00961038, 0.00428499, 0.00359803, 0.00049997, 0.20045850, 0.16857620, 0.00137018, -0.00017219, 0.15104370, 0.13051850, -0.01049498, 0.00810097, 0.00000007, 0.00002645),
  illness_male = c(-7.918751, 0.07221236, 0.00149893, -0.00510851, -0.00118427, -8.596e-05, 0.00843669, -0.00097469, 0.00770072, 0.00035951, 0.00153262, 0.01484445, 0.01113922, 0.00558471, 0.00497714, 0.00084493, -0.01554061, 0.08595671, -0.01090196, -0.00012964, 0.0721467, 0.2057304, 0.1481436, 0.06728764, 6e-08, 0.00034252)
)

for(i in c('cancer','mace','death','t2dm','htn','metsyn','illness')){
  temp <- coef_dt[,.SD,.SDcols=patterns(i)]
  temp <- transpose(temp[,1:2])
  names(temp) <- coef_dt$features
  filename <- paste0(i,'.csv')
  fwrite(temp, paste0('shiny/KHC_HRA/coefs/',filename))
}

# HR 계산---------------------------------------------------------------------


hr_table_male <- data.table(
  disease=rep(c('t2dm','mace','brain','cancer','htn'),each=4),
  age = rep(c(40,50,60,90),5),
  hr = c(1.201, 1.377, 1.562, 1.220, 
         1.124, 1.136, 1.106, 1.068,
         1.107, 1.135, 1.038, 1.054,
         1.195, 1.082, 1.116, 1.038,
         1.323, 1.339, 1.432, 1.148
         )
)
hr_table_female <- data.table(
  disease=rep(c('t2dm','mace','brain','cancer','htn'),each=4),
  age = rep(c(40,50,60,90),5),
  hr = c(1.086, 1.295, 1.549, 1.260, 
         1.145, 1.225, 1.089, 1.054,
         0.902, 1.194, 1.110, 0.993,
         1.379, 1.213, 1.061, 1.003,
         1.182, 1.418, 1.402, 1.153
  )
)
hr_table_female[hr_table_female[age>=59,.I[1L],by=disease]$V1]

getwd()
fwrite(hr_table_female,'shiny/KHC_HRA/hr/2_hr_table.csv')
fwrite(hr_table_male,'shiny/KHC_HRA/hr/1_hr_table.csv')


temp <- data.table(
  var=c('나이'),
  variable=100
)
levels(temp$var) <- c(levels(temp$var),'')
temp$var
temp
ggplot(temp,aes(x=var,y=variable))+
  geom_col(fill='lightblue',color='black')+
  scale_x_discrete(expand = expansion(add=2))
p  
geom_segment(aes(x=0.9,y=20,xend=1.1, yend=20), color='blue')+
  geom_segment(aes(x=0.9, y=50, xend=1.1, yend=50),color='red',size=1)


# 원격서버 전연령 5000명 Tukey transform 점수 계산 ---------------------------------------------

temp <- fread('shiny/KHC_HRA/score_raw/remote/total2.csv')
temp[,sex := rep(rep(c(2,1), each=5000),7)]
setnames(temp, 'duration','id')
target <- c('cancer','mace','death','illness','t2dm','htn','metsyn')
for(t in target){
  for(s in 1:2){
    sex <- ifelse(s ==1,'m','f')
    message('Sex: ',s ,' Disease: ', t)
    subset <- temp[sex ==s & disease == t]; print(nrow(subset))
    subset[,pred_transform := scale(transformTukey(pred,quiet = T))]
    min <- subset[,min(pred_transform)]
    subset[,score_transform := pred_transform-min(pred_transform)]
    max <- subset[,max(score_transform)]
    multi <- 1000/max;
    subset[,score_transform := 1000-multi*(score_transform)]
    setorder(subset,-score_transform)
    filename <- paste0(c(sex,'all',t,'score.csv'),collapse = '_')
    path <- 'shiny/KHC_HRA/scores/remote/'
    fwrite(subset, paste0(path,filename))
  }
}

# 원격서버 질병별로 Tukey transformation score 계산---------------------------------------------------------------
# by age & sex 

temp2 <- fread('shiny/KHC_HRA/score_raw/remote/total.csv')

temp2[,sex := rep(rep(c(2,1),each=10000),7)]

target <- c('cancer','mace','death','illness','t2dm','htn','metsyn')
ages <- seq(30,70,10)
for(t in target){
  for(s in 1:2){
    for(a in ages){
      sex <- ifelse(s==1,'m','f')
      message('Sex: ',s ,' Disease: ', t, ' Age: ', a)
      subset <- temp2[sex==s & disease==t & AGE_GROUP==a]
      subset[,pred_transform := scale(transformTukey(pred,quiet = T))]
      min <- subset[,min(pred_transform)]
      subset[,score_transform := pred_transform-min(pred_transform)]
      max <- subset[,max(score_transform)]
      multi <- 1000/max ;
      subset[,score_transform := 1000-multi*(score_transform)]
      setorder(subset,-score_transform)
      filename <- paste0(c(sex,a,t,'score.csv'),collapse = '_')
      fwrite(subset,paste0('shiny/KHC_HRA/scores/remote/',filename))
    }
  }
}
temp

# fread(paste0('shiny/KHC_HRA/score_raw/',files[1]))
# for(i in files){
#   temp <- fread(paste0(getwd(),'/shiny/KHC_HRA/score_raw/',i))
#   target <- gsub('_score_2000.csv','',i)
#   for(s in 1:2){
#     sex <- ifelse(s==1,'m','f')
#     for(a in seq(30,70,10)){
#       message('Sex: ',s , ' Age: ', a, ' Disease: ', substr(i,1,4))
#       subset <- temp[sex==s & age==a,.(pred,
#                                        score_raw = health_score_raw,
#                                        rank=health_score_rank,
#                                        age=age)]
#       subset[,pred_transform := scale(transformTukey(pred,quiet = T))]
#       min <- subset[,min(pred_transform)]
#       subset[,score_transform := pred_transform-min(pred_transform)]
#       max <- subset[,max(score_transform)]
#       multi <- 1000/max ;
#       subset[,score_transform := 1000-multi*(score_transform)]
#       setorder(subset,-score_transform)
#       # subset[,score_transform := 1000-125*(scale(transformTukey(pred,quiet = T))+4L)]
#       # subset[,score_transform := fifelse(score_transform>1000,1000,
#       #                                    fifelse(score_transform<0,0,score_transform))]
#       filename <- paste0(c(sex,a,target,'score.csv'),collapse = '_')
#       fwrite(subset,paste0('shiny/KHC_HRA/scores/remote/',filename))
#     }
#   }
# }

# total score 만들기 -------------------------------------------------
files2 <- list.files('shiny/KHC_HRA/scores/remote/',pattern='_score.csv')
fread(paste0('shiny/KHC_HRA/scores/remote/',files2[1]))
files2
# files_for_total <- files2[files2 %like% 'mace|cancer|death|all']
                            # !files2 %like% 'all']
files_for_total
age <- seq(30,70,10)
sex <- c('m','f')
file_dir <- paste0('shiny/KHC_HRA/scores/remote/')
for(i in age){
  for(j in sex){
    message('SEX: ',j,' AGE: ',i)
    mace <- fread(paste0(file_dir,j,'_',i,'_','mace_score.csv'))
    death <- fread(paste0(file_dir,j,'_',i,'_','death_score.csv'))
    cancer <- fread(paste0(file_dir,j,'_',i,'_','cancer_score.csv'))
    total_score <- data.table(score_raw= (0.69*mace$score_raw + death$score_raw + 0.71*cancer$score_raw)/2.4,
                              score_transform= (0.69*mace$score_transform + death$score_transform + 0.71*cancer$score_transform)/2.4,
                              age=age)
    total_filename <- paste0(file_dir, j,'_',i,'_total_score.csv')
    fwrite(total_score, total_filename)
  }
}

for(j in sex){
  message('SEX: ',j)
  mace <- fread(paste0(file_dir,j,'_','all','_','mace_score.csv'))
  death <- fread(paste0(file_dir,j,'_','all','_','death_score.csv'))
  cancer <- fread(paste0(file_dir,j,'_','all','_','cancer_score.csv'))
  print(nrow(mace))
  print(nrow(death))
  print(nrow(cancer))
  total_score <- data.table(score_raw= (0.69*mace$health_score_raw + death$health_score_raw + 0.71*cancer$health_score_raw)/2.4,
                            score_transform= (0.69*mace$score_transform + death$score_transform + 0.71*cancer$score_transform)/2.4
  )
  # total_filename <- paste0(file_dir, j,'_','all','_total_score.csv')
  # fwrite(total_score, total_filename)
}
j
mace <- fread(paste0(file_dir,j,'_','all','_','mace_score.csv'))
death <- fread(paste0(file_dir,j,'_','all','_','death_score.csv'))
cancer <- fread(paste0(file_dir,j,'_','all','_','cancer_score.csv'))
j <- 'm'
mace
death
cancer
# T640 5000명 Tukey transform 점수 계산 ---------------------------------------------------------------------
calcPred <- function(db, target){
  coeflist <- list.files('shiny/KHC_HRA/coefs/',)
  coef_file <- coeflist[coeflist %like% target] ;
  coef <- fread(paste0('shiny/KHC_HRA/coefs/',coef_file));
  vars <- colnames(coef)
  input_mat <- as.matrix(db[,..vars]);
  probs <- c()
  for(i in 1:nrow(db)){
    sex <- db[i][,SEX]
    if(sex==1) coef_mat <- as.matrix(coef[2,])
    else  coef_mat <- as.matrix(coef[1,])
    
    prob_raw <- as.numeric(input_mat[i,] %*% t(coef_mat))
    prob <- 1/(1+exp(-prob_raw))
    probs <- c(probs,prob)
  }  
  target_pred <- paste0(target,'_pred')
  db[,eval(target_pred):=probs]
  db[,score_raw := 1000*(1-get(target_pred))]
  return(db[,.(SEX,
               AGE= (AGE%/%10)*10, 
               pred = get(target_pred),
               score_raw)])
}
getwd()
require(rcompanion)
m[,.N,by=AGE]
m <- fread('shiny/KHC_HRA/score_raw/t640/t640_5000_male_sample.csv')
f <- fread('shiny/KHC_HRA/score_raw/t640/t640_5000_female_sample.csv')
target <- c('cancer','mace','death','illness','t2dm','htn','metsyn')
ages <- seq(30,70,10)
for(i in c('m','f')){
  for(j in target){
      dfname <- paste0(c(i,j,'pred_df'), collapse = '_')
      assign(x = dfname,
             value = calcPred(get(i), j)) # 각 질병별로 계산하기
      temp <- copy(get(dfname))
      temp[,pred_transform := scale(transformTukey(pred,quiet = T))]
      min <- temp[,min(pred_transform)]
      temp[,score_transform := pred_transform-min(pred_transform)]
      max <- temp[,max(score_transform)]
      multi <- 1000/max ;
      temp[,score_transform := 1000-multi*(score_transform)]
      setorder(temp,-score_transform)
      filename <- paste0(c(i,'all',j,'score.csv'),collapse = '_')
      path <- 'shiny/KHC_HRA/scores/t640/'
      fwrite(temp, paste0(path,filename))
    # }
  }
}

m_cancer <- fread('shiny/KHC_HRA/scores/t640/m_all_cancer_score.csv')
m_mace <- fread('shiny/KHC_HRA/scores/t640/m_all_mace_score.csv')
m_death <- fread('shiny/KHC_HRA/scores/t640/m_all_death_score.csv')

f_cancer <- fread('shiny/KHC_HRA/scores/t640/f_all_cancer_score.csv')
f_mace <- fread('shiny/KHC_HRA/scores/t640/f_all_mace_score.csv')
f_death <- fread('shiny/KHC_HRA/scores/t640/f_all_death_score.csv')

f_total <- data.table(SEX=f_death$SEX,
                      AGE = f_death$AGE,
                      score_raw = (f_death$score_raw + 0.69*f_mace$score_raw +
                                     0.71*f_cancer$score_raw)/2.4,
                      score_transform = (f_death$score_transform + 0.69*f_mace$score_transform +
                                           0.71*f_cancer$score_transform)/2.4)

m_total <- data.table(SEX=m_death$SEX,
                            AGE = m_death$AGE,
                            score_raw = (m_death$score_raw + 0.69*m_mace$score_raw +
                                           0.71*m_cancer$score_raw)/2.4,
                            score_transform = (m_death$score_transform + 0.69*m_mace$score_transform +
                                           0.71*m_cancer$score_transform)/2.4)

fwrite(f_total,'shiny/KHC_HRA/scores/t640/f_all_total_score.csv')
fwrite(m_total,'shiny/KHC_HRA/scores/t640/m_all_total_score.csv')
f_total

# T640 연령대별 2000명 샘플링 점수 계산----------------------------------------------------
fileList <- list.files('shiny/KHC_HRA/score_raw/t640',pattern = 't640_2000')
fileList
for(f in fileList){
  for(t in target){
    temp <- fread(paste0('shiny/KHC_HRA/score_raw/t640/',f))
    sex <- substr(f,14,14) ;print(sex)
    age <- substr(f,11,12); print(age)
    name <- paste0(c(sex,age,'df'),collapse = '_')
    assign(name, value=calcPred(temp,t))
    
    temp2 <- copy(get(name))
    temp2[,pred_transform := scale(transformTukey(pred,quiet = T))]
    min <- temp2[,min(pred_transform)]
    temp2[,score_transform := pred_transform-min(pred_transform)]
    max <- temp2[,max(score_transform)]
    multi <- 1000/max ; print(multi)
    temp2[,score_transform := 1000-multi*(score_transform)]
    setorder(temp2,-score_transform)
    filename <- paste0(c(sex,age,t,'score.csv'),collapse = '_')
    # filename <- paste0(c(i,k,j,'score.csv'),collapse = '_')
    path <- 'shiny/KHC_HRA/scores/t640/'
    fwrite(temp2, paste0(path,filename))
  }
}



# mean transform score by age*sex ----------------------------------------------
files <- list.files('shiny/KHC_HRA/scores/t640/', pattern='all')
for( i in files){
  temp <- fread(paste0('shiny/KHC_HRA/scores/t640/',i))
  sex <- ifelse(substr(i,1,1)=='m','남성','여성')
  disease <- c('cancer','htn','mace', 'death','illness','metsyn','t2dm','total')
  d <- substr(i,7,10)
  rst <- temp[,.(mean=mean(score_transform),
                  sd=sd(score_transform)),keyby=AGE]
  message('disease: ',d)
  message('Sex: ',sex)
  print(rst)
}





# 백분율 구하기 ----------------------------------------------------------------


temp <- fread('shiny/KHC_HRA/scores/f_40_death_score.csv')
temp_val <- 995.34

percent <- quantile(temp$score_raw, seq(0,1,0.001))
percent_df <- data.table(prob = names(percent),
                         value= percent)
v <- percent_df[percent_df[value<=temp_val,.I[.N]],prob]
100-as.numeric(gsub('%','',v))

percent_df |> str()
names(percent)

percent
percent[percent<temp_val,.I[.N]]



# 가설 검정 -------------------------------------------------------------------

m_cancer <- fread('shiny/KHC_HRA/scores/t640/m_all_cancer_score.csv')
m_mace <- fread('shiny/KHC_HRA/scores/t640/m_all_mace_score.csv')
m_death <- fread('shiny/KHC_HRA/scores/t640/m_all_death_score.csv')
m_illness <- fread('shiny/KHC_HRA/scores/t640/m_all_illness_score.csv')

temp <- data.table(cancer_score = m_cancer$score_transform,
           mace_score = m_mace$score_transform,
           health_score = m_illness$score_transform,
           death_prob = m_death$pred)

lm(death_prob ~ cancer_score, temp) %>% summary()
lm(death_prob ~ mace_score, temp) %>% summary()
lm(death_prob ~ health_score, temp) %>% summary()

f_cancer <- fread('shiny/KHC_HRA/scores/t640/f_all_cancer_score.csv')
f_mace <- fread('shiny/KHC_HRA/scores/t640/f_all_mace_score.csv')
f_death <- fread('shiny/KHC_HRA/scores/t640/f_all_death_score.csv')
f_illness <- fread('shiny/KHC_HRA/scores/t640/f_all_illness_score.csv')

temp <- data.table(cancer_score = f_cancer$score_transform,
                   mace_score = f_mace$score_transform,
                   health_score = f_illness$score_transform,
                   death_prob = f_death$pred)

lm(death_prob ~ cancer_score, temp) %>% summary()
lm(death_prob ~ mace_score, temp) %>% summary()
lm(death_prob ~ health_score, temp) %>% summary()

# score 정규 분포 변환 ----------------------------------------------------------

require(rcompanion)
require(MASS)
getwd()
temp <- fread('shiny/KHC_HRA/scores/f_40_cancer_score.csv')
temp
temp[,score_transform := 1000-125*(scale(transformTukey(pred,quiet = T))+4L)]
temp[,score_transform := ifelse(score_transform<0,0,score_transform)]


transformTukey(temp$pred, returnLambda = T)
hist(temp$score_transform)
x <- log10(temp$pred)
m <- mean(x)
sd <- sd(x)
s <- (x-m)/sd
hist(s)
require(patchwork)
plotNormalHistogram(temp$pred,main = 'Raw predict')
plotNormalHistogram(log(temp$pred), main='Log transformation')
plotNormalHistogram(scale(log(temp$pred),center = T,scale = T),main='Standardized Log transformation')
plotNormalHistogram(transformTukey(temp$pred, plotit = T),main="Tukey's Ladder of Powers transformation")
plotNormalHistogram(scale(transformTukey(temp$pred, plotit = T)),main="Standardized Tukey's Ladder of Powers transformation")
summary(scale(log(temp$pred)), plotit = T)
?transformTukey
?boxcox
b <- boxcox(lm(temp$pred~1))
lambda <- b$x[which.max(b$y)]

boxcox_pred <- (temp$pred^lambda-1)/lambda
plotNormalHistogram(boxcox_pred, main='Box-cox transformation')
plotNormalHistogram(scale(boxcox_pred),main='Standardized Box-cox transformation')
summary(scale(boxcox_pred))
shapiro.test(scale(boxcox_pred))

shapiro.test(temp$pred)
shapiro.test(log(temp$pred))
shapiro.test(transformTukey(temp$pred))
shapiro.test(scale(transformTukey(temp$pred)))
ggplot(temp, aes(pred)) +
  geom_histogram(bins=50)


summary(scale(log(temp$pred)))

ggplot(temp, aes(log(pred))) +
  geom_histogram(bins=50)

ggplot(temp, aes(scale(log(pred))))+
  geom_histogram(bins=50)


scale(x, center = T, scale = T)
# mean -------------------------------------------------------------------

checkup <- data.table::data.table(
           SEX = c(1L,1L,1L,1L,1L,1L,1L,1L,1L,2L,
                   2L,2L,2L,2L,2L,2L,2L,2L),
           AGE = c(35L,40L,45L,50L,55L,60L,65L,70L,
                   75L,35L,40L,45L,50L,55L,60L,65L,70L,75L),
  G1E_TOT_CHOL = c(193.9,196.9,199.9,199.7,199.2,
                   197.1,193.8,190.7,187.1,180.3,183.5,189.7,194.9,208.6,
                   208.3,206.9,204.6,204),
       G1E_HDL = c(52,51.1,52,51.6,51.4,51.8,2.6,
                   51.7,51.3,62,60.6,59.8,59.2,58.7,56.6,55.2,54.6,54),
       G1E_LDL = c(112,112,116,114.7,116.2,114,112.2,
                   110.9,108.1,101.3,104.6,110.5,114.6,125.4,125.3,124,
                   122,121.1),
        G1E_TG = c(141.6,155.5,149.7,158.4,153.1,
                   148.7,142,137.8,135.2,85.4,90,94.8,101.5,115.9,129.1,
                   134,136.6,138.8),
       G1E_FBS = c(93.1,96.2,98.9,100.4,103.9,105.8,
                   105,105.9,103.4,88.2,90.5,93.6,93.4,93,98.2,100.4,
                   101.4,102.4),
       G1E_HGB = c(15.3,15.2,15.1,15.1,14.9,14.7,
                   14.6,13.3,14.1,12.7,12.7,12.6,12.7,12.9,13.1,12.9,12.8,
                   12.7),
      G1E_SGOT = c(26,27,27.2,27.2,28,28.2,27.3,
                   27.2,27.8,19.6,20,20.7,21.8,24.2,25.9,25.4,25.2,25.3),
      G1E_SGPT = c(32.3,32.9,31.5,29.8,28.9,28.7,27,
                   25,24.3,16.2,16.6,17.9,19.1,22.2,24.2,22.8,22.2,
                   20.9),
       G1E_GGT = c(41.6,44.5,45.9,47.9,49.4,47.4,
                   42.9,39.6,38.4,17.3,18.6,19,21.8,24.9,25,24.3,23.5,
                   23.9),
       G1E_BMI = c(24.6,24.4,24.5,24.4,24.4,24.2,
                   24.1,24,23.4,21.5,22.1,22.7,23.1,23.9,24.2,24.5,24.7,
                   24.4),
      G1E_WSTC = c(83.2,83.1,83.9,84,84.9,84.6,85.4,
                   85.3,85,71.8,72.8,74.1,74.9,77.6,79,80.6,82,82.3),
    G1E_BP_SYS = c(122,123.1,123.1,124.3,124.7,126.5,
                   128.1,128.7,130.7,110.5,112.5,114.8,117.4,120.9,
                   123.2,126.5,129,129.7),
    G1E_BP_DIA = c(76.6,77.5,77.9,79,79,79.2,79.1,
                   78,78.6,69.5,71.1,71.8,73.7,75.4,76.4,76.9,78.1,77.6),
          EGFR = c(92.7,91.4,91,89.4,88.4,86.5,83.9,
                   82.9,81,95.6,92.3,90.8,88.4,86.8,83.6,82.7,81.3,
                   79.7),
       DRK_CON = c(6.2,5.6,5.5,5.3,4.8,3.8,3.5,2.6,
                   2.1,1.7,1.4,1.2,1,0.9,0.6,0.3,0.3,0.1),
          METS = c(522.9,515.3,496.3,548.8,555.1,
                   567.9,641.5,647.4,597.4,389.2,433.2,488.9,480.1,517.8,
                   499,500.1,452,381.9)
)
getwd()
fwrite(checkup,'shiny/KHC_HRA/scores/checkup_means.csv')

install.packages('echarts4r')
require(echarts4r)

penguins

penguins %>% 
  group_by(species) %>% 
  summarise(mean = mean(bill_length_mm, na.rm=T),
            med = median(bill_length_mm, na.rm=T),
            rest = 100-mean(bill_length_mm, na.rm=T)) %>% 
  e_chart(species) %>% 
  e_bar(serie = mean, stack='grp') %>% 
  e_bar(serie = med, stack='grp2') %>%
  e_bar(serie= rest, stack='grp') %>% 
  e_color(c('#E41A1C','#377EB8','#FFFFFF'),
          "#d3d3d3") %>% 
  e_flip_coords()

?echarts4r::e_color()
  
install.packages('highcharter')
require(highcharter)


temp <- penguins %>% 
  group_by(species) %>% 
  summarise(mean = mean(bill_length_mm, na.rm=T),
            med = median(bill_length_mm, na.rm=T),
            rest = 100-mean(bill_length_mm, na.rm=T))

temp %>% 
  e_chart(x=species) %>% 
  e_bar(serie=med) %>% 
  e_bar(serie=mean)
temp
# highchart() %>% 
#   hc_add_series(
#     temp,
#     'column',
#     hcaes(x=species, y=mean),
#     name='bars',
#     showInLegend=T,
#   )


install.packages("palmerpenguins")
library(palmerpenguins)
library(RColorBrewer)
my_colors <- brewer.pal(2, "Set1")
my_colors
penguins %>%                            
  count(species) %>%               
  e_charts(x = species) %>%        
  e_bar(serie = n, legend=F) %>% 
  e_flip_coords() %>% 
  e_tooltip(trigger = "axis") %>% 
  e_theme("bee-inspired") %>% 
  e_color(my_colors)



col_stops <- data.frame(
  q = c(0.15, 0.4, .8),
  c = c('#55BF3B', '#DDDF0D', '#DF5353'),
  stringsAsFactors = FALSE
)

highchart() %>%
  hc_chart(type = "solidgauge") %>%
  hc_pane(
    startAngle = -90,
    endAngle = 90,
    background = list(
      outerRadius = '100%',
      innerRadius = '60%',
      shape = "arc"
    )
  ) %>%
  hc_tooltip(enabled = FALSE) %>% 
  hc_yAxis(
    stops = list_parse2(col_stops),
    lineWidth = 0,
    minorTickWidth = 0,
    tickAmount = 2,
    min = 0,
    max = 1000,
    labels = list(y = 26, style = list(fontSize = "15px"))
  ) %>%
  hc_add_series(
    data = 807,
    dataLabels = list(
      y = -50,
      borderWidth = 0,
      useHTML = TRUE,
      style = list(fontSize = "20px")
    )
  ) %>% 
  hc_size(height = 300)


y <- 10
df <- data.frame(
  x = 0.5,
  y = 100,
  y2= 50,
  z = y - rnorm(10, 5, 1)
)
df
path <- 'path://M0,10 L10,10 C5.5,10 5.5,5 5,0 C4.5,5 4.5,10 0,10 z'
df |>
  e_charts(x) |>
  e_pictorial(
    serie = y,
    symbol = 'triangle',
    symbolClip=T,
    symbolBoundingData=100,
    animationDuration=0,
    # symbolRepeat = TRUE,
    color='#ccc'
    # symbolSize = c(10, 4)
  ) %>% 
  e_pictorial(
    serie = y2,
    symbol = path,
    symbolClip=T,
    symbolBoundingData=100,
    color='#bb0004',
    # symbolRepeat = TRUE,
    # symbolSize = c(10, 4)
  ) 


df <- data.frame(
  y = dnorm(seq(-3,3,length=200))
)
df$y
dd <- c(3.5, 3, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, 3.4, 3, 3, 4,
         4.4, 3.9, 3.5, 3.8, 3.8, 3.4, 3.7, 3.6, 3.3, 3.4, 3, 3.4, 3.5, 3.4, 3.2,
         3.1, 3.4, 4.1, 4.2, 3.1, 3.2, 3.5, 3.6, 3, 3.4, 3.5, 2.3, 3.2, 3.5, 3.8, 3,
         3.8, 3.2, 3.7, 3.3, 3.2, 3.2, 3.1, 2.3, 2.8, 2.8, 3.3, 2.4, 2.9, 2.7, 2, 3,
         2.2, 2.9, 2.9, 3.1, 3, 2.7, 2.2, 2.5, 3.2, 2.8, 2.5, 2.8, 2.9, 3, 2.8, 3,
         2.9, 2.6, 2.4, 2.4, 2.7, 2.7, 3, 3.4, 3.1, 2.3, 3, 2.5, 2.6, 3, 2.6, 2.3,
         2.7, 3, 2.9, 2.9, 2.5, 2.8, 3.3, 2.7, 3, 2.9, 3, 3, 2.5, 2.9, 2.5, 3.6,
         3.2, 2.7, 3, 2.5, 2.8, 3.2, 3, 3.8, 2.6, 2.2, 3.2, 2.8, 2.8, 2.7, 3.3, 3.2,
         2.8, 3, 2.8, 3, 2.8, 3.8, 2.8, 2.8, 2.6, 3, 3.4, 3.1, 3, 3.1, 3.1, 3.1, 2.7,
         3.2, 3.3, 3, 2.5, 3, 3.4, 3)
dim(temp)
temp$group <- z(c(1,2),c(80,20))


z=seq(-4,4,0.01)
dat <- data.table(norm=dnorm(z))
qnorm(0.5)

normalDistribution[(nrow(normalDistribution)*0.9):nrow(normalDistribution),x][1]
myLoc <- function(percent){
  normalDistribution <- data.table(
    x = seq(-3,3, by = 0.1),
    y = dnorm(seq(-3,3, by = 0.1))
  )
  criticalValue <- normalDistribution[(nrow(normalDistribution)*percent):nrow(normalDistribution),x][1]
  shadeNormal <- rbind(data.table(x=criticalValue,y=0), 
                       normalDistribution[(nrow(normalDistribution)*percent):nrow(normalDistribution)],
                       data.table(x=3,y=0))
  ggplot(normalDistribution, aes(x,y)) +
    geom_area(fill='#CCCCCC',color='#BBBBBB',alpha=.5) +
    geom_polygon(data = shadeNormal, aes(x=x, y=y),fill='#fede22') +
    guides(fill="none") +
    geom_hline(yintercept = 0) +
    geom_segment(aes(x=0,
                     xend=0,
                     y=-0.02, 
                     yend=dnorm(0)+0.01),
                 lty='dashed') +
    geom_segment(aes(x = criticalValue, y = 0, 
                     xend = criticalValue, 
                     yend = dnorm(criticalValue) +0.1)) + 
    annotate(geom = 'text',x=criticalValue,y=dnorm(criticalValue)+0.12,
             label='내 위치',
             size=10)+
    annotate(geom='text', x=0, y=-0.05, label='연령대 평균\n')+
    coord_cartesian(ylim=c(-0.05,.4), clip='off') +
    theme_void()
}
myLoc(0.90)
data.table(x=3,y=0)
normalDistribution[x>criticalValue]
shadeNormal <- rbind(data.table(x=criticalValue,y=0), 
                     normalDistribution[x>criticalValue],
                     data.table(x=3,y=0))

ggplot(normalDistribution, aes(x,y)) +
  geom_area(fill='#CCCCCC',color='#BBBBBB',alpha=.5) +
  geom_polygon(data = shadeNormal, aes(x=x, y=y),fill='#fede22') +
  guides(fill="none") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x=0,
                   xend=0,
                   y=-0.02, 
                   yend=dnorm(0)+0.01),
               lty='dashed') +
  geom_segment(aes(x = criticalValue, y = 0, 
                   xend = criticalValue, 
                   yend = dnorm(criticalValue) +0.1)) + 
  annotate(geom = 'text',x=criticalValue,y=dnorm(criticalValue)+0.12,
           label='내 위치',
           size=10)+
  annotate(geom='text', x=0, y=-0.05, label='연령대 평균\n')+
  coord_cartesian(ylim=c(-0.05,.4), clip='off') +
  theme_void()

temp <- data.table(x=seq(-3,3,0.1),
                   y=dnorm(seq(-3,3,0.1)))
temp[,.N]
nrow(temp)*0.9
temp[trunc(nrow(temp)*0.9):nrow(temp), z:=y]
temp
temp %>% 
  e_chart(x) %>% 
  e_line(y, symbol='none',
         color='#000000',
         areaStyle=list(
           color='#000000',
           opacity=0.5
         ),
         markPoint = list(
           data=list( 
             list(xAxis=2.3)
           )
         ),
         markLine=list(
           label = list(
             show=T,
             position='middle'
           ),
           data=list(
             list(xAxis=2.3)
           )
         )
         ) %>% 
  # e_line(z, 
  #        symbol='none',
  #        color='#fede22',
  #        areaStyle=list(
  #          color='#fede22'
  #         )
  #        ) %>% 
  e_grid(containLabel=T) %>% 
  e_x_axis(show=F) %>% 
  e_y_axis(show=F) %>% 
  e_tooltip(formatter=htmlwidgets::JS(
    "function(p){console.log(p)}"
    )
  )

temp %>% 
  e_chart(x) %>% 
  e_line(y, symbol='pin') %>% 
  e_tooltip()

