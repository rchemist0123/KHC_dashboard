makeReactiveTrigger <- function(){
  rv <- reactiveValues(a=0)
  list(
    depend = function(){
      rv$a
      invisible()
    },
    trigger = function(){
      rv$a <- isolate(rv$a+1)
    }
  )
}
dbTrigger <- makeReactiveTrigger()

## login authentication ----------------------------------

calcPred <- function(db, target){
  coeflist <- list.files('coefs/',)
  coefFile <- coeflist[coeflist %like% target]
  coef <- fread(paste0('coefs/',coefFile))
  vars <- colnames(coef)
  input_mat <- as.matrix(db[,..vars])
  if(db$SEX=='남성') {
    coef_mat <- as.matrix(coef[2,])
  } else {
    coef_mat <- as.matrix(coef[1,])
  }
  prob_raw <- as.numeric(input_mat %*% t(coef_mat))
  prob <- 1/(1+exp(-prob_raw)) # logit transform
  return(prob)
}

# 상위 n% 계산하기.
#FIXME mulitple rows calculation.
calcTopProb <- function(data, sex, age, disease, vars){
  #@ disease: 질병 영어명
  #@ vars: 질병 한국어명
  sex2 <- ifelse(sex=='남성','m','f')
  age <- as.numeric((age %/% 10) * 10)
  
  # 건강점수 불러오기
  # 동일 연령대 건강점수 계산: 전체 score 데이터에서 연령대 추출.
  # 환자가 측정해서 나온 점수: 같은 연령대와 비교해야 함.
  filename <- paste0(c(sex2,'all',disease,'score.csv'),collapse='_');
  cohort <- fread(paste0('scores/t640/',filename))
  
  score_age <- data[variable == vars, score_age]
  # print(paste0(vars, '점수: ',score_age))
  
  # 같은 연령대의 점수: 기준은 변환한 점수
  percent <- quantile(cohort[AGE==age,score_transform], seq(0,1,0.001))
  
  # 환자가 측정해서 나온 점수를 같은 연령대의 점수와 비교하기.
  percent_df <- data.table(prob = names(percent),
                           score= percent)
  check <- percent_df[score<=score_age]
  if(nrow(check)==0){
    my_prob <- percent_df[1,prob]    
  }else {
    my_prob <- percent_df[percent_df[score<=score_age,.I[.N]],prob]
  }
  
  # print(paste0('raw prob: ',my_prob))
  my_prob <- as.numeric(gsub('%','',my_prob))
  # print(paste(vars,': 상위', my_prob,'%'))
  return(my_prob) # 특정 질병의 상위 % 보여주기.
}

gaugeChart <- function(data, sex, age, disease, vars){
  prob <- round(100-calcTopProb(data, sex, age, disease, vars),1)
  e_charts() %>% 
    e_gauge(prob, '상위',
            center=c('50%','55%'),
            radius= '90%',
            startAngle=0, 
            endAngle=-180,
            clockwise=F,
            title=list(
              offsetCenter = c(0,'-20%')
            ),
            axisLine = list(
              lineStyle = list(
                width=20,
                color=list(c(0.2,'#6495ED'),
                           c(0.5,'#9FE2BF'),
                           c(0.8,'#FFBF00'),
                           c(1,'#DE3163'))
              )
            ),
            axisTick = list(
              lineStyle= list(
                width=2,
                color='#000000'
              )
            ),
            splitLine = list(
              lineStyle=list(
                color='#000000'
              )
            ),
            axisLabel = list(
              show=F
            ),
            pointer = list(
              icon='path://M12.8,0.7l12,40.1H0.7L12.8,0.7z',
              length= '40%',
              width= 20,
              offsetCenter= c(0, '-40%'),
              itemStyle=list(color='auto')
            ),
            detail=list(formatter = '{value} %',
                        color='auto',
                        offsetCenter = c(0,'0%'),
                        valueAnimation=T)) 
}

histoChart <- function(data, sex, age, disease, vars){
  
  # 연령대 평균 표준편차 구하기.
  sex2 <- ifelse(sex=='남성','m','f')
  age <- as.numeric((age %/% 10) * 10)
  filename <- paste0(c(sex2,'all',disease,'score.csv'),collapse='_');
  cohort <- fread(paste0('scores/t640/',filename))
  score_mean <- trunc(cohort[AGE==age, mean(score_transform), by=AGE]$V1,0)
  score_sd <- round(cohort[AGE==age, sd(score_transform), by=AGE]$V1,1)
  
  prob <- calcTopProb(data, sex, age, disease, vars)/100
  normalDistribution <- data.table(
    x = seq(-4,4, by = 0.01),
    y = dnorm(seq(-4,4, by = 0.01))
  );
  
  criticalValue <- normalDistribution[(nrow(normalDistribution)*prob):nrow(normalDistribution),x][1]
  if(prob<0.5){
    shadeNormal <- rbind(data.table(x=criticalValue,y=0), 
                         normalDistribution[x<criticalValue])
  } else {
    shadeNormal <- rbind(data.table(x=criticalValue,y=0), 
                         normalDistribution[x>criticalValue], 
                         data.table(x=3,y=0))
  }
  
  ggplot(normalDistribution, aes(x,y)) +
    geom_area(fill='#DDDDDD',color='#AAAAAA',alpha=.5) +
    geom_polygon(data = shadeNormal, aes(x=x, y=y),fill='#fede22') +
    guides(fill="none") +
    # geom_hline(yintercept = 0) +
    geom_segment(aes(x=0,
                     xend=0,
                     y=-0.01, 
                     yend=dnorm(0)+0.01),
                 lty='dashed') +
    geom_segment(aes(x = criticalValue, y = 0, 
                     xend = criticalValue, 
                     yend = dnorm(criticalValue) +0.1),
                 lty='dashed') + 
    annotate(geom = 'text',
             x=criticalValue,
             y=dnorm(criticalValue)+0.12,
             size=5,
             label='내 위치')+
    annotate(geom='text', x=0, y=-0.04, 
             size=5,
             label=paste0(age,'대 연령 그룹\n',
                          vars ,' 건강점수 평균 ', score_mean,' (',
                          '표준편차 ',score_sd,')점'))+
    coord_cartesian(ylim=c(-0.07,.4), clip='off') +
    theme_void()
}

Robert_BA<-function(db){
  if(db$SEX=='남성') x <- 1
  else x <- 2
    dt <- setDT(db)
    input <- copy(dt[,.(AGE, G1E_BMI, G1E_WSTC, G1E_BP_SYS, G1E_FBS, G1E_TOT_CHOL, G1E_HDL, G1E_HGB, G1E_SGPT, G1E_CRTN,
                                drk_amt,drk_freq, SMK_COMBINED1,SMK_COMBINED2)])
    input[,`:=`(scrmin = ifelse(G1E_CRTN<=0.9, G1E_CRTN/0.9,1),
                scrmax = ifelse(G1E_CRTN>=0.9, G1E_CRTN/0.9,1))]
    input[,EGFR := ifelse(x==1,
                         141*((scrmin)^(-0.411))*((scrmax)^(-1.209))*((0.993)^(AGE)),
                         1.018*141*((scrmin)^(-0.329))*((scrmax)^(-1.209))*((0.993)^(AGE)))]
    input[,G1E_CRTN := ifelse(EGFR>=90,1, ifelse(EGFR>=60,2, ifelse(EGFR>=30,3, ifelse(EGFR>=15,4,5))))]
    input[,EGFR:=NULL]
    input[,drk_amt := (drk_amt*drk_freq)/7]
    input[,SMK_COMBINED1 := SMK_COMBINED1 + SMK_COMBINED2]
    input[,drk_freq := NULL]
    input[,SMK_COMBINED2 := 1]
    BA_dataframe <- matrix(0, dim(input)[1],1)
    age_label <- input$AGE %/% 10;
    if (age_label<=2) age_label <- 3
    if (age_label>=7) age_label <- 6
    age_label <- age_label*10
    filename_coef <- paste0('coefs/', x,'_',age_label,'.csv')
    coefs <- fread(filename_coef)
    input_row<-as.data.table(input)
    input_params<-input_row%>%select(-scrmin)%>%select(-scrmax)
    coefs<-as.data.table(coefs)
    input_params_mtrx<-as.matrix(input_params)
    coef_mtrx<-as.matrix(coefs[,2])
    BA<-input_params_mtrx %*% coef_mtrx
    BA_dataframe[,1]<-BA
    BA_out<-round(data.table(BA_dataframe),2)
    BA_out<- cbind(input$AGE,BA_out)
    names(BA_out)<-c('AGE', 'BA')
  return(BA_out$BA)
}

user_base <- tibble(
  user_id = "admin",
  password  = sodium::password_store('123'),
  name='admin'
)

shiny::shinyServer(function(input, output, session){
  
  ns <- session$ns
  dt <- reactiveValues(data=list())
  flag <- 0
  
# intro image ------------------------------------------------------------
  # login ---------------
  
  output$display_content_authr <- renderUI({
    req(user_auth())
    div(
      class = "bg-success",
      id = "success_module",
      h4("Access confirmed!"),
      p("Welcome to your shinyauthr-secured application! 
              Notice that password is encrypted.")
    )
  })

  credential <- shinyauthr::loginServer(
    id='login',
    data=user_base,
    user_col = user_id,
    pwd_col = password,
    sodium_hashed = T,
    log_out = reactive(logout_init()),
    reload_on_logout = T
  )
  
  logout_init <- shinyauthr::logoutServer(
    id='logout',
    active=reactive(credential()$user_auth),
  )
  
  user_auth <- reactive({
    credential()$user_auth
  })
  user_data <- reactive({
    credential()$info
  })
  
  output$sidebar <- renderMenu({
    req(credential()$user_auth)
    sidebarMenu(
      id='menu',
      menuItem('건강검진 작성', tabName='Checkup', icon = icon('file-csv',lib='font-awesome',verify_fa = FALSE)),
      menuItem('대시보드', tabName='Dashboard', icon=icon('chart-line',lib = 'font-awesome', verify_fa = FALSE))
    )
  })
  # Input Validation ---------------------------------------
  
  not_greater_than <- function(value, limit){
    if(value>limit) paste0(limit, " 이하의 값이어야 합니다.")
  }
  not_less_than <- function(value,limit){
    if(value<limit) paste0(limit, " 이상의 값이어야 합니다.")
  }
  
  iv <- InputValidator$new() # baseline
  target1 <-c('name','age','sex')
  for(i in target1){
    iv$add_rule(i, sv_required('필수 입력사항입니다.'))
  }
  iv$add_rule('age',not_greater_than, limit=100)
  iv$add_rule('age',not_less_than, limit=20)
  
  iv2 <- InputValidator$new() # checkup
  target2 <- c('bmi','wc','sbp','dbp','fbs','hgb','totChol',
               'ldlChol','hdlChol','ast','alt','ggt','tg','cr')
  for(i in target2){
    iv2$add_rule(i, sv_required('필수 입력사항입니다.'))
  }
  
  iv3 <- InputValidator$new() # lifestyle
  target3 <- c('drk_freq','drk_amt','pa_walk_freq','pa_walk_amt','pa_mid_freq',
               'pa_mid_amt','pa_high_freq','pa_high_amt')
  for(i in target3){
    iv3$add_rule(i, sv_required('필수 입력사항입니다.'))
    }
  iv3$add_rule('drk_freq',not_greater_than, limit=7)
  iv3$add_rule('pa_walk_freq',not_greater_than, limit=7)
  iv3$add_rule('pa_mid_freq',not_greater_than, limit=7)
  iv3$add_rule('pa_high_freq',not_greater_than, limit=7)
  
  
  observe({
    if(user_auth()){
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      # newtab <- switch(input$tabtab,
      #                  'login'='intro')
      # updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  

# 버튼 로직설정 -------------------------------------------------------------------

  observeEvent(input$start,{
    newtab <- switch(input$tabtab,
                     'intro'='baseline')
    updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
  })
  
  observeEvent(input$writeData,{
    newtab <- switch(input$tabtab,
                     'baseline'='checkup')
    updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
  })
  
  observeEvent(input$getData,{
    newtab <- switch(input$tabtab,
                     'baseline'='db')
    updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
  })
  
  observeEvent(input$toNext,{
    newtab <- switch(input$tabtab,
                     'checkup'='lifestyle',
                     'lifestyle'='familyHistory')
    if(input$tabtab=='baseline'){
      iv$enable()
      if(iv$is_valid()){
        updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
      }
    } else if (input$tabtab=='checkup'){
      iv2$enable()
      if(iv2$is_valid()){
        updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
      }
    } else if (input$tabtab=='lifestyle'){
      iv3$enable()
      if(iv3$is_valid()){
        updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
      }
    }
  })
  
  observeEvent(input$toPrev,{
    newtab <- switch(input$tabtab,
                     'baseline'='intro',
                     'checkup'='baseline',
                     'db' ='baseline',
                     'lifestyle'='checkup',
                     'familyHistory'='lifestyle')
    updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
  })
  observeEvent(input$reset,{
    shinyalert(
      inputId = 'checkReset',
      title='경고!',
      text='초기화하시겠습니까?',
      imageUrl="jordy_surprise.gif",
      closeOnEsc = T,
      closeOnClickOutside = T,
      showCancelButton = T,
      showConfirmButton = T,
      confirmButtonText = '초기화',
      cancelButtonText = '취소'
    )
  })
  observeEvent(input$checkReset,{
    if(input$checkReset){
      # session$reload()
      target <- c('sex','age','bmi','wc','sbp','dbp','fbs','hgb','tg','totChol',
                  'ldlChol','hdlChol','ast','alt','ggt','cr','drk_freq','drk_amt','pa_walk_freq',
                  'pa_walk_amt','pa_mid_freq','pa_mid_amt','pa_high_freq','pa_high_amt')
      for(i in target){
        updateNumericInput(session, inputId = i, value=NA)
      }
      updateNumericInput(session, inputId = 'name', value='')
      newtab <- switch(input$tabtab,
                       'familyHistory' = 'baseline')
      updateTabItems(session,inputId = 'tabtab', selected=newtab)
    }
  })
  
  observeEvent(input$toStart,{
    newtab <- switch(input$tabtab,
                     'familyHistory'='intro')
    updateTabsetPanel(session, inputId = 'tabtab', selected=newtab)
  })
  
  # 검진 기록 작성하기 --------------------------------------------------------------
  
  observeEvent(input$write,{
    shinyalert(
      title='입력 완료',
      text='건강검진 정보가 저장되었습니다!',
      showConfirmButton = T,
      confirmButtonText = 'OK',
      closeOnEsc = T,
      closeOnClickOutside = T,
      timer = 2000,
      imageUrl = "jordy_lion.gif",
      imageWidth = 200
    )
    
    newtab <- switch(input$menu,
                     'Dashboard' = 'Checkup',
                     'Checkup' = 'Dashboard'
    )
    updateTabItems(session,inputId = 'menu', selected=newtab)
  })
  
  # TODO 검진 정보 이용 시, 결측치가 있을 경우, 연령대별 평균 데이터 넣기...
  data <- eventReactive(input$write,{
    if(nrow(selectedDB())==1){
      temp <- selectedDB()
    } else {
      record <- list(
        date = as.character(input$dates),
        name = input$name,
        age = input$age,
        sex = input$sex,
        bmi = input$bmi,
        wc = input$wc,
        sbp = input$sbp,
        dbp = input$dbp,
        fbs = input$fbs,
        hgb = input$hgb,
        totChol = input$totChol,
        ldlChol = input$ldlChol,
        hdlChol = input$hdlChol,
        ast = input$ast,
        alt = input$alt,
        ggt = input$ggt,
        tg = input$tg,
        cr = input$cr,
        smk = input$smk,
        drk_freq = input$drk_freq,
        drk_amt = input$drk_amt,
        pa_walk_freq = input$pa_walk_freq,
        pa_walk_amt = input$pa_walk_amt,
        pa_mid_freq = input$pa_mid_freq,
        pa_mid_amt = input$pa_mid_amt,
        pa_high_freq = input$pa_high_freq,
        pa_high_amt = input$pa_high_amt,
        fhx_hptn = input$fhx_hptn,
        fhx_dm = input$fhx_dm,
        fhx_hdise = input$fhx_hdise,
        fhx_strk = input$fhx_strk
      )
      target <- c(이름='name',날짜='dates',나이='age',성별='sex', 'BMI'='bmi', 허리둘레='wc',
                  수축기혈압='sbp',이완기혈압='dbp',공복혈당='fbs',혈색소='hgb', 총콜레스테롤='totChol',
                  `LDL 콜레스테롤`='ldlChol',`HDL 콜레스테롤`='hdlChol',AST='ast',ALT='alt',`Gamma GTP`='ggt',`트리글리세라이드`='tg',크레아티닌 = 'cr',
                  흡연상태='smk',`음주빈도`='drk_freq',`1회음주량`='drk_amt',`1주일 걷기 횟수` = 'pa_walk_freq',`걷기 시간`='pa_walk_amt',
                  `중강도 운동 횟수`='pa_mid_freq',`중강도 운동 시간`='pa_mid_amt',`고강도 운동 횟수`='pa_high_freq',
                  `고강도 운동 시간` = 'pa_high_amt')
      if(is.null(dt$data)){flag<<-1} # 처음 write할 때는 null
      if(flag==1){
        dt$data[['table']] <- NULL
        flag <- 0
      }
      
      # db에 데이터 넣기
      sql <- DBI::sqlInterpolate(con," INSERT INTO checkup (
                               dates, NAME, AGE, SEX, G1E_BMI, G1E_WSTC, G1E_BP_SYS, G1E_BP_DIA, G1E_FBS, G1E_HGB,
                               G1E_TOT_CHOL, G1E_LDL, G1E_HDL, G1E_SGOT,
                               G1E_SGPT, G1E_GGT, G1E_TG, G1E_CRTN,
                               smk, drk_freq, drk_amt, pa_walk_freq, pa_walk_amt, pa_mid_freq, pa_mid_amt, pa_high_freq, pa_high_amt,
                               Q_FHX_HTDZ1, Q_FHX_DM1, Q_FHX_HTN1, Q_FHX_STK1)
                               values (
                                ?dates, ?NAME, ?AGE, ?SEX, ?G1E_BMI, ?G1E_WSTC, ?G1E_BP_SYS,  ?G1E_BP_DIA, ?G1E_FBS, ?G1E_HGB,
                                ?G1E_TOT_CHOL, ?G1E_LDL, ?G1E_HDL, ?G1E_SGOT, ?G1E_SGPT, ?G1E_GGT,
                                ?G1E_TG, ?G1E_CRTN,
                                ?smk, ?drk_freq, ?drk_amt, ?pa_walk_freq, ?pa_walk_amt, ?pa_mid_freq,
                                ?pa_mid_amt, ?pa_high_freq, ?pa_high_amt,
                                ?Q_FHX_HTDZ1, ?Q_FHX_DM1, ?Q_FHX_HTN1, ?Q_FHX_STK1)",
                               dates = as.character(input$dates),
                               NAME = input$name,
                               AGE = input$age,
                               SEX = input$sex,
                               G1E_BMI = input$bmi,
                               G1E_WSTC = input$wc,
                               G1E_BP_SYS = input$sbp,
                               G1E_BP_DIA = input$dbp,
                               G1E_FBS = input$fbs,
                               G1E_HGB = input$hgb,
                               G1E_TOT_CHOL = input$totChol,
                               G1E_LDL = input$ldlChol,
                               G1E_HDL = input$hdlChol,
                               G1E_SGOT = input$ast,
                               G1E_SGPT = input$alt,
                               G1E_GGT = input$ggt,
                               G1E_TG = input$tg,
                               G1E_CRTN = input$cr,
                               smk = input$smk,
                               drk_freq = input$drk_freq,
                               drk_amt = input$drk_amt,
                               pa_walk_freq = input$pa_walk_freq,
                               pa_walk_amt = input$pa_walk_amt,
                               pa_mid_freq = input$pa_mid_freq,
                               pa_mid_amt = input$pa_mid_amt,
                               pa_high_freq = input$pa_high_freq,
                               pa_high_amt = input$pa_high_amt,
                               Q_FHX_HTN1 = input$fhx_hptn,
                               Q_FHX_DM1 = input$fhx_dm,
                               Q_FHX_HTDZ1 = input$fhx_hdise,
                               Q_FHX_STK1 = input$fhx_strk)
      dbExecute(con, sql)
      dbTrigger$trigger()
      
      con <- dbConnect(SQLite(), dbname = 'db.sqlite')
      dbTrigger$depend()
      
      temp <- as.data.table(dbGetQuery(con,'select * from checkup order by id desc limit 1;'))
      dbDisconnect(con)
    }
   
    colnames(temp) <- c('ID', 'DATES', 'NAME','AGE','SEX','G1E_BMI','G1E_WSTC','G1E_BP_SYS','G1E_BP_DIA',
                        'G1E_FBS','G1E_HGB','G1E_TOT_CHOL','G1E_LDL','G1E_HDL','G1E_SGOT',
                        'G1E_SGPT','G1E_GGT','G1E_TG','G1E_CRTN','smk','drk_freq',
                        'drk_amt','pa_walk_freq','pa_walk_amt','pa_mid_freq','pa_mid_amt','pa_high_freq',
                        'pa_high_amt','Q_FHX_HTDZ1','Q_FHX_DM1','Q_FHX_HTN1','Q_FHX_STK1')
    numVars <- c('AGE','G1E_BMI','G1E_WSTC','G1E_BP_DIA','G1E_BP_SYS','G1E_HGB',
                 'G1E_FBS','G1E_TOT_CHOL','G1E_LDL','G1E_HDL','G1E_SGOT',
                 'G1E_SGPT','G1E_GGT','G1E_TG','G1E_CRTN','drk_freq','drk_amt','pa_walk_freq','pa_walk_amt','pa_mid_freq','pa_mid_amt','pa_high_freq',
                 'pa_high_amt')
    temp[,(numVars):=lapply(.SD, as.numeric),.SDcols=numVars]
    temp[,`:=`(
      Q_FHX_HTDZ1 = ifelse(Q_FHX_HTDZ1==F,0,1),
      Q_FHX_DM1 = ifelse(Q_FHX_DM1==F,0,1),
      Q_FHX_HTN1 = ifelse(Q_FHX_HTN1==F,0,1),
      Q_FHX_STK1 = ifelse(Q_FHX_STK1==F,0,1)
    )]
    temp[,`:=`(
      DRK_CON = drk_freq,
      DRK_COMBINED = ifelse(drk_freq==0,0,
                            ifelse((SEX=='남성' & drk_freq>=4 & drk_amt>=7)|
                                     (SEX=='여성' & drk_freq>=4 & drk_amt>=5),2,1)) %>% as.factor,
      SMK_COMBINED = ifelse(smk=='한 번도 흡연한 적 없음',0,
                            ifelse(smk=='현재 흡연 중',2,1)) %>% as.factor,
      METS = 2.9*(pa_walk_freq * pa_walk_amt) +
        4*(pa_mid_freq * pa_mid_amt) +
        7*(pa_high_freq * pa_high_amt),
      EGFR = ifelse(SEX=='남성',186.3*(G1E_CRTN^-1.154)*AGE^-0.203,
                    186.3*(G1E_CRTN^-1.154)*AGE^-0.203*0.742)
    )][,`:=`(
      SMK_COMBINED1 = ifelse(smk=='과거 흡연했으나 현재는 안함',1,0),
      SMK_COMBINED2 = ifelse(smk=='현재 흡연 중',1,0),
      # DRK_COMBINED1 = ifelse(DRK_COMBINED==1,1,0),
      # DRK_COMBINED2 = ifelse(DRK_COMBINED==2,1,0),
      intercept = 1,
      `METS^2` = I(METS^2),
      `DRK_CON^2` = I(drk_freq^2)
    )]
  })
  
  dbData <- eventReactive(input$getData,{
    temp <- as.data.table(dbGetQuery(con,sprintf("select * from checkup where name= '%s';", input$name)))
  })
  # 입력한 데이터 DB에 반영하기
  output$records <- renderDT({
    dbData()
  },selection='single',
    rowname=F,
  # colnames=c('ID','검진일자','이름','나이','성별','BMI','허리둘레(cm)',
  #            '수축기혈압','이완기혈압','공복혈당','당화혈색소',
  #            '총 콜레스테롤','LDL 콜레스테롤','HDL 콜레스테롤','AST','ALT','GGT',
  #            '중성지방','크레아티닌','흡연여부','1주일 음주빈도','1회 평균 음주량',
  #            '1주일 걷기횟수','1회 평균 걷기시간',' 1주일 중강도운동 횟수','1회 평균 중강도운동 시간',
  #            '1주일 고강도운동 횟수','1회 평균 고강도운동 시간',
  #            '가족력: 심장질환','가족력: 당뇨','가족력: 고혈압','가족력: 뇌졸중'),
  options=list(dom='tip',
               searching=F,
               columnDefs=list(list(targets='_all', class='dt-center')),
               scrollX=T,
               rowCallback=DT::JS(
                 "function(row, data) {
                   if (data[5]>=25) {
                      $('td:eq(5)', row).css('font-weight','bold')
                                        .css('color','red')
                                        .css('background-color','#F9EBEA');
                   } if(data[7]>=140) {
                      $('td:eq(7)', row).css('font-weight','bold')
                                        .css('color','red')
                                        .css('background-color','#F9EBEA');
                   } if(data[8]>= 90) {
                      $('td:eq(8)', row).css('font-weight','bold')
                                        .css('color','red')
                                        .css('background-color','#F9EBEA');
                   } if(data[9]>= 126) {
                      $('td:eq(9)', row).css('font-weight','bold')
                                        .css('color','red')
                                        .css('background-color','#F9EBEA');;
                    }
                 }"
               )))
  
  selectedDB <- reactive({
    dbData()[input$records_rows_selected]
  })
  
  # 환자 기본 정보 -------------------------------------------------------------------
  user_sex <- reactive({data()$SEX})
  user_age <- reactive({data()$AGE})
  user_name <- reactive({data()$NAME})
  # 신상정보 표시 ----------------------------------------------------
  output$subjectInfo <- renderText({
    paste0(user_name(),'님 | ',substr(user_sex(),1,1), ' | ',user_age(),'세')
  })
  
  # 건강검진 수치 그래프 ----------------------------------------
  output$checkBar <- renderEcharts4r({
    
    # 평균 데이터 가져오기
    score_table <- fread('scores/checkup_means.csv')
    score_table[,SEX:=ifelse(SEX==1,'남성','여성')]
    
    ## 20대인 경우 30대로 그냥 사용
    if(user_age() %/% 10 == 2 ) user_age <- 30 # 10세 단위
    else if (user_age() %/% 10 >7) user_age <- 70
    else user_age <- (user_age() %/% 10) * 10 
    score_table_matched <- score_table[SEX==data()$SEX & AGE==user_age]
    
    target <- c('SEX', 'AGE', 'G1E_BMI', 'G1E_WSTC', 'G1E_BP_SYS', 'G1E_BP_DIA', 
                'G1E_FBS', 'G1E_HGB', 'G1E_TOT_CHOL', 'G1E_LDL', 'G1E_HDL',
                'EGFR', 'G1E_SGOT', 'G1E_SGPT', 'G1E_GGT', 'G1E_TG','METS')
    kor_names <- c('성별','나이','BMI','허리둘레','수축기혈압','이완기혈압',
                   '공복혈당','혈색소','총콜레스테롤','LDL','HDL','eGFR',
                   'AST','ALT','GGT','중성지방','운동량')
    my_score <- copy(data()[,..target]); setnames(my_score,target,kor_names)
    setnames(score_table_matched, target, kor_names)
    dt_melt <- melt(my_score, id.vars=c('성별','나이'), variable.name = 'features', value.name = '내 점수')
    score_melt <- melt(score_table_matched, id.vars=c('성별','나이'),variable.name='features', value.name = '또래 평균')
    
    dt_final <- merge(dt_melt[,.(features, `내 점수`=round(`내 점수`),1)], 
                      score_melt[,.(features, `또래 평균`=round(`또래 평균`),1)], by='features')
    dt_final %>% 
      e_chart(x=features) %>% 
      e_bar(serie=`내 점수`,
            itemStyle = list(
              borderRadius = c(4,4,0,0)
            )) %>% 
      e_bar(serie=`또래 평균`,
            itemStyle = list(
              borderRadius = c(4,4,0,0)
            )) %>% 
      e_color(color = c('#79AFFF','#D6EAF8')) %>%
      e_legend(textStyle=list(
        fontSize=15
      )) %>% 
      e_tooltip() %>% 
      e_x_axis(axisLabel = list(interval=0, rotate=45))
  })
  

  ## chronic Score ----------------------------------------
  chronicScore <- reactive({
    t2dm_probs <- calcPred(data(),'t2dm'); t2dm_score <-  (1-t2dm_probs)*1000
    htn_probs <- calcPred(data(),'htn'); htn_score <-  (1-htn_probs)*1000
    metsyn_probs <- calcPred(data(),'metsyn'); metsyn_score <-  (1-metsyn_probs)*1000
    message('################### Raw Score ########################')
    message('당뇨 건강점수: ', t2dm_score,'점')
    message('고혈압 건강점수: ', htn_score,'점')
    message('대사증후군 건강점수: ', metsyn_score,'점')
    user_age <- 10*(user_age() %/% 10)
    user_age <- ifelse(user_age<30,'30',ifelse(user_age>80,'70',as.character(user_age)))
    user_sex <- ifelse(data()$SEX=='남성','m','f')
    target <- c('t2dm','htn','metsyn')
    for(d in target){
      filename <- paste0(c(user_sex,user_age,d,'score.csv'),collapse='_');
      score_age <- fread(paste0('scores/t640/',filename)) 
      # score_age[,score_transform := ifelse(score_transform<0,0,ifelse(score_transform>1000,1000,score_transform))]
      
      filename_all <- paste0(c(user_sex,'all',d,'score.csv'),collapse='_');
      score_all <- fread(paste0('scores/t640/',filename_all)) 
      # score_all[,score_transform := ifelse(score_transform<0,0,ifelse(score_transform>1000,1000,score_transform))]
      score <- get(paste0(d,'_score')) # 실제 점수;
      score_adjust_age <- score_age[score_transform<=score][1,score_transform]# 조정점수
      if(is.na(score_adjust_age)) score_adjust_age <- score_age[score_transform>score][.N,score_transform]
      assign(paste0(c(d,'score','adjust_age'),collapse='_'), score_adjust_age)
      
      score_adjust_all <- score_all[score_transform<=score][1,score_transform]# 조정점수
      if(is.na(score_adjust_all)) score_adjust_all <- score_all[score_transform>score][.N,score_transform]
      assign(paste0(c(d,'score','adjust_all'),collapse='_'), score_adjust_all)
    }
    
    message('################### Adjusted Score ####################')
    message('당뇨 건강점수: ', t2dm_score_adjust_age,'점')
    message('고혈압 건강점수: ', htn_score_adjust_age,'점')
    message('대사증후군 건강점수: ', metsyn_score_adjust_age,'점')
    temp <- data.table(variable= c('당뇨','고혈압','대사증후군'),
                       score_raw = c(t2dm_score, htn_score, metsyn_score),
                       score_age = c(t2dm_score_adjust_age, htn_score_adjust_age, metsyn_score_adjust_age),
                       score_all = c(t2dm_score_adjust_all, htn_score_adjust_all, metsyn_score_adjust_all)
    )
    temp[,`:=`(score_age = score_age,
               score_all = score_all,
               score_age2 = 1000-score_age,
               score_all2 = 1000-score_all,
               total = 1000)]
    temp
  })
  
  # healthScore -------------------------------
  healthScore <- reactive({
    
    cancer_probs <- calcPred(data(),'cancer') ; cancer_score <-  (1-cancer_probs)*1000
    mace_probs <- calcPred(data(),'mace'); mace_score <-  (1-mace_probs)*1000
    death_probs <- calcPred(data(),'death'); death_score <-  (1-death_probs)*1000
    illness_probs <- calcPred(data(),'illness'); illness_score <-  (1-illness_probs)*1000
    total_score <- 1000/2.4*((1-death_probs) + 0.69*(1-mace_probs) + 0.71*(1-cancer_probs))
    message('############# Raw Score  ##################')
    message('기본 건강점수 (건강상태유지): ', illness_score,'점')
    message('기본 건강점수 (사망): ', death_score,'점')
    message('암 건강점수: ',  cancer_score,'점')
    message('심혈관 건강점수: ', mace_score,'점')
    message('통합 건강점수: ', total_score,'점')
    
    
    # find user age
    user_age <- 10*(user_age() %/% 10)
    user_age <- ifelse(user_age<30,'30',ifelse(user_age>80,'70',as.character(user_age)))
    user_sex <- ifelse(data()$SEX=='남성','m','f')
    target <- c('cancer','death','mace','illness')
    
    for(d in target){
      # 연령대별 계산
      filename <- paste0(c(user_sex,user_age,d,'score.csv'),collapse='_');
      score_age <- fread(paste0('scores/t640/',filename))
      # score_age[,score_transform := ifelse(score_transform<0,0,ifelse(score_transform>1000,1000,score_transform))]
      
      # 나의 건강점수: 전체에서 점수 계산
      # score_all[,score_transform := ifelse(score_transform<0,0,ifelse(score_transform>1000,1000,score_transform))]
      score <- get(paste0(d,'_score')) # 실제 점수;
      score_adjust_age <- score_age[score_transform<=score][1,score_transform] # 조정점수
      if(is.na(score_adjust_age)) score_adjust_age <- score_age[score_transform>score][.N,score_transform]
      assign(paste0(c(d,'score','adjust_age'),collapse='_'), score_adjust_age)
      
      filename_all <- paste0(c(user_sex,'all',d,'score.csv'),collapse='_');
      score_all <- fread(paste0('scores/t640/',filename_all)) 
      score_adjust_all <- score_all[score_transform<=score][1,score_transform]# 조정점수
      if(is.na(score_adjust_all)) score_adjust_all <- score_all[score_transform>score][.N,score_transform]
      assign(paste0(c(d,'score','adjust_all'),collapse='_'), score_adjust_all)
    }
    total_score_adjust_age <- round((death_score_adjust_age + 0.69*(mace_score_adjust_age) + 0.71*(cancer_score_adjust_age))/2.4)
    total_score_adjust_all <- round((death_score_adjust_all + 0.69*(mace_score_adjust_all) + 0.71*(cancer_score_adjust_all))/2.4)
    
    message('############# AGE Adjust Score #################')
    message('기본 건강점수 (건강상태유지): ', illness_score_adjust_age,'점')
    message('기본 건강점수 (사망): ', death_score_adjust_age,'점')
    message('암 건강점수: ', cancer_score_adjust_age,'점')
    message('심혈관 건강점수: ', mace_score_adjust_age,'점')
    message('통합 건강점수: ',  total_score_adjust_age,'점')
    
    temp <- data.table(variable= c('건강상태유지', '사망','암','심뇌혈관', '통합'),
                       raw_score = c(illness_score,death_score,cancer_score,mace_score, total_score),
                       score_age = c(illness_score_adjust_age, death_score_adjust_age, 
                                     cancer_score_adjust_age, mace_score_adjust_age, 
                                     total_score_adjust_age),
                       score_all = c(illness_score_adjust_all, death_score_adjust_all, 
                                     cancer_score_adjust_all, mace_score_adjust_all, 
                                     total_score_adjust_all)
    )
    temp[,`:=`(score_age = score_age,
               score_all = trunc(score_all),
               total = 1000)]
    temp
  })
  
  ## 만성질환 게이지 ----------------------------------------------------------------
  chronic_target <- c('t2dm','htn','metsyn')
  chronic_gauges <- paste0(chronic_target,'Gauge')
  chronic_vars <- c('당뇨','고혈압','대사증후군')
  chronic_ranks <- paste0(chronic_target,'Rank')
  
  lapply(1:3, \(i){
    output[[paste0('sigh',i)]] <- renderUI({
      tags$img(src='jordy_sigh.gif', height='30%',width='30%', align='center')
    })
    
    output[[paste0('msg',i)]] <- renderUI({
      HTML(paste0('<h4><strong>',chronic_vars[i], '</strong>이 의심됩니다.<br/><br/>병원 진료를 고려해보세요.</h4>'))
    })
    
    output[[chronic_ranks[i]]] <- renderUI({
      prob <- calcTopProb(data= chronicScore(),
                          sex = user_sex(),
                          age = user_age(),
                          disease = chronic_target[i],
                          vars=chronic_vars[i])
      HTML(paste0(user_name(),'님의 ', chronic_vars[i],' 건강점수는 상위 ',
                  '<strong>',round(100-prob,1), '%</strong> 입니다.'))
    })
    # gauge chart
    output[[chronic_gauges[i]]] <- renderEcharts4r({
      gaugeChart(data=chronicScore(), 
                 sex = user_sex(),
                 age = user_age(),
                 disease = chronic_target[i],
                 vars=chronic_vars[i])
    })
  })
  
  chronicDisease <- reactive({
    input$tabtab2
  })
  observe({
    if(chronicDisease() %like% 't2dm'){
      t2dm_check <- ifelse(data()$G1E_FBS>=126,1,0);
      if(t2dm_check==1){
        output$jordySigh1 <- renderUI({
          uiOutput('sigh1')
        })
        output$t2dmMsg <- renderUI({
          uiOutput('msg1')
        })
        output$t2dmPercent <- renderUI({})
      } else {
        output$jordySigh1 <- renderUI({})
        
        output$t2dmMsg <- renderUI({
          uiOutput('t2dmRank')
        })
        output$t2dmPercent <- renderUI({
          echarts4rOutput('t2dmGauge')
        })
      }
    } else if (chronicDisease() %like% 'htn') {
      htn_check <- ifelse(data()$G1E_BP_SYS>=140 | 
                            data()$G1E_BP_DIA>=90,1,0)
      if(htn_check==1){
        output$jordySigh1 <- renderUI({})
        output$jordySigh3 <- renderUI({})
        output$jordySigh2 <- renderUI({
          uiOutput('sigh2')
        })
        output$htnMsg <- renderUI({
          uiOutput('msg2')
        })
        output$htnPercent <- renderUI({})
      } else {
        output$jordySigh2 <- renderUI({})
        output$htnMsg <- renderUI({
          uiOutput('htnRank')
        })
        output$htnPercent <- renderUI({
          echarts4rOutput('htnGauge')
        })
      }
      
    } else if (chronicDisease() %like% 'metsyn'){
      ifg_criteria <- ifelse(data()$G1E_FBS>=100,1,0)
      bp_criteria <- ifelse((data()$G1E_BP_SYS>=130 | data()$G1E_BP_DIA>=85),1,0)
      hdl_criteria <- ifelse(user_sex()=='남성' & data()$G1E_HDL<40 |
                                 user_sex()=='여성' & data()$G1E_HDL<50,1,0)
      wc_criteria <- ifelse(user_sex()=='남성' & data()$G1E_WSTC>=95 |
                               user_sex()=='여성' & data()$G1E_WSTC>=85,1,0)
      tg_criteria <- ifelse(data()$G1E_TG>=150,1,0)
      mets_criteria_sum <- sum(ifg_criteria, bp_criteria, hdl_criteria, wc_criteria, tg_criteria)
      # print(paste0('mets sum: ', mets_criteria_sum))
      metsyn_check <- ifelse(mets_criteria_sum>=3,1,0)
      if(metsyn_check==1){
        output$jordySigh1 <- renderUI({})
        output$jordySigh2 <- renderUI({})
        output$jordySigh3 <- renderUI({
          uiOutput('sigh3')
        })
        output$metsynMsg <- renderUI({
          uiOutput('msg3')
        })
        output$metsynPercent <- renderUI({})
      } else {
        output$jordySigh3 <- renderUI({})
        output$metsynMsg <- renderUI({
          uiOutput('metsynRank')
        })
        output$metsynPercent <- renderUI({
          echarts4rOutput('metsynGauge')
        })
      }
    }
  })
  
  # 만성질환 정보 infobox ---------------------------------------------------------
  output$T2DMInfo <-renderInfoBox({
    t2dm_yn <- ifelse(data()$G1E_FBS>=126,1,0)
    t2dm_message <- ifelse(data()$G1E_FBS>=126,'당뇨가 의심됩니다!',
                           ifelse(data()$G1E_FBS>=100,'주의하세요.','양호합니다.'))
    t2dm_icon <- ifelse(data()$G1E_FBS>=126,'fa-solid fa-land-mine-on',
                        ifelse(data()$G1E_FBS>=100,'fa-solid fa-triangle-exclamation',
                               'fa-regular fa-thumbs-up'))
    color <- ifelse(data()$G1E_FBS>=126,'red',
                    ifelse(data()$G1E_FBS>=100,'orange','light-blue'))
    infoBox('제2형당뇨',t2dm_message,
            icon = tags$i(class=t2dm_icon),
            color=color,fill=T, width=4)
    
  })
  output$HTNInfo <-renderInfoBox({
    htn_yn <- ifelse(data()$G1E_BP_SYS>=140 | data()$G1E_BP_DIA>=90,1,0)
    htn_message <- ifelse(data()$G1E_BP_SYS>=140 | data()$G1E_BP_DIA>=90,'고혈압이 의심됩니다!',
                          ifelse(data()$G1E_BP_SYS>=130 | data()$G1E_BP_DIA>=85,'주의하세요.','양호합니다.'))
    htn_icon <- ifelse(data()$G1E_BP_SYS>=140 | data()$G1E_BP_DIA>=90,'fa-solid fa-land-mine-on',
                       ifelse(data()$G1E_BP_SYS>=130 | data()$G1E_BP_DIA>=85,'fa-solid fa-triangle-exclamation',
                              'fa-regular fa-thumbs-up'))
    color <- ifelse(data()$G1E_BP_SYS>=140 | data()$G1E_BP_DIA>=90,'red',
                    ifelse(data()$G1E_BP_SYS>=130 | data()$G1E_BP_DIA>=85,'orange','light-blue'))
    infoBox('고혈압',htn_message,
            icon = tags$i(class=htn_icon),
            color=color,fill=T, width=4)
  })
  output$METSInfo <-renderInfoBox({
    ifg_criteria <- ifelse(data()$G1E_FBS>=100,1,0)
    bp_criteria <- ifelse((data()$G1E_BP_SYS>=130 | data()$G1E_BP_DIA>=85),1,0)
    hdl_criteria <- ifelse( (data()$SEX=='남성' & data()$G1E_HDL<40 |
                               data()$SEX=='여성' & data()$G1E_HDL<50),1,0)
    wc_criteria <- ifelse((data()$SEX=='남성' & data()$G1E_WSTC>=95 |
                             data()$SEX=='여성' & data()$G1E_WSTC>=85),1,0)
    tg_criteria <- ifelse(data()$G1E_TG>=150,1,0)
    mets_criteria_sum <- sum(ifg_criteria, bp_criteria, hdl_criteria, wc_criteria, tg_criteria)
    mets_yn <- ifelse(mets_criteria_sum>=3,1,0)
    mets_message <- ifelse(mets_criteria_sum>=3,'대사증후군이 의심됩니다!',
                           ifelse(mets_criteria_sum>=2,'주의하세요.','양호합니다.'))
    mets_icon <- ifelse(mets_criteria_sum>=3,'fa-solid fa-land-mine-on',
                        ifelse(mets_criteria_sum>=2,'fa-solid fa-triangle-exclamation','fa-regular fa-thumbs-up'))
    color <- ifelse(mets_criteria_sum>=3,'red',
                    ifelse(mets_criteria_sum>=2,'orange','light-blue'))
    infoBox('대사증후군',mets_message,
            icon = tags$i(class=mets_icon),
            color=color,fill=T, width=4)
  })
  

  # 건강나이 위험도 ---------------------------------------------------------------------
  
  ba <- reactive({
    dbs<-data.frame(data())
    db<-dbs%>%select(-c(NAME, intercept))
    db[is.na(db)]<-0
    biologic_age<-Robert_BA(db)
    biologic_age
  })
  
  baca <- reactive({
    round(ba()-user_age(),1)
  })
  
    output$statusMsg <- renderUI({
      if(baca()<0){
        msg <- HTML('건강한 상태를 유지해봐요!')
      } else if (baca()==0){
        msg <- HTML('건강한 삶을 위해 노력해봐요!')
      } else {
        if(data()$SEX=='남성'){
          filename <- '1_hr_table.csv'
        } else {
          filename <- '2_hr_table.csv'
        }
        hr <- fread(paste0('hr/',filename)); 
        target_idx <- hr[age>=user_age(),.I[1L],by=disease]$V1;
        hr_table <- hr[target_idx]
        diseases <- c('mace','brain','cancer','t2dm','htn')
        disease <- c('당뇨', '심혈관질환','뇌혈관질환','암','고혈압')
        x <- lapply(1:5,function(x){
          # space <- paste0(rep('',5-nchar(disease[x])),collapse = ' '); print(space)
          assign(paste0(diseases[x],'_hr'), hr_table[disease==diseases[x],hr])
          paste0('▶ ',disease[x], ' 발생 <strong>',
                 round(baca()* (get(paste0(diseases[x],'_hr'))-1)*100,1), 
                 '</strong>% 증가')
        })
        msg <- HTML(paste0(x, sep='<br/>'))
      }
      msg
    })
  
  output$statusGIF <- renderUI({
    if(baca()>0) filename <- 'jordy_crying.gif'
    else if (baca()==0) filename <- 'jordy_react.gif'
    else filename <- 'jordy_congrat.gif'
    tags$img(src=filename, height='30%',width='30%', align='center')
  })
  output$haResult <- renderUI({
    ttl <- ifelse(baca()>0, 
                  paste0('실제 나이보다 <strong>',baca(),'세</strong> 많습니다!'),
            ifelse(baca()==0, 
                   paste0('실제 나이와 건강 나이가 같습니다!'),
                  paste0('실제 나이보다 <strong>', abs(baca()),'세 </strong> 젊습니다!')))
    HTML(ttl)
  })
    
  # 건강 나이 그래프 ---------------------------------------------------------------
  output$haPlot <- renderEcharts4r({
    HealthAgeDT <- data.table(variable=c('나이'),
                              `건강 나이` = as.numeric(round(ba(),1)),
                              `실제 나이`= as.numeric(user_age()))
    p <- HealthAgeDT %>% 
    e_chart(x=variable) %>% 
      e_bar(serie = `건강 나이`, barWidth=40,
            itemStyle=list(borderRadius=c(0,4,4,0))) %>% 
      e_bar(serie=`실제 나이`, barWidth=40,
            itemStyle=list(borderRadius=c(0,4,4,0))) %>% 
      e_flip_coords() %>%
      e_legend(show = F) %>% 
      e_labels(position = 'right',
               fontSize=15,
               fontWeight='bold',
               formatter = htmlwidgets::JS(
                 "function(params){
                    return(params.value[0]+'세')
                 }"
               )) %>% 
      e_tooltip(formatter = htmlwidgets::JS("
                          function(params) {
                          return ('<strong>' + params.seriesName +': ' + params.value[0] + '세' +'</strong>')}
                                            ")) 
    if(HealthAgeDT$`건강 나이`> HealthAgeDT$`실제 나이`) {
      p %>% e_color(c('#FF463C','#FDB3AF'))
    } else {
        p %>% e_color( c('#79AFFF','#D6EAF8')) 
      }
  })
  
  
  # 건강점수 그래프 ---------------------------------------------------------------
  
  output$hsPlot <- renderEcharts4r({
    healthScore() %>% 
      e_charts(variable) %>% 
      # e_bar(total, barGap='-100%', 
      #       silent=T,
      #       color='#D6EAF8') %>% 
      e_bar(score_all, color='#79AFFF',
            showBackground=T) %>% 
      # e_pictorial(total, symbol='rect', 
      #             symbolBoundingData=1000,
      #             animationDuration=0, 
      #             color='#D6EAF8') %>% 
      e_legend(show=F) %>%
      e_labels(position='insideRight',
               color = '#FFFFFF',
               fontSize=15,
               fontWeight='bold',
               formatter=htmlwidgets::JS(
                  "function(params){
                      if(params.seriesName==='score_all'){
                        return(params.value[0]);
                      } else {return ''};
                  }
                "
      ))%>% 
      e_flip_coords() %>% 
      e_tooltip(formatter = htmlwidgets::JS(
        "function(params){
          if(params.seriesName==='score_all'){
            return(params.name + ' 건강점수: ' + params.value[0]+'점');
          }
        }"
      )) %>% 
      # e_color(c('#79AFFF','#D6EAF8')) %>%
      e_y_axis(axisLabel = list(fontSize=15))
  })
  

# 건강점수 게이지 ----------------------------------------------------------------
  health_score_target <- c('cancer','mace','illness','death','total')
  health_hist <- paste0(health_score_target,'Hist')
  health_vars <- c('암','심뇌혈관','건강상태유지','사망','통합')
  health_ranks <-paste0(health_score_target,'Rank')
  
  output$sigh <- renderUI({
    baca <- round(ba()-user_age(),1)
    tags$img(src='jordy_sigh.gif', height='30%',width='30%', align='center')
  })
  lapply(1:5, \(i){
    output[[health_ranks[i]]] <- renderUI({
      prob <- calcTopProb(data=healthScore(),
                          sex = user_sex(),
                          age = user_age(),
                          disease = health_score_target[i],
                          vars=health_vars[i])
      HTML(paste0(user_name(),'님의 ', health_vars[i],' 건강점수는 상위 ',
                  '<strong>',round(100-prob,1), '%</strong> 입니다.'))
    })
    # histo chart
    output[[health_hist[i]]] <- renderPlot({
      # print(user_sex())
      # print(user_age())
      # print(healthScore() %>% head())
      histoChart(sex = user_sex(),
                 age = user_age(),
                 data = healthScore(),
                 disease = health_score_target[i],
                 vars=health_vars[i])
    })
  })
  
  
  healthDisease <- reactive({
    input$tabtab3
  })
  observe({
    if(healthDisease() %like% 'cancer'){
      output$cancerMsg <- renderUI({
        uiOutput('cancerRank')
      })
      output$cancerPercent <- renderUI({
        plotOutput('cancerHist',height='300px')
      })
    } else if (healthDisease() %like% 'mace') {
      output$maceMsg <- renderUI({
        uiOutput('maceRank')
      })
      output$macePercent <- renderUI({
        plotOutput('maceHist',height='300px')
      })
    } else if (healthDisease() %like% 'illness') {
      output$illnessMsg <- renderUI({
        uiOutput('illnessRank')
      })
      output$illnessPercent <- renderUI({
        plotOutput('illnessHist',height='300px')
      })
    } else if (healthDisease() %like% 'death') {
      output$deathMsg <- renderUI({
        uiOutput('deathRank')
      })
      output$deathPercent <- renderUI({
        plotOutput('deathHist',height='300px')
      })
    } else if (healthDisease() %like% 'total') {
      output$totalMsg <- renderUI({
        uiOutput('totalRank')
      })
      output$totalPercent <- renderUI({
        plotOutput('totalHist',height='300px')
      })
    }
  })
})

