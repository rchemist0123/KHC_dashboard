shiny::shinyUI(
  dashboardPage(
    skin = 'blue-light',
    dashboardHeader(
      title='KHC HRA',
      tags$li(class='dropdown', style='padding: 8px;',
              shinyauthr::logoutUI(id='logout',
                                   label = '로그아웃'
                                   ))
    ),
    dashboardSidebar(
      shinyjs::useShinyjs(),
      minified = T, 
      collapsed = T,
      width = 200,
      sidebarMenuOutput('sidebar')
      ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(
        tags$script(src="https://kit.fontawesome.com/097e732ef8.js"),
        tags$link(rel='stylesheet', type='text/css', href='style.css')
      ),
      tags$style(
        ".loginDiv btn {color:#fede22;}"
      ),
      div(
        class='loginDiv',
        shinyauthr::loginUI(
          id='login',
          title = h4(class='text-center','사용자 인증'),
          tags$br(),
            error_message = '아이디 또는 패스워드가 올바르지 않습니다.',
          cookie_expiry = 1,
          login_title = '로그인',
          user_title = '사용자 ID',
          pass_title = '패스워드 PW'
        )
      ),
      tabItems( 
       
        tabItem(tabName = 'Checkup',
                div(id='form',
                    fluidRow(
                      tabsetPanel(
                        id = 'tabtab',
                        type = 'hidden',
                        # Panel 0 ---------------------------------------------
                        tabPanel(
                          title='인삿말',
                          value='intro',
                          box(
                            title=strong('카카오 헬스케어 건강 테스트'),
                            width=12,
                            status='warning',
                            collapsed = F,
                            tags$br(),
                            div(
                              style='text-align: center;',
                              tags$h4(
                                '카카오헬스케어 건강나이 파일럿 시스템에 오신 것을 환영합니다.'
                              ),
                            ),
                            tags$br(),
                            div(
                              id='gif',
                              style='text-align: center;',
                              img(src='jordy_hi.gif',align='center')
                            ),
                            tags$br(),
                            div(
                              style='text-align: center;',
                              actionButton('start',
                                           label = '시작하기',
                                           class='btn-warning btn-lg')

                            ),
                            tags$br()
                          )
                        ),
                        tabPanel(
                          title="건강검진 기록",
                          value='db',
                          box(
                            title=strong('건강검진 데이터 선택하기'),
                            width=12,
                            class='warning',
                            collapsible = T,
                            tags$br(),
                            h3('건강검진 데이터를 선택해주세요.'),
                            tags$br(),
                            DTOutput('records'),
                            tags$br()
                          )
                        ),
                        # survey page -------------------------------------------
                        tabPanel(
                          title = '인적사항',
                          value = 'baseline',
                          box(
                            title=strong('1. 인적사항'),
                            width=12,
                            status = 'warning',
                            collapsible = T,
                            tags$br(),
                            dateInput('dates',label='검진일자', 
                                      value= as.character(Sys.Date()),autoclose = T),
                            textInput('name','이름',value='홍길동', placeholder = '성함을 입력하세요.'),
                            selectInput('sex','성별',
                                        choices = c('남성','여성'),
                                        selected = NULL),
                            numericInput('age','나이(세)', value=39),
                            tags$br(),
                            div(
                              style='text-align:center',
                              actionButton('getData',
                                           '기록 가져오기',
                                           width = '150px',
                                           class='btn-warning btn-lg',
                                           icon = tags$i(class='fa-solid fa-database')),
                              actionButton('writeData',
                                           width = '150px',
                                           '직접 작성하기',
                                           class='btn-warning btn-lg',
                                           icon = tags$i(class='fa-solid fa-pen'))
                            )
                          ),
                        ),
                        # Panel 2-----------------------------------------------
                        tabPanel(
                          title = '건강 검진 정보',
                          value='checkup',
                          box(
                            title=strong("2. 건강 검진 정보"),
                            collapsible = T,
                            status = 'warning',
                            width = 12,
                            tags$br(),
                            strong(tags$h3('계측검사')),
                            tags$h4('비만'),
                            numericInput('bmi','체질량지수(BMI)', value=27),
                            numericInput('wc','허리 둘레(cm)', value=90),
                            tags$h4('고혈압'),
                            numericInput('sbp','수축기 혈압(mmHg)', value=130),
                            numericInput('dbp','이완기 혈압(mmHg)', value=85),
                            
                            tags$br(),
                            tags$hr(),
                            strong(tags$h3('혈액검사')),
                            tags$h4('당뇨'),
                            numericInput('fbs','공복 혈당(mg/dL)', value=126),
                            tags$br(),
                            tags$h4('빈혈'),
                            numericInput('hgb','혈색소(g/dL)', value=13),
                            tags$br(),
                            tags$h4('이상지질혈증'),
                            numericInput('tg','중성지방(트리글리세라이드, mg/dL)', value=50),
                            numericInput('totChol','총콜레스테롤(mg/dL)', value=200),
                            numericInput('ldlChol','LDL콜레스테롤(mg/dL)', value=140),
                            numericInput('hdlChol','HDL콜레스테롤(mg/dL)', value=60),
                            tags$br(),
                            tags$h4('간장질환'),
                            numericInput('ast','AST(SGOT)(U/L)', value=20),
                            numericInput('alt','ALT(SGPT)(U/L)', value=20),
                            numericInput('ggt','감마지티피(V-GTP)(U/L)', value=20),
                            tags$br(),
                            tags$line(),
                            tags$h4('신장질환'),
                            numericInput('cr','혈청크레아티닌(mg/dL)', value=1.1),
                            tags$br()
                          )
                        ),
                        # Panel 3 -----------------------------------------------
                        tabPanel(
                          title = '생활 습관',
                          value='lifestyle',
                          box(title=strong('3. 생활 습관'),
                              collapsible = T,
                              status = 'warning',
                              tags$br(),
                              width = 12,
                              strong(tags$h3('1.흡연')),
                              selectInput('smk','흡연 경험', 
                                          choices = c('한 번도 흡연한 적 없음',
                                                      '과거 흡연했으나 현재는 안함',
                                                      '현재 흡연 중')),
                              tags$hr(),
                              strong(tags$h3('2.음주')),
                              numericInput('drk_freq','1주일 음주 빈도', value=3,min=0,max = 7),
                              numericInput('drk_amt','평균 1회 음주량 (소주 1잔)', value=5, min=0),
                              tags$hr(),
                              strong(tags$h3('3.운동')),
                              tags$h4('1) 저강도 운동: 걷기'),
                              numericInput('pa_walk_freq','1주일 걷기 수',value=3,min=0, max=7),
                              numericInput('pa_walk_amt', '평균 걷는 시간(단위: 분)', value=20, min=0),
                              tags$br(),
                              tags$h4('2) 중강도 운동'),
                              tags$h6('빠르게 걷기, 자전거 타기, 요가 등'),
                              numericInput('pa_mid_freq','1주일 중강도 운동 횟수',value=2, min=0, max=7),
                              numericInput('pa_mid_amt', '평균 중강도 운동 시간(단위: 분)', value=10, min=0),
                              tags$br(),
                              tags$h4('3) 고강도 운동'),
                              tags$h6(' 조깅, 빠르게 자전거 타기, 웨이트 트레이닝, 등산, 수영, 축구, 테니스 등'),
                              numericInput('pa_high_freq','1주일 고강도 운동 횟수',value=0, min=0, max=7),
                              numericInput('pa_high_amt', '평균 고강도 운동 시간(단위: 분)', value=0, min=0),
                              tags$br()
                              ),
                          
                        ),
                        # Panel 4----------------------------------------------
                        tabPanel(
                          
                          title = '가족력',
                          value='familyHistory',
                          box(title=strong('4. 가족력'),
                              tags$br(),
                              tags$p('해당되는 곳에 체크해주세요.'),
                              tags$p('가족력 범위: 부모, 형제/자매 (2촌)'),
                              tags$br(),
                              status = 'warning',
                              collapsible = T,
                              width = 12,
                              shinyWidgets::prettyCheckbox('fhx_hptn', '가족력: 고혈압',
                                                           status = 'warning',
                                                           outline=T,
                                                           icon=icon('check'),
                                                           animation = 'jelly'),
                              shinyWidgets::prettyCheckbox('fhx_dm', '가족력: 당뇨',
                                                           status = 'warning',
                                                           outline=T,
                                                           icon=icon('check'),
                                                           animation = 'jelly'),
                              shinyWidgets::prettyCheckbox('fhx_hdise', '가족력: 심장질환',
                                                           status = 'warning',
                                                           outline=T,
                                                           icon=icon('check'),
                                                           animation = 'jelly'),  
                              shinyWidgets::prettyCheckbox('fhx_strk', '가족력: 뇌졸중',
                                                           status = 'warning',
                                                           outline=T,
                                                           icon=icon('check'),
                                                           animation = 'jelly')
                          )
                        )
                      ),
                    column(
                      width = 12,
                      align='center',
                      div(style='display:inline-block',
                        div(style="display:inline-block",
                            conditionalPanel(condition="input.tabtab != 'login' &
                                             input.tabtab != 'intro' &
                                             input.tabtab != 'baseline'",
                             actionButton('toPrev','이전',style="material-flat",
                                          class='btn-lg'),
                              ),
                            width=6),
                        div(style="display:inline-block",
                            conditionalPanel(condition="input.tabtab != 'familyHistory' &
                                                input.tabtab != 'intro' &
                                             input.tabtab != 'baseline' &
                                             input.tabtab != 'db' &
                                             input.tabtab != 'login'",
                              actionButton('toNext','다음',style="material-flat",
                                           class='btn-lg')
                              ),
                            ),
                        div(style='display:inline-block',
                            conditionalPanel(condition = "input.tabtab == 'familyHistory'",
                                             actionButton('toStart','처음',
                                                          style="material-flat",
                                                          icon=tags$i(class='fa-solid fa-house'),
                                                          class='btn-lg')
                            )
                        )
                      ),
                      tags$br(),
                      tags$br(),
                      div(style='display:inline-block',
                          conditionalPanel(condition = "input.tabtab == 'familyHistory'",
                                           actionButton('reset','초기화',
                                                        style="material-flat",
                                                        icon=tags$i(class='fa-solid fa-trash-can'),
                                                        class='btn-reset btn-lg')
                          )
                      ),
                      div(style='display:inline-block',
                        conditionalPanel(condition = "input.tabtab == 'familyHistory' |
                                          input.tabtab =='db'",
                          actionButton(id='write',
                                       '확인',style="material-flat",
                                       class='btn-warning btn-lg',
                                       icon=tags$i(class='fa-solid fa-floppy-disk'),
                                       style="color: #FFFFFF;")
                        )
                      ),
                    )
                    )
                ),
        ),
        # dashboard page ---------------------------------------
        tabItem(tabName = 'Dashboard', 
          h2(textOutput('subjectInfo')),
          fluidRow(
            box(
              width=8,
              solidHeader = T,
              title=strong('건강검진 정보'),
              collapsible = T,
              status = 'warning',
              echarts4rOutput('checkBar')
            ),
            box(
              width=4,
              title=strong('나의 만성질환 점수'),
              solidHeader = T,
              status = 'warning',
              tabsetPanel(
                id='tabtab2',
                type = 'pills',
                tabPanel(
                  title='당뇨',
                  value='t2dmScore',
                  tags$br(),
                  div(
                    class='gaugeBox',
                    style='text-align:center;',
                    uiOutput('jordySigh1'),
                    tags$br(),
                    tags$br(),
                    h4(uiOutput('t2dmMsg')),
                    uiOutput('t2dmPercent')
                  ),
                ),
                tabPanel(
                  title='고혈압',
                  value='htnScore',
                  tags$br(),
                  div(
                    style='text-align:center',
                    uiOutput('jordySigh2'),
                    tags$br(),
                    tags$br(),
                    h4(uiOutput('htnMsg')),
                    uiOutput('htnPercent')
                  ),
                ),
                tabPanel(
                  title='대사증후군',
                  value='metsynScore',
                  tags$br(),
                  div(
                    class='gaugeBox',
                    style='text-align:center',
                    uiOutput('jordySigh3'),
                    tags$br(),
                    tags$br(),
                    h4(uiOutput('metsynMsg')),
                    uiOutput('metsynPercent')
                  ),
                )
              )
            )
          ),
          fluidRow(
            box(
              width=12,
              title=strong('나의 건강나이'),
              collapsible = T,
              solidHeader = T,
              status = 'warning',
              column(
                width=8,
                echarts4rOutput('haPlot'),
              ),
              column(
                width=4,
                div(
                  tags$br(),
                  style='text-align:center;',
                  uiOutput('statusGIF'),
                  h3(uiOutput('haResult')),
                ),
                tags$br(),
                div(
                  style='text-align: left; padding-left: 20%', #
                  h4(uiOutput('statusMsg')),
                )
              )
            ),
          ),
          fluidRow(
            box(
              width=8,
              title=strong('나의 건강점수'),
              solidHeader = T,
              collapsible = T,
              status = 'warning',
              echarts4rOutput('hsPlot')
              # sidebar = boxSidebar(
              #   id='graphBox',
              #   p('항목')
              # )
            ),
            box(
              width=4,
              title=strong('나의 건강점수 등수'),
              solidHeader = T,
              status='warning',
              tabsetPanel(
                id='tabtab3',
                type = 'pills',
                tabPanel(
                  title='암',
                  value='cancer_score',
                  tags$br(),
                  div(
                    h5(uiOutput('cancerMsg')),
                    uiOutput('cancerPercent'),
                    style='text-align:center'
                  )
                ),
                tabPanel(
                  title='심뇌혈관',
                  value='mace_score',
                  tags$br(),
                  div(
                    style='text-align:center',
                    h5(uiOutput('maceMsg')),
                    uiOutput('macePercent')
                  )
                ),
                tabPanel(
                  title='건강상태유지',
                  value='illness_score',
                  tags$br(),
                  div(
                    style='text-align:center',
                    h5(uiOutput('illnessMsg')),
                    uiOutput('illnessPercent')
                  )
                ),
                tabPanel(
                  title='사망',
                  value='death_score',
                  tags$br(),
                  div(
                    style='text-align:center',
                    h5(uiOutput('deathMsg')),
                    uiOutput('deathPercent')
                  )
                ),
                tabPanel(
                  title='통합',
                  value='total_score',
                  tags$br(),
                  div(
                    style='text-align:center',
                    h5(uiOutput('totalMsg')),
                    uiOutput('totalPercent')
                  )
                )
              )
            )
          )
          # fluidRow(
          #   box(
          #     width=12,
          #     title=strong('건강검진 테이블'),
          #     collapsible = T,
          #     status = 'warning',
          #     DTOutput('records')
          #   ),
          # ),
      )
    )
  )
  )
)

