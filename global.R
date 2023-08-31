require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyalert)
library(shinyjs)
require(shinyauthr)
require(sodium)
require(shinyWidgets)
require(shinyvalidate)
require(DT)
library(data.table)
require(rcompanion)
require(echarts4r)
require(data.table)
require(ggplot2)
require(shinyalert)
require(dplyr)


# wd <- paste0(getwd(),'/shiny/workoutDashboard/db.sqlite')
con <- dbConnect(SQLite(), dbname = 'db.sqlite')
# dbExecute(con, "drop table checkup;")
# dbExecute(con, "
#           CREATE TABLE if not exists checkup (
#           id INTEGER PRIMARY KEY AUTOINCREMENT,
#           dates TEXT,
#           NAME TEXT,
#           AGE NUMBER,
#           SEX TEXT,
#           G1E_BMI NUMBER,
#           G1E_WSTC NUMBER,
#           G1E_BP_SYS NUMBER,
#           G1E_BP_DIA NUMBER,
#           G1E_FBS NUMBER,
#           G1E_HGB NUMBER,
#           G1E_TOT_CHOL NUMBER,
#           G1E_LDL NUMBER,
#           G1E_HDL NUMBER,
#           G1E_SGOT NUMBER,
#           G1E_SGPT NUMBER,
#           G1E_GGT NUMBER,
#           G1E_TG NUMBER,
#           G1E_CRTN NUMBER,
#           smk TEXT,
#           drk_freq NUMBER,
#           drk_amt NUMBER,
#           pa_walk_freq NUMBER,
#           pa_walk_amt NUMBER,
#           pa_mid_freq NUMBER,
#           pa_mid_amt NUMBER,
#           pa_high_freq NUMBER,
#           pa_high_amt NUMBER,
#           Q_FHX_HTDZ1 NUMBER,
#           Q_FHX_DM1 NUMBER,
#           Q_FHX_HTN1 NUMBER,
#           Q_FHX_STK1 NUMBER
#           )
# ")
# dbSendQuery(con,
#                "INSERT INTO checkup
#             (dates,NAME,
#           AGE,SEX,G1E_BMI,G1E_WSTC, G1E_BP_SYS, G1E_BP_DIA, G1E_FBS, G1E_HGB,
#          G1E_TOT_CHOL,G1E_LDL,G1E_HDL,G1E_SGOT,
#           G1E_SGPT,G1E_GGT,G1E_TG,G1E_CRTN,smk,drk_freq,
#           drk_amt,pa_walk_freq,pa_walk_amt,pa_mid_freq,pa_mid_amt,pa_high_freq,
#           pa_high_amt,Q_FHX_HTDZ1,Q_FHX_DM1,Q_FHX_HTN1,Q_FHX_STK1)
#             values(
# '2022-11-28', '홍길동', 29, '남성',24, 70,120,80,80,10,180,80,100,50,50,50,50,50,
# 0,0,0,3,20,3,30,3,20,0,0,0,0);"
#                )
# # 
# setDT(dbGetQuery(con, 'select * from checkup'))
