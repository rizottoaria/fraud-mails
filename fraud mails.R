
###Code for get data from billing for fraud cases

#I get alerts from fraud system Fraud view and I'd like to get also information about these sip-numbers from billing and predbilling to make right desision if it is real fraud.

#I'll connect to oracle databases via r.

library(RJDBC)
library(lubridate)
library(gmailr)
library(stringr)
library(mailR)
library(htmlTable)
library(htmlTable)
Sys.setlocale("LC_TIME", "english") ##english months

drv <- JDBC("oracle.jdbc.driver.OracleDriver","Path of ojdbc5.jar")
connbill<- dbConnect(drv, url="tns, login,pass")
connbillamur<- dbConnect(drv, url="tns_name, login,pass")
connbillhab<- dbConnect(drv, url="tns_name, login,pass")
connbillkam<- dbConnect(drv, url="tns_name, login,pass")
connbillmag<- dbConnect(drv, url="tns_name, login,pass")
connbillyak<- dbConnect(drv, url="tn_name, login,pass")
connbillsahal<- dbConnect(drv, url="tns, login,pass")
conn <- dbConnect(drv, url="tns, login,pass")
# dataframe with regions and bases' names
dataframe<-data.frame("Region"= c('725', '727', '749', '741', '728', '765', '779', '787', '714'), billing=c('connbill', 'connbillhab', 'connbillmag', 'connbillkam', 'connbillamur', 'connbillsahal', 'connbillhab', 'connbillhab', 'connbillyak'),filial=c('PF', 'HF', 'MF', 'KF', 'AF', 'SF', 'HF', 'HF', 'Sakhatelecom')) ##find right billing

MessageId<-messages(search = 'habar', num_results = NULL, label_ids = NULL,include_spam_trash = NULL, page_token = NULL, user_id = "me")
size<-MessageId[[1]]$resultSizeEstimate ## letters from habar

t<-message(MessageId[1][[1]]$messages[[1]]$id)$snippet;
number<-substr(t,regexpr('[=]', t)+1,nchar(t));
region<-as.character(subset(dataframe$Billing,dataframe$Region==substr(number,2,4)));
numbers<- as.character(dbGetQuery(get(region),paste0("select distinct phone from t_services t,t_users u
                                                     where u.user_id=t.user_id and account in 
                                                     (select distinct account from t_services t,t_users u
                                                     where u.user_id=t.user_id
                                                     and t.PHONE='",substr(number,2,nchar(number)),"' and t.date_end is null)
                                                     and phone is not null
                                                     and t.date_end is null and phone not like '%RT%'")))
##Данные из биллинга за последние 2 месяца по звонкам на МН
last_period<-as.data.frame(dbGetQuery(get(region),paste0("select g.talkdate, g.billing_id, g.phonefrom, g.codetown, g.phoneto, m.mts_zone_id, m.dlit, m.summ, z.name, g.payer_phone 
                                                         from t_mts_sig g, t_mts_res m, t_mts_zone z 
                                                         where 1 = 1  and g.billing_id = ", as.numeric(format(ymd(Sys.Date()) %m+% months(-2),"%Y%m"))," and m.mtr_id = g.mtr_id 
                                                         and g.phonefrom in (",ifelse(nchar(as.array(numbers))==10,numbers,substr(str_replace_all
                                                                                                                                  (str_replace_all(numbers,'\"',"'"),'\", \"',"','"),3,nchar(str_replace_all(str_replace_all(numbers,'\"',"'"),'\", \"',"','"))-1)),") and m.mts_zone_id = z.mts_zone_id and z.isintl 
                                                         = 'Y' order by g.talkdate, m.mts_zone_id")))

last_period<-rbind(last_period,as.data.frame(dbGetQuery(get(region),paste0("select g.talkdate, g.billing_id, g.phonefrom, g.codetown, g.phoneto, m.mts_zone_id, m.dlit, m.summ, 
                                                                           z.name, g.payer_phone from t_mts_sig g, t_mts_res m, t_mts_zone z 
                                                                           where 1 = 1  and g.billing_id = ", as.numeric(format(ymd(Sys.Date()) %m+% months(-1),"%Y%m"))," and 
                                                                           m.mtr_id = g.mtr_id
                                                                           and g.phonefrom in (",ifelse(nchar(as.array(numbers))==10,numbers,substr(str_replace_all
                                                                                                                                                    (str_replace_all
                                                                                                                                                      (numbers,'\"',"'"),'\", \"',"','"),3,nchar(str_replace_all(str_replace_all(numbers,'\"',"'"),'\", \"',"','"))-1)),") and 
                                                                           m.mts_zone_id = z.mts_zone_id and z.isintl = 'Y' order by g.talkdate, m.mts_zone_id"))))
name<-as.data.frame(dbGetQuery(get(region),paste0("select distinct account, u.name from t_services t,t_users u where u.user_id=t.user_id and t.PHONE='",substr(number,2,nchar
                                                                                                                                                               (number)), "' and t.date_end is null")))## find abonents

one<-paste0("to_date('", ymd(Sys.Date()), "', 'YYYY-MM-DD')")
two<-paste0("to_date('", ymd(Sys.Date()) %m+% days(1), "', 'YYYY-MM-DD')")
three<-str_replace_all(str_replace_all(str_replace_all(numbers,'\"',"'"),'\", \"',"','"),"'4","'74")

predbill<-as.data.frame(dbGetQuery(conn, paste0( "select * from core.fact ft where  1=1 and ft.dtb between " , one, " and ", two, " and addra in  (",ifelse(nchar(as.array(numbers))==10,paste0("'",number,"'"),substr(three,3,nchar(three)-1)), ") and telb like '810%'")))
html_body <- paste0("<html><head>
                    <style>
                    body{font-family:Calibri, sans-serif;}
                    table{border-left:1px solid #000000;border-top:1px solid #000000;}
                    table th{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:12px; font-weight:bold; margin: 0px; padding-left: 5px; padding-right: 5px; margin: 0px;}
                    table td{border-right:1px solid #000000;border-bottom:1px solid #000000;font-size:12px; font-weight:normal; margin: 0px; padding-left: 5px; padding-right: 5px; margin: 0px;}
                    </style>
                    </head><body><p> Абонент:</p>","<br>",htmlTable(name, rnames = F),"<br><p> Вызовы на МН ранее:</p>",htmlTable(unique(last_period$NAME), rnames = F), "<br>",
                    htmlTable(last_period, rnames = F), "<br><h3>Предбиллинг:<br>",htmlTable(predbill, rnames = F),
                    "</body></html>")


ifelse(as.numeric(Sys.time()-strptime(message(MessageId[1][[1]]$messages[[1]]$id)[["payload"]][["headers"]][[18]][["value"]],  "%a, %d %b %Y %H:%M:%S %z"),units="mins")<15, {send.mail(from="my_mail@mail.com",to=c('my_mail1@mail.com"', 'fraud@dv.rt.ru'),subject='Fraud review',body=html_body,html=TRUE,authenticate=TRUE,smtp = list(host.name="SDVDR002EXCA2.dv.rt.ru", user.name = "lapuckayaov@dv.rt.ru", passwd = "password", ssl = F),encoding = "utf-8",send=TRUE)},"")


