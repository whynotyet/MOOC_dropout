library(dplyr)
library(reshape2)
library(psych)
library(foreach)
library(ggplot2)

### Analysis of correlations between dropout reasons
setwd("~/Dropbox/r-code/dropout_reasons/data_all_pre3/")
files = dir()
Mcor = list()
i = 1
N = 0

for(f in files){
  print(f)
  dat = read.csv(f)
  n = sum(!is.na(dat[,12]))
  print(n)
  if(n > 30) {
    m = data.frame(cor(dat[,12:25], use="pair"))
    m[is.na(m)] = 0
    Mcor[[i]] = m
    N = N + n
    i = i + 1
  }
}

CombMcor = Reduce('+', Mcor)/length(Mcor)
N
fa.parallel(CombMcor, n.obs=N)
fa.diagram(fa(CombMcor, 5, n.obs=N))
fa(CombMcor, 5, n.obs=N)
plot(princomp(CombMcor, cor=T))
summary(princomp(CombMcor, cor=T))


### Analysis of correlations between demographics/geo and dropout reasons
library(foreach)
setwd("~/Dropbox/r-code/dropout_reasons/data_all_pre3/")
files = dir()

dat = foreach(f=files, .combine=rbind) %do% {
  dat = read.csv(f)
  temp = dat[,c(4,6,12:25,30,33)]
  temp$course = f
  subset(temp, did_dropout==1)
}  

# Geolocation
dat$ip_address = as.character(dat$ip_address)
# write.table(dat$ip_address, file="../../ip_lookup/ips.csv", col.names=F, row.names=F, sep=",", quote=F)
ips = read.csv("../../ip_lookup/ips_geo.csv")
ips$continent = as.character(ips$continent)
ips[is.na(ips$continent),]$continent = "NAmerica"
ips[ips$continent=="SA",]$continent = "SAmerica"
ips[ips$continent%in%c("--","unk"),]$continent = NA
plot(ips$longitude,ips$latitude, col=as.numeric(factor(ips$continent)))
dat$country = ips$country_name
dat$continent = ips$continent
dat$continent = factor(dat$continent, levels=c("NAmerica","EU","OC","AF","AS","SAmerica"))
rm(ips)

a = dat %>% filter(!is.na(continent) & (is.na(gender)|gender<2)) %>% group_by(continent=continent%in%c("NAmerica","EU","OC")) %>% summarise(
  exam_diff = mean(rfd.asn_exam_too_difficult, na.rm=T),
  deadline = mean(rfd.cant_keep_up_deadlines, na.rm=T),
  cred_val = mean(rfd.credentials_value , na.rm=T),
  format = mean(rfd.didnt_enjoy_online_format, na.rm=T),
  explore = mean(rfd.exploring_only, na.rm=T),
  all_done = mean(rfd.learned_all_i_wanted, na.rm=T),
  explain = mean(rfd.material_explanation, na.rm=T),
  no_chal = mean(rfd.not_challenging, na.rm=T),
  no_fit = mean(rfd.not_what_im_looking_for, na.rm=T),
  time = mean(rfd.requires_too_much_time, na.rm=T),
  late = mean(rfd.started_late, na.rm=T),
  tech = mean(rfd.tech_difficulties, na.rm=T),
  advanced = mean(rfd.too_advanced, na.rm=T),
  confused = mean(rfd.videos_confusing, na.rm=T)
)

ggplot(melt(a, id.vars="gender"), aes(factor(gender), value)) + geom_bar(stat="identity") + facet_wrap(~variable, scales="free")
ggplot(melt(a, id.vars="continent"), aes(continent, value)) + geom_bar(stat="identity") + facet_wrap(~variable)

# 4 ip
# 6 did dropout
# 12:25 rfd
# 30 age
# 33 gender


################################################
### Analysis of dropout by prior characteristics
################################################

setwd("~/Dropbox/r-code/dropout_reasons/data_all_pre3/")
files = dir()
dat = read.csv(files[7])
names(dat)

dat = foreach(f=files, .combine=rbind) %do% {
  d = read.csv(f)[,c(1,4,30:68)]
  d$planned_weekly_hours = ifelse(is.na(d$planned_weekly_hours), NA, as.numeric(gsub(",", ".", d$planned_weekly_hours)))
  d$course = f
  d[!duplicated(d$common_user_id),] # this could be improved
}
rm(d)
str(dat)

# Videos watched, Grade
boxplot(dat$num_videos_watched ~ dat$course)
dat[is.na(dat$num_videos_watched),]$num_videos_watched = 0
dat = dat %>% group_by(course) %>% 
  mutate(
    upper = quantile(num_videos_watched[num_videos_watched!=0], .9, na.rm=T),
    upperScore60 = quantile(avg_score[avg_score!=0], .60, na.rm=T),
    upperScore80 = quantile(avg_score[avg_score!=0], .80, na.rm=T),
    upperQuiz = quantile(num_quizzes_attempted[num_quizzes_attempted!=0], .95, na.rm=T)
  ) %>% mutate(
    prop_videos_watched = ifelse(num_videos_watched/upper>1, 1, num_videos_watched/upper),
    watched30 = num_videos_watched/upper >= .3,
    watched50 = num_videos_watched/upper >= .5,
    watched80 = num_videos_watched/upper >= .8,
    score60th = !is.na(avg_score) & (avg_score >= upperScore60),
    score80th = !is.na(avg_score) & (avg_score >= upperScore80),
    prop_quiz_attempted = ifelse(num_quizzes_attempted/upperQuiz>1, 1, num_quizzes_attempted/upperQuiz),
    quiz30 = num_quizzes_attempted/upperQuiz >= .3,
    quiz50 = num_quizzes_attempted/upperQuiz >= .5,
    quiz80 = num_quizzes_attempted/upperQuiz >= .8
  )

# Geolocation
dat$ip_address = as.character(dat$ip_address)
table(grepl("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+", dat$ip_address))
dat[!grepl("[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+", dat$ip_address),]$ip_address = "113.197.8.238" #replace 2 with most frequent
# write.table(dat$ip_address, file="../../ip_lookup/ips.csv", col.names=F, row.names=F, sep=",", quote=F)
ips = read.csv("../../ip_lookup/ips_geo.csv")
ips$continent = as.character(ips$continent)
ips[is.na(ips$continent),]$continent = "NAmerica"
ips[ips$continent=="SA",]$continent = "SAmerica"
ips[ips$continent%in%c("--","unk"),]$continent = NA
plot(ips$longitude,ips$latitude, col=as.numeric(factor(ips$continent)))
dat$country = ips$country_name
dat$continent = ips$continent
dat$continent = factor(dat$continent, levels=c("NAmerica","EU","OC","AF","AS","SAmerica"))
rm(ips)

# Courses started
dat[!is.na(dat$courses_started) & dat$courses_started=="",]$courses_started = NA
dat$courses_started3 = dat$courses_started
dat[!is.na(dat$courses_started3) & (dat$courses_started3=="4+" | dat$courses_started3>3),]$courses_started3 = "4 or more"
dat[!is.na(dat$courses_started3) & !dat$courses_started3%in%c(0,"4 or more"),]$courses_started3 = "1-3"
dat$courses_started3 = factor(dat$courses_started3)
table(dat$courses_started3)

# Demographics
dat$isFemale = as.numeric(dat$gender==1)
dat$ageNorm = as.numeric(scale(ifelse(dat$age>90, 85, dat$age)))
dat$eduBachlorHigher = ifelse(is.na(dat$highest_degree), NA, as.numeric(dat$highest_degree%in%(5:9)))
dat$hours3 = cut_number(dat$planned_weekly_hours, 3)
dat$hasPriorExp = as.numeric(dat$prior_experience==1)
dat$intentAll = as.numeric(dat$intentions==4)

# cor(dat$hasPriorExp,dat$intentAll,use='pair')
# table(dat$course,dat$highest_degree)
# table(dat$courses_started>=4)
# table(dat$planned_weekly_hours>0, dat$course)
# table(dat$intentions==4)

names(dat)
full = dat
save(full, file="all_pre_comb.Rdata")
dat = dat[,c(42,48:64)]
save(dat, file="sub_pre_comb.Rdata")

# Missing values...
summary(dat)
library(mice)
keep_for_imp = apply(is.na(subset(dat, select=continent:intentAll)), 1, sum) <= 3
imp = mice(dat[keep_for_imp,], m=3)
# save(imp, file="../sub_pre_imp.Rdata")

# check covariate correlations
imp$data$continent
with(imp, round(cor(cbind(isFemale, ageNorm, eduBachlorHigher, hasPriorExp, intentAll, hours3, courses_started3)),2))

# fit the models

library(lme4)

# imp$data %>% group_by(course) %>% summarise(mean(isFemale, na.rm=T)>.4)
# fewWomen = imp$data$course %in% c("Engineering_CVX101_Winter2014.csv","Engineering_Nano_Summer2014.csv","Engineering_Networking_Winter2014.csv","Engineering_Solar_Fall2013.csv","automata-002.csv")
# manyWomen = imp$data$course %in% c("GlobalHealth_IWHHR_Summer2014.csv","GlobalHealth_WomensHealth_Winter2014.csv","HumanitiesSciences_EP101_Environmental_Physiology.csv","HumanitiesSciences_NCP101_Winter2014.csv","organalysis-002.csv","organalysis-003.csv")
# femaleInstructor = imp$data$course %in% c("")

m_w30 = with(imp, glmer(watched30 ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_w50 = with(imp, glmer(watched50 ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_w80 = with(imp, glmer(watched80 ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_s60 = with(imp, glmer(score60th ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_s80 = with(imp, glmer(score80th ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_q30 = with(imp, glmer(quiz30 ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_q50 = with(imp, glmer(quiz50 ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))
m_q80 = with(imp, glmer(quiz80 ~ isFemale + ageNorm + eduBachlorHigher + hasPriorExp + intentAll + hours3 + courses_started3 + continent + (1|course), family=binomial))

class(m_w30$analyses[[1]])=class(m_w30$analyses[[2]])=class(m_w30$analyses[[3]]) = "lmerMod"
class(m_w50$analyses[[1]])=class(m_w50$analyses[[2]])=class(m_w50$analyses[[3]]) = "lmerMod"
class(m_w80$analyses[[1]])=class(m_w80$analyses[[2]])=class(m_w80$analyses[[3]]) = "lmerMod"

class(m_q30$analyses[[1]])=class(m_q30$analyses[[2]])=class(m_q30$analyses[[3]]) = "lmerMod"
class(m_q50$analyses[[1]])=class(m_q50$analyses[[2]])=class(m_q50$analyses[[3]]) = "lmerMod"
class(m_q80$analyses[[1]])=class(m_q80$analyses[[2]])=class(m_q80$analyses[[3]]) = "lmerMod"

class(m_s60$analyses[[1]])=class(m_s60$analyses[[2]])=class(m_s60$analyses[[3]]) = "lmerMod"
class(m_s80$analyses[[1]])=class(m_s80$analyses[[2]])=class(m_s80$analyses[[3]]) = "lmerMod"

res = data.frame(rbind(
  summary(pool(m_w30,"a")),
  summary(pool(m_w50,"a")),
  summary(pool(m_w80,"a")),
  summary(pool(m_q30,"a")),
  summary(pool(m_q50,"a")),
  summary(pool(m_q80,"a")),
  summary(pool(m_s60,"a")),
  summary(pool(m_s80,"a")) ))

# colnames(res) = c("est","se","z","p")
# res$coef = names(fixef(m_w30))
res$y = factor(rep(c(">30% videos", ">50% videos", ">80% videos", ">30% assessments", ">50% assessments", ">80% assessments", ">60% pctile grade", ">80% pctile grade"), each=15))
res$y = factor(res$y, levels=c(">30% videos", ">50% videos", ">80% videos", ">60% pctile grade", ">30% assessments", ">50% assessments", ">80% assessments", ">80% pctile grade"))

res$coef2 = factor(rep(c("Intercept", "Gender:F", "Age (norm.)", "Educ.:Bachelor+", "Prior Exp.", "Intent:All", "Hours:2-4", "Hours:4+ ", "Prior MOOCs:1-3", "Prior MOOCs:4+ ", "Geo:Europe", "Geo:Oceania", "Geo:Africa", "Geo:Asia", "Geo:S.America"), 8))
res$coef2 = factor(res$coef2, levels=rev(c("Intercept", "Gender:F", "Age (norm.)", "Educ.:Bachelor+", "Prior Exp.", "Intent:All", "Hours:2-4", "Hours:4+ ", "Prior MOOCs:1-3", "Prior MOOCs:4+ ", "Geo:Europe", "Geo:Oceania", "Geo:Africa", "Geo:Asia", "Geo:S.America")))

res$sign = as.numeric(sign(res$est-1.96*res$se) == sign(res$est+1.96*res$se))
res[res$sign!=0,]$sign = ifelse(res[res$sign!=0,]$est < 0, -1, 1)

library(ggplot2)

write.csv(res, file="../paper/pred_coefs.csv", row.names=F)

ggplot(subset(res, coef2!="Intercept"), aes(coef2, exp(est), 
      ymin=exp(est-1.96*se), ymax=exp(est+1.96*se), color=factor(sign))) + 
  geom_hline(xintercept=0) + geom_pointrange() + facet_wrap(~y, ncol=4) + coord_flip() + 
  theme_minimal() + theme(legend.position="none",axis.title=element_text(vjust=0)) + 
  scale_color_manual(values=c('red','grey40','darkgreen')) +
  labs(y="Estimated change in probability (95% CIs)", x="")




# 
# sat = data.frame(matrix(c(c(1215,1.68477366255,1.11712016321,
# 1210,2.12066115702,0.905622233046,
# 1215,2.04362139918,0.955248902493,
# 440,2.13409090909,0.877870145144),
# c(579,2.18134715026,0.777443831138,
# 648,2.30401234568,0.719127523399,
# 607,2.37726523888,0.751282550051,
# 280,2.37857142857,0.76967393043),
# c(99,2.05050505051,0.880469771004,
# 56,1.94642857143,0.87463549259,
# 57,2.21052631579,0.613784409984,
# 69,2.24637681159,0.749991248286),
# c(67,1.77611940299,1.0483945868,
# 72,1.97222222222,0.927544880948,
# 22,2.13636363636,0.814385130326,
# 6,2.33333333333,0.7453559925),
# c(60,1.71666666667,1.12681951626,
# 25,1.84,0.924337600663,
# 54,1.75925925926,0.921656825047,
# 40,1.975,0.757875319561),
# c(62,1.87096774194,0.958558577942,
# 63,2.09523809524,0.810923160282,
# 75,2.04,0.855414129725,
# 56,2.21428571429,0.699854212224),
# c(100,1.96,1.23223374406,
# 141,2.33333333333,0.680703293657,
# 560,2.43571428571,0.683590460998,
# 195,2.51794871795,0.696832318009),
# c(6,1.83333333333,0.37267799625,
# 11,1.81818181818,0.38569460792,
# 13,1.53846153846,0.745796901141,
# 10,2.1,0.943398113206)), ncol=3, byrow=T))
# colnames(sat)=c('n','m','sd')
# sat$video=c(.25,.5,.75,1)
# sat$crs=rep(1:8,each=4)
# sat$se = sat$sd/sqrt(sat$n-1)
# ggplot(sat, aes(video, m, ymin=m-se, ymax=m+se, color=factor(crs))) + geom_pointrange() + geom_line() + theme_bw() + labs(x='video',y='satisfaction')
# 
# satTot = sat %>% group_by(video) %>% summarise(m=weighted.mean(m,n), N=sum(n), sd=sqrt(weighted.mean(sd^2, n)), se=sd/sqrt(N-1))
# ggplot(satTot, aes(video, m, ymin=m-se, ymax=m+se)) + geom_pointrange() + geom_line() + theme_bw()+ labs(x='video',y='satisfaction')
