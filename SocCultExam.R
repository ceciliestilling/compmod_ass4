

##### LOAD PACKAGES AND CSV #####
library(pacman)
library(tidyverse)
library(corrplot)
library(brms)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(tidytext)
library(R.utils)
library(wordcloud)
library(viridis)
library(wesanderson)
pacman::p_load(
  rethinking,
  brms,
  tidyverse,
  bayesplot,
  viridis,
  ordinal
)
color_scheme_set("viridis")



setwd("C:/Users/cecil/OneDrive/AU/SocKult/Exam paper/Mental Health and Peer Interactions.csv")


data_eng <- read.csv("Mental Health and Peer Interactions.csv", header = TRUE, sep = ",",encoding = "UTF-8")
data_dan <- read.csv("Mentalt Helbred og Interaktioner.csv", header = TRUE, sep = ",",encoding = "UTF-8")
  
##### DATA CLEANING #####
data_eng_clean <- data_eng %>%
  rename(#demographics
         time = Tidsstempel,
         age = What.is.your.age.,
         gender = What.gender.do.you.identify.as.,
         education_deg = What.is.the.highest.degree.or.level.of.education.you.have.completed.,
         psyc_education = Have.you.received.some.kind.of.education.relating.to.psychiatric.disorders..either.specific.disorders.or.in.general.,
         expl_psyc_education = What.kind.of.education.relating.to.psychiatric.disorders.have.you.received...e.g..in.school..through.treatment..courses.for.relatives.etc..,
         peers_w_MI = Do.you.know.anyone.who.suffers.from.mental.illness., 
         choose_peers_w_MI = Who.do.you.know.who.suffers.from.mental.illness.,
         expl_peers_w_MI = If.you.have.more.answers.to.the.question.above.that.are.not.included.in.the.choices..please.feel.free.to.specify.here,
         diagnosis = Do.you.or.have.you.had.one.or.more.psychiatric.diagnoses.,
         choose_diagnosis = Select.the.diagnoses.that.you.have.or.think.you.might.have.,
         expl_diagnosis = If.you.have.more.other.answers.to.the.question.above.that.are.not.included.in.the.choices..please.feel.free.to.specify.here,
         
         #How much does your own mental health affect your life? LIKERT SCALE (THEIR OWN MENTAL HEALTH SITUATION)
         MH_social = How.much.would.you.say.your.mental.health.affects.your.ability.to.function.in.your.social.life.,
         MH_professional = How.much.would.you.say.your.mental.health.affects.your.ability.to.function.in.your.professional.life..school.or.work..,
         MH_private = How.much.would.you.say.your.mental.health.affects.your.ability.to.function.in.your.private.life.,
         MH_daily = How.much.would.you.say.your.mental.health.affects.your.ability.to.function.in.your.daily.life...getting.groceries..cleaning.,
         expl_MH = If.you.have.any.comments.on.how.mental.illness.affects.your.life..please.write.so.here,
         
         #Their actual interactions with peers with mental health issues
         act_int_peers = When.you.interact.with.people..how.often.do.you.interact.with.people.who.are.experiencing.or.have.experienced.mental.illness.that.you.know.of.,
         act_int_peers_support = How.often.do.you.go.to.people.with.mental.illness.for.emotional.support.,
         act_int_peers_disr_own_MH = How.often.do.you.disregard.your.own.mental.health.trying.to.help.others.,
         expl_act_int_peers = If.you.have.anything.to.add.to.this.section..please.write.so.here,
         
         #Statements
         int_MI_understood = Interacting.with.people.with.mental.illness.makes.me.feel.understood,
         int_MI_sim_understood = Interacting.with.people.with.similar.mental.health.issues.as.myself.makes.me.feel.understood..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_understood = If.you.have.any.comments.to.these.statements..please.write.so.here.,
         
         int_MI_guilt = Interacting.with.people.with.mental.illness.makes.me.feel.guilty.about.my.own.mental.health,
         int_MI_sim_guilt = Interacting.with.people.with.similar.mental.health.issues.as.myself.makes.me.feel.guilty.about.my.own.mental.health..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_guilt = If.you.have.any.comments.to.these.statements..please.write.so.here..1,
         
         int_MI_avoid_own = When.interacting.with.people.with.mental.illness.I.avoid.bringing.up.my.own.problems.since.I.m.afraid.to.make.their.situation.worse,
         int_MI_sim_avoid_own = When.interacting.with.people.with.similar.mental.health.issues.as.myself.I.avoid.bringing.up.my.own.problems.since.I.m.afraid.to.make.their.situation.worse..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_avoid_own = If.you.have.any.comments.to.these.statements..please.write.so.here..2,
         
         int_MI_supported = Interacting.with.people.with.mental.illness.makes.me.feel.supported,
         int_MI_sim_supported = Interacting.with.people.with.similar.mental.issues.as.myself.makes.me.feel.supported..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_supported = If.you.have.any.comments.to.these.statements..please.write.so.here..3,
         
         int_MI_exhausting = Interacting.with.people.with.mental.illness.is.hard.exhausting.for.my.own.mental.health,
         int_MI_sim_exhausting = Interacting.with.people.with.similar.mental.health.issues.as.myself.is.hard.exhausting.for.my.own.mental.health..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_exhausting = If.you.have.any.comments.to.these.statements..please.write.so.here..4,
         
         int_MI_learn = I.learn.a.lot.from.interacting.with.people.who.have.mental.illness,
         int_MI_sim_learn = I.learn.a.lot.from.interacting.with.people.with.similar.mental.issues.as.myself..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_learn = If.you.have.any.comments.to.these.statements..please.write.so.here..5,
         
         int_MI_responsibility = I.feel.like.it.is.my.responsibility.to.make.my.peers.who.have.mental.illness.happy,
         int_MI_sim_responsibility = I.feel.like.it.is.my.responsibility.to.make.my.peers.who.have.similar.mental.health.issues.as.myself.happy..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_responsibility = If.you.have.any.comments.to.these.statements..please.write.so.here..6,
         
         int_MI_blame_myself = Interacting.with.people.with.mental.illness.makes.me.blame.myself.for.their.mental.state,
         int_MI_sim_blame_myself = Interacting.with.people.with.similar.mental.health.issues.as.myself.makes.me.blame.myself.for.their.mental.state..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_blame_myself = If.you.have.any.comments.to.these.statements..please.write.so.here..7,
         
         int_MI_compare_neg = When.interacting.with.people.with.mental.illness.I.tend.to.compare.myself.to.them.in.a.way.that.affects.me.negatively,
         int_MI_sim_compare_neg = When.interacting.with.people.with.similar.mental.issues.as.myself.I.tend.to.compare.myself.to.them.in.a.way.that.affects.me.negatively..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_compare_neg = If.you.have.any.comments.to.these.statements..please.write.so.here..8,
         
         int_MI_compare_pos = When.interacting.with.people.with.mental.illness.I.tend.to.compare.myself.to.them.in.a.way.that.affects.me.positively,
         int_MI_compare_sim_pos = When.interacting.with.people.with.similar.mental.health.issues.as.myself.I.tend.to.compare.myself.to.them.in.a.way.that.affects.me.positively..If.you.have.not.experienced.any.mental.health.issues.yourself..then.just.skip.this.question.,
         expl_MI_compare_pos = If.you.have.anything.to.add..explanations..other.statements.or.thoughts.you.think.need.to.be.added...please.write.so.here)
         
         


data_dan_clean <- data_dan %>%
  rename(#demographics
        time = Tidsstempel,
        age = Hvad.er.din.alder.,
        gender = Hvilket.køn.identificerer.du.dig.som.,
        education_deg = Hvad.er.det.højeste.niveau.af.uddannelse..du.har.gennemført.,
        psyc_education = Har.du.haft.nogen.form.for.undervisning.uddannelse.i.psykiske.lidelser..enten.om.specifikke.lidelser.eller.generelt.,
        expl_psyc_education = Hvilken.form.for.undervisning.i.psykiske.lidelser.har.du.modtaget...f.eks..skole..gennem.behandling..kurser.for.pårørende.osv..,
        peers_w_MI = Kender.du.nogen..der.har.en.psykisk.lidelse., 
        choose_peers_w_MI = Hvem.kender.du..som.har.en.psykisk.lidelse.,
        expl_peers_w_MI = Hvis.du.har.flere.svar.til.ovenstående.spørgsmål.eller.uddybende.kommentarer..er.du.velkommen.til.at.udfylde.her,
        diagnosis = Har.du.eller.har.du.før.haft.en.eller.flere.psykiatriske.diagnoser.,
        choose_diagnosis = Venligst.afkryds.de.diagnoser..du.har..tror.du.har.eller.har.haft,
        expl_diagnosis = Ved..Andre...uddyb.gerne.her..Her.kan.du.også.tilføje.eventuelle.kommentarer,
        
        #How much does your own mental health affect your life? LIKERT SCALE (THEIR OWN MENTAL HEALTH SITUATION)
        MH_social = Hvor.meget.påvirker.dit.mentale.helbred.din.evne.til.at.fungere.i.dit.sociale.liv.,
        MH_professional = Hvor.meget.påvirker.dit.mentale.helbred.din.evne.til.at.fungere.i.dit.professionelle.liv..skole.eller.arbejde..,
        MH_private = Hvor.meget.påvirker.dit.mentale.helbred.din.evne.til.at.fungere.i.dit.private.liv.,
        MH_daily = Hvor.meget.påvirker.dit.mentale.helbred.din.evne.til.at.fungere.i.dit.daglige.liv...indkøb..rengøring..osv..,
        expl_MH = Hvis.du.har.yderligere.kommentarer.til..hvordan.dit.mentale.helbred.påvirker.dit.liv..er.du.velkommen.til.at.uddybe.her,
        
        #Their actual interactions with peers with mental health issues
        act_int_peers = Når.du.interagerer.med.folk..hvor.ofte.interagerer.du.så.med.folk..der.oplever.eller.har.oplevet.psykisk.lidelse..så.vidt.du.ved.,
        act_int_peers_support = Hvor.ofte.går.du.til.folk.med.psykiske.lidelser..når.du.skal.have.følelsesmæssig.støtte.,
        act_int_peers_disr_own_MH = Hvor.ofte.ignorerer.du.dit.eget.mentale.helbred.for.at.hjælpe.andre.med.psykiske.lidelser.,
        expl_act_int_peers = Hvis.du.har.noget.at.tilføje.til.denne.sektion..er.du.velkommen.til.at.udfylde.her,
        
        #Statements
        int_MI_understood = At.interagere.med.folk.med.psykiske.lidelser.får.mig.til.at.føle.mig.forstået,
        int_MI_sim_understood = At.interagere.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..får.mig.til.at.føle.mig.forstået..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_understood = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her,
        
        int_MI_guilt = At.interagere.med.folk.med.psykiske.lidelser.får.mig.til.at.føle.skyld.over.mit.eget.mentale.helbred,
        int_MI_sim_guilt = At.interagere.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..får.mig.til.at.føle.skyld.over.mit.eget.mentale.helbred..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_guilt = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.1,
        
        int_MI_avoid_own = Når.jeg.interagerer.med.folk.med.psykiske.lidelser..undgår.jeg.at.nævne.mine.egne.problemer..da.jeg.er.bange.for.at.forværre.deres.situation,
        int_MI_sim_avoid_own = Når.jeg.interagerer.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..undgår.jeg.at.nævne.mine.egne.problemer..da.jeg.er.bange.for.at.forværre.deres.situation..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_avoid_own = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.2,
        
        int_MI_supported = At.interagere.med.folk.med.psykiske.lidelser.får.mig.til.at.føle.mig.støttet,
        int_MI_sim_supported = At.interagere.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..får.mig.til.at.føle.mig.støttet..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_supported = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.3,
        
        int_MI_exhausting = At.interagere.med.folk.med.psykiske.lidelser.er.hårdt.udmattende.for.mit.eget.mentale.helbred,
        int_MI_sim_exhausting = At.interagere.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..er.hårdt.udmattende.for.mit.eget.mentale.helbred..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_exhausting = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.4,
        
        int_MI_learn = Jeg.lærer.meget.af.at.interagere.med.folk.med.psykiske.lidelser,
        int_MI_sim_learn = Jeg.lærer.meget.af.at.interagere.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_learn = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.5,
        
        int_MI_responsibility = Jeg.føler..at.det.er.mit.ansvar.at.opmuntre.mine.peers...som.har.psykiske.lidelser,
        int_MI_sim_responsibility = Jeg.føler..at.det.er.mit.ansvar.at.opmuntre.mine.peers...som.har.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer...hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_responsibility = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.6,
    
        int_MI_blame_myself = At.interagere.med.folk.med.psykiske.lidelser..får.mig.til.at.bebrejde.mig.selv.for.deres.psykiske.tilstand,
        int_MI_sim_blame_myself = At.interagere.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..får.mig.til.at.bebrejde.mig.selv.for.deres.psykiske.tilstand..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_blame_myself = Hvis.du.har.kommentarer.til.disse.udsagn..er.du.velkommen.til.at.udfylde.her.7,
    
        int_MI_compare_neg = Når.jeg.interagerer.med.folk.med.psykiske.lidelser..har.jeg.en.tendens.til.at.sammenligne.mig.selv.med.dem.på.en.måde..der.påvirker.mig.negativt,
        int_MI_sim_compare_neg = Når.jeg.interagerer.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..har.jeg.en.tendens.til.at.sammenligne.mig.selv.med.dem.på.en.måde..der.påvirker.mig.negativt..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_compare_neg = Hvis.du.har.kommentarer.til.disse.udsagn...er.du.velkommen.til.at.udfylde.her,
          
        int_MI_compare_pos = Når.jeg.interagerer.med.folk.med.psykiske.lidelser..har.jeg.en.tendens.til.at.sammenligne.mig.selv.med.dem.på.en.måde..der.påvirker.mig.positivt,
        int_MI_compare_sim_pos = Når.jeg.interagerer.med.folk.med.psykiske.lidelser..der.minder.om.mine.egne.psykiske.problemer..har.jeg.en.tendens.til.at.sammenligne.mig.selv.med.dem.på.en.måde..der.påvirker.mig.positivt..hvis.du.ikke.selv.har.oplevet.psykiske.problemer.af.nogen.art..kan.du.springe.dette.spørgsmål.over.,
        expl_MI_compare_pos = Hvis.du.har.noget.at.tilføje..forklaringer..andre.udsagn.eller.tanker.du.finder.relevante..er.du.velkommen.til.at.udfylde.her)


#merge the two datasets
data_all <- rbind(data_eng_clean, data_dan_clean)
 


#code women female, men male, and other 2
# male_terms <- c("Male","Mand")
# female_terms <- c("Female","Kvinde")
  
#data_all_test <- data_all %>%
#  mutate(gender = if_else(gender %in% male_terms,0,
#                          if_else(gender %in% female_terms,1,2)))

data_all_test <- data_all %>%
  mutate(gender = if_else(gender %in% "Mand","male",
                          if_else(gender %in% "Male","male", 
                          if_else(gender %in% "Kvinde","female",
                          if_else(gender %in% "Female","female","other")))))

#code 1 for yes and 0 for no - andet  = 2 (foretrækker ikke at svare)
#psyc_education
# data_all_test <- data_all_test %>%
#   mutate(psyc_education = if_else(psyc_education %in% "No",0,
#                           if_else(psyc_education %in% "Nej",0,
#                           if_else(psyc_education %in% "Yes",1,
#                           if_else(psyc_education %in% "Ja", 1,2)))))

data_all_test <- data_all_test %>%
  mutate(psyc_education = if_else(psyc_education %in% "No","No",
                                  if_else(psyc_education %in% "Nej","No",
                                          if_else(psyc_education %in% "Yes","Yes",
                                                  if_else(psyc_education %in% "Ja", "Yes","Other")))))

#code 1 for yes and 0 for no - andet  = 2 (foretrækker ikke at svare/don't know)
#peers_w_MI
# data_all_test <- data_all_test %>%
#   mutate(peers_w_MI = if_else(peers_w_MI %in% "No",0,
#                       if_else(peers_w_MI %in% "Nej",0,
#                       if_else(peers_w_MI %in% "Yes",1,
#                       if_else(peers_w_MI %in% "Ja", 1,2)))))

data_all_test <- data_all_test %>%
  mutate(peers_w_MI = if_else(peers_w_MI %in% "No","No",
                              if_else(peers_w_MI %in% "Nej","No",
                                      if_else(peers_w_MI %in% "Yes","Yes",
                                              if_else(peers_w_MI %in% "Ja", "Yes","Don't know")))))



#code 1 for yes and 0 for no - tror  = 2 , andet=3(foretrækker ikke at svare/don't know)
#diagnosis
# data_all_test <- data_all_test %>%
#   mutate(diagnosis = if_else(diagnosis %in% "No",0,
#                      if_else(diagnosis %in% "Nej",0,
#                      if_else(diagnosis %in% "Yes",1,
#                      if_else(diagnosis %in% "Ja", 1,
#                      if_else(diagnosis %in% "Ingen diagnose, men jeg tror, jeg lider af en eller flere psykiske lidelser",2,
#                      if_else(diagnosis %in% "No diagnosis, but I think I suffer from one or more psychiatric disorders",2,3)))))))
# 

data_all_test <- data_all_test %>%
  mutate(diagnosis = if_else(diagnosis %in% "No","No",
                             if_else(diagnosis %in% "Nej","No",
                                     if_else(diagnosis %in% "Yes","Yes",
                                             if_else(diagnosis %in% "Ja", "Yes",
                                                     if_else(diagnosis %in% "Ingen diagnose, men jeg tror, jeg lider af en eller flere psykiske lidelser","Might have",
                                                             if_else(diagnosis %in% "No diagnosis, but I think I suffer from one or more psychiatric disorders","Might have","Other")))))))





#age
data_all_test <- data_all_test %>%
  mutate(age = if_else(age %in% "Under 15 år","<15",
               if_else(age %in% "Under 15 years old","<15",
               if_else(age %in% "15-24 år","15-24",
               if_else(age %in% "15-24 years old", "15-24",
               if_else(age %in% "25-34 år","25-34",
               if_else(age %in% "25-34 years old","25-34",
               if_else(age %in% "35-44 år","35-44",
               if_else(age %in% "35-44 years old","35-44",
               if_else(age %in% "45-54 år","45-54",
               if_else(age %in% "45-54 years old","45-54",
               if_else(age %in% "55-64 år","55-64",
               if_else(age %in% "55-64 years old","55-64",
               if_else(age %in% "65 eller ældre","65+",
               if_else(age %in% "65 or older","65+", "Prefer not to answer")))))))))))))))



#code distant and close relatives  and other 2

data_all_test <- data_all_test %>%
  mutate(choose_peers_w_MI = if_else(grepl("Close", data_all$choose_peers_w_MI),"close",
                             if_else(grepl("Tæt", data_all$choose_peers_w_MI),"close", 
                             if_else(grepl("Tætte", data_all$choose_peers_w_MI),"close",
                             if_else(grepl("partner", data_all$choose_peers_w_MI),"close",
                             if_else(grepl("Fjern", data_all$choose_peers_w_MI),"distant",
                             if_else(grepl("Fjerne", data_all$choose_peers_w_MI),"distant",
                             if_else(grepl("Acquaintances", data_all$choose_peers_w_MI),"distant", 
                             if_else(grepl("Bekendte", data_all$choose_peers_w_MI),"distant",
                             if_else(grepl("Distant", data_all$choose_peers_w_MI),"distant",
                             if_else(grepl("Other", data_all$choose_peers_w_MI),"other","other")))))))))))










#Individual diagnoses

#ADHD
data_all_test$adhd <- ifelse(grepl("ADHD",data_all_test$choose_diagnosis),1,0)

#ANXIETY
data_all_test$anxiety_eng <- ifelse(grepl("Anxiety",data_all_test$choose_diagnosis),1,0)
data_all_test$anxiety_dk <- ifelse(grepl("Angst",data_all_test$choose_diagnosis),1,0)
data_all_test$anxiety <- paste(data_all_test$anxiety_eng,data_all_test$anxiety_dk)
data_all_test$anxiety <- ifelse(grepl(1, data_all_test$anxiety),1,0)
data_all_test <- subset(data_all_test, select = -c(anxiety_dk, anxiety_eng))

#ASD
data_all_test$asd <- ifelse(grepl("Autism",data_all_test$choose_diagnosis),1,0)

#BIPOLAR
data_all_test$bipolar <- ifelse(grepl("Bipolar",data_all_test$choose_diagnosis),1,0)

#DEPRESSION
data_all_test$depression <- ifelse(grepl("depression",data_all_test$choose_diagnosis),1,0)

ggplot(data = data_all_test) +
  geom_bar(mapping = aes(x = depression, fill=depression))+ scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1"))

n <- nrow(data_all_test)  # Number of participants
(percent_depression <- table(data_all_test$depression)/n * 100)
(depression_test <- table(data_all_test$depression))

n <- nrow(data_all_test)  # Number of participants
(diagnosis_test <- table(data_all_test$diagnosis))



#EATING DISORDER
data_all_test$eating_disorder_eng <- ifelse(grepl("eating",data_all_test$choose_diagnosis),1,0)
data_all_test$eating_disorder_dk <- ifelse(grepl("spiseforstyrrelse",data_all_test$choose_diagnosis),1,0)
data_all_test$eating_disorder <- paste(data_all_test$eating_disorder_eng,data_all_test$eating_disorder_dk)
data_all_test$eating_disorder <- ifelse(grepl(1, data_all_test$eating_disorder),1,0)
data_all_test <- subset(data_all_test, select = -c(eating_disorder_dk, eating_disorder_eng))

#OCD
data_all_test$ocd <- ifelse(grepl("OCD",data_all_test$choose_diagnosis),1,0)

#PTSD
data_all_test$ptsd <- ifelse(grepl("stress",data_all_test$choose_diagnosis),1,0)

#SCZ
data_all_test$scz_eng <- ifelse(grepl("Schizophrenia",data_all_test$choose_diagnosis),1,0)
data_all_test$scz_dk <- ifelse(grepl("Skizofreni",data_all_test$choose_diagnosis),1,0)
data_all_test$scz <- paste(data_all_test$scz_eng,data_all_test$scz_dk)
data_all_test$scz <- ifelse(grepl(1, data_all_test$scz),1,0)
data_all_test <- subset(data_all_test, select = -c(scz_dk, scz_eng))

#OTHER
data_all_test$other_eng <- ifelse(grepl("Other",data_all_test$choose_diagnosis),1,0)
data_all_test$other_dk <- ifelse(grepl("Andre",data_all_test$choose_diagnosis),1,0)
data_all_test$other <- paste(data_all_test$other_eng,data_all_test$other_dk)
data_all_test$other <- ifelse(grepl(1, data_all_test$other),1,0)
data_all_test <- subset(data_all_test, select = -c(other_dk, other_eng))






#######visualizations#######
color_scheme_set("viridis")

#distribution of who has one or more diagnoses
ggplot(data = data_all_test) +
  geom_bar(mapping = aes(x = diagnosis, fill=diagnosis))+ scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1"))
#percent
n <- nrow(data_all_test)  # Number of participants
(percent_diagnosis <- table(data_all_test$diagnosis)/n * 100)
#Might have: 5.36%
#Yes:       54.76%
#No:        39.88%



#distribution of gender
ggplot(data = data_all_test) +
  geom_bar(mapping = aes(x = gender, fill=gender))+ scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1"))

#percent
n <- nrow(data_all_test)  # Number of participants
(percent_gender <- table(data_all_test$gender)/n * 100)
  #female 89.29%
  #male 9.52%
  #other 1.19%


#distribution of age
ggplot(data = data_all_test) + 
  geom_bar(mapping = aes(x = age, fill=age))+ scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1", "#DE77AE", "#FFD92F", "#FF7F00"))
#percent
(percent_age <- table(data_all_test$age)/n * 100)
  #15-24: 21.43%
  #25-34: 26.79%
  #35-44: 22.02%
  #45-54: 21.43%
  #55-64:  7.14%
  #65+     1.19%



#display.brewer.pal(n = 8, name = 'PRGn')
#brewer.pal(n = 8, name = "Paired")

#distribution of psyc_education
ggplot(data = data_all_test) +
  geom_bar(mapping = aes(x = psyc_education, fill=psyc_education)) + scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1"))

#percent
(percent_psyc_education <- table(data_all_test$psyc_education)/n * 100)
  #Yes: 59.52%
  #No:  40.48%

#distribution of whether people know anyone with MI
ggplot(data = data_all_test) +
  geom_bar(mapping = aes(x = peers_w_MI, fill=peers_w_MI)) + scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1"))

#percent
(percent_peers_w_MI <- table(data_all_test$peers_w_MI)/n * 100)
  #Yes:        95.8%
  #No:         3.57%
  #Don't know: 0.6



#distribution of how close people are with people they know who have mental illness
ggplot(data = data_all_test) +
  geom_bar(mapping = aes(x = choose_peers_w_MI, fill=choose_peers_w_MI)) + scale_fill_manual(values=c("#9970AB", "#5AAE61", "#80CDC1", "#DE77AE", "#FFD92F", "#FF7F00"))

(percent_choose_peers <- table(data_all_test$choose_peers_w_MI)/n * 100)
  #close:   81.55%
  #distant: 9.52%
  #other:   8.93%




# Correlation matrix of items
cormat <- data_all_test %>%
  select(starts_with(c("int"))) %>%
  cor(., use = "pairwise.complete.obs")

corrplot(cormat, # correlation matrix
         order = "hclust", # hierarchical clustering of correlations
         addrect = 2) # number of rectangles to draw around clusters

# Activate the ggcorrplot package
library(ggcorrplot)

# Correlation matrix plot
ggcorrplot(cormat, # correlation matrix
           type = "lower", # print the lower part of the correlation matrix
           hc.order = TRUE, # hierarchical clustering of correlations
           lab = TRUE) # add correlation values as label


data_statements <- select(data_all_test, starts_with("int"))

data_statements <- na.omit(data_statements)
### Change Likert scores to factor and specify levels

data_statements$int_MI_understood = factor(data_statements$int_MI_understood,
                         levels = c("1", "2", "3", "4", "5", "6"),
                         ordered = TRUE)
library(psych)
library(likert)
headTail(data_statements)

str(data_statements)


result <- likert(data_statements)
plot(result)


#own mental health
data_own <- select(data_all, starts_with("MH_"))

data_own <- na.omit(data_own)
### Change Likert scores to factor and specify levels

data_own$MH_social = factor(data_own$MH_social,
                                           levels = c("1", "2", "3", "4", "5", "6"),
                                           ordered = TRUE)
library(psych)
library(likert)
headTail(data_own)

str(data_own)


result2 <- likert(data_own)
plot(result2)


#ordered categorical outcomes
simplehist(data_all_test$act_int_peers, xlim=c(1,6), xlab="act_int_peers")
#disccrete proportion of each response value
pr_k <- table(data_all_test$act_int_peers) / (nrow(data_all_test))

#cumsum converts to cumulative proportions
cum_pr_k <- cumsum(pr_k)
#plot
plot(1:6, cum_pr_k, type="b", xlab="act_int_peers", xlim=c(1,6), ylab="cumulative proportion", ylim=c(0,1))

logit <- function(x) log(x/(1-x)) #convenience function
round(lco <- logit(cum_pr_k), 2)
# 1     2     3     4     5     6
#-4.42 -1.61 -0.46  0.77  2.48  4.42

plot(1:6, lco, type="b", xlab="act_int_peers", xlim=c(1,6), ylab="log-cumulative odds")








##################MODEL TIME###########################


#####int_MI_guilt predicted by 1 + diagnosis#####
    #first model (m0): cumulative model with equal variances####
    f_guilt_by_diagnosis0 <- bf(int_MI_guilt ~ 1 + diagnosis)
    
    get_prior(f_guilt_by_diagnosis0,
              data_all_test, 
              family = cumulative("probit"))
    
    
    prior_guilt_by_diagnosis0 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
     p2 <- rnorm(10000, 0, 1)
    dens(p)
    dens(inv_logit(p))
    dens(p2)
    dens(inv_logit(p2))
    
    #setting a better prior
    prior_guilt_by_diagnosis0 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    
    
    # Testing the prior in the model
    m_guilt_by_diagnosis_prior0 <- brm(
      f_guilt_by_diagnosis0,
      data_all_test,
      family = cumulative("probit"),#using probit link function since I'm working with ordinal regression
      prior = prior_guilt_by_diagnosis0,
      sample_prior = "only"
    )
    
    pp_check(m_guilt_by_diagnosis_prior0, nsamples = 100)
    
    #testing the actual model
    m_guilt_by_diagnosis0 <- brm(
      formula = f_guilt_by_diagnosis0,
      data = data_all_test,
      family = cumulative("probit"),
      prior = prior_guilt_by_diagnosis0,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_guilt_by_diagnosis0)
    
    
    # Posterior predictive check
    pp_check(m_guilt_by_diagnosis0, nsamples = 100)
    ## Check the model for warnings
    m_guilt_by_diagnosis0

    hypothesis(m_guilt_by_diagnosis0,"diagnosisYes > 0")
    hypothesis(m_guilt_by_diagnosis0,"diagnosisNo > 0")
    plot(hypothesis(m_guilt_by_diagnosis0,"diagnosisYes > 0"))
    plot(hypothesis(m_guilt_by_diagnosis0,"diagnosisNo > 0"))
    plot(m_guilt_by_diagnosis0, "b_diagnosisNo", "b_diagnosisYes")
  
   
    
    
    mcmc_trace(m_guilt_by_diagnosis0, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    mcmc_rank_overlay(m_guilt_by_diagnosis0, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    
    marginal_effects(m_guilt_by_diagnosis0, "diagnosis", categorical = TRUE)
    conditional_effects(m_guilt_by_diagnosis0, "diagnosis", categorical = TRUE)
    #second model (m1):adjacent category model with equal variances and category specific effects####
    f_guilt_by_diagnosis1 <- bf(int_MI_guilt ~ 1 + cs(diagnosis))

    get_prior(f_guilt_by_diagnosis1,
              data_all_test, 
              family = acat("probit"))


    prior_guilt_by_diagnosis1 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 

    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))

    #setting a better prior
    prior_guilt_by_diagnosis1 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 



    
    # Testing the prior in the model
    m_guilt_by_diagnosis_prior1 <- brm(
      f_guilt_by_diagnosis1,
      data_all_test,
      family = acat("probit"),#using probit link function since I'm working with ordinal regression
      prior = prior_guilt_by_diagnosis1,
      sample_prior = "only"
    )
    
    pp_check(m_guilt_by_diagnosis_prior1, nsamples = 100)
    
    #testing the actual model
    m_guilt_by_diagnosis1 <- brm(
      formula = f_guilt_by_diagnosis1,
      data = data_all_test,
      family = acat("probit"),
      prior = prior_guilt_by_diagnosis1,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_guilt_by_diagnosis1)
    
    
    # Posterior predictive check
    pp_check(m_guilt_by_diagnosis1, nsamples = 100)
    ## Check the model for warnings
    m_guilt_by_diagnosis1
    
    
    color_scheme_set("viridis")
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisYes > 0"))
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisNo > 0"))
    # 
    # # 
    # mcmc_trace(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    # mcmc_rank_overlay(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    
    marginal_effects(m_guilt_by_diagnosis1, "diagnosis", categorical = TRUE)
    
    #
    
    #third model (m2): adjacent category model with equal variances#####
    f_guilt_by_diagnosis2 <- bf(int_MI_guilt ~ 1 + diagnosis)
    
    get_prior(f_guilt_by_diagnosis2,
              data_all_test, 
              family = acat("probit"))
    
    
    prior_guilt_by_diagnosis2 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    p2 <- rnorm(10000, 0, 1)
    dens(p)
    dens(inv_logit(p))
    dens(p2)
    dens(inv_logit(p2))
    #setting a better prior
    prior_guilt_by_diagnosis2 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    
    
    # Testing the prior in the model
    m_guilt_by_diagnosis_prior2 <- brm(
      f_guilt_by_diagnosis2,
      data_all_test,
      family = acat("probit"),#using probit link function since I'm working with ordinal regression
      prior = prior_guilt_by_diagnosis2,
      sample_prior = "only"
    )
    
    pp_check(m_guilt_by_diagnosis_prior2, nsamples = 100)
    
    #testing the actual model
    m_guilt_by_diagnosis2 <- brm(
      formula = f_guilt_by_diagnosis2,
      data = data_all_test,
      family = acat("probit"),
      prior = prior_guilt_by_diagnosis2,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_guilt_by_diagnosis2)
    
    
    # Posterior predictive check
    pp_check(m_guilt_by_diagnosis2, nsamples = 100)
    ## Check the model for warnings
    m_guilt_by_diagnosis2
    
    
    color_scheme_set("viridis")
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisYes > 0"))
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisNo > 0"))
    # 
    # # 
    # mcmc_trace(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    # mcmc_rank_overlay(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    
    marginal_effects(m_guilt_by_diagnosis2, "diagnosis", categorical = TRUE)
    

    #fourth model (m3): cumulative model with unequal variances.#####
    f_guilt_by_diagnosis3 <- bf(int_MI_guilt ~ 1 + diagnosis) +
      lf(disc ~ 0 + diagnosis, cmc = FALSE)
    
    get_prior(f_guilt_by_diagnosis3,
              data_all_test, 
              family = cumulative("probit"))
    
    prior_guilt_by_diagnosis3 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5"),
      prior(normal(0,1), class=b, coef=diagnosisNo, dpar=disc),
      prior(normal(0,1), class=b, coef=diagnosisYes, dpar=disc)) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    p2 <- rnorm(10000, 0,1)
    dens(p)
    dens(inv_logit(p))
    dens(p2)
    dens(inv_logit(p2))
    
    #setting a better prior
    prior_guilt_by_diagnosis3 <- c(
      prior(normal(0,1), class=b, coef=diagnosisYes),
      prior(normal(0,1), class=b, coef=diagnosisNo),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5"),
      prior(normal(0,1), class=b, coef=diagnosisNo, dpar=disc),
      prior(normal(0,1), class=b, coef=diagnosisYes, dpar=disc))
    
    
    # Testing the prior in the model
    m_guilt_by_diagnosis_prior3 <- brm(
      f_guilt_by_diagnosis3,
      data_all_test,
      family = cumulative("probit"),#using probit link function since I'm working with ordinal regression
      prior = prior_guilt_by_diagnosis3,
      sample_prior = "only"
    )
    
    pp_check(m_guilt_by_diagnosis_prior3, nsamples = 100)
    
    #testing the actual model
    m_guilt_by_diagnosis3 <- brm(
      formula = f_guilt_by_diagnosis3,
      data = data_all_test,
      family = cumulative("probit"),
      prior = prior_guilt_by_diagnosis3,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_guilt_by_diagnosis3)
    
    
    # Posterior predictive check
    pp_check(m_guilt_by_diagnosis3, nsamples = 100)
    ## Check the model for warnings
    m_guilt_by_diagnosis3
    
    
    color_scheme_set("viridis")
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisYes > 0"))
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisNo > 0"))
    # 
    # # 
    # mcmc_trace(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    # mcmc_rank_overlay(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    
    marginal_effects(m_guilt_by_diagnosis3, "diagnosis", categorical = TRUE)
    
    #Model comparison#####
    loo(m_guilt_by_diagnosis0, m_guilt_by_diagnosis1, m_guilt_by_diagnosis2, m_guilt_by_diagnosis3)
    loo_model_weights(m_guilt_by_diagnosis0, m_guilt_by_diagnosis1, m_guilt_by_diagnosis2, m_guilt_by_diagnosis3)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
                        
##### Act_int_peers ~ 1 + psyc_education #####
    #First model (m0): cumulative model with equal variances#####
    f_int_peers_by_psycedu0 <- bf(act_int_peers ~ 1 + psyc_education)
    
    get_prior(f_int_peers_by_psycedu0,
              data_all_test, 
              family = cumulative("probit"))
    
    
    prior_int_peers_by_psycedu0 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))
    
    #setting a better prior
    prior_int_peers_by_psycedu0 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    # Testing the prior in the model
    m_int_peers_by_psycedu_prior0 <- brm(
      f_int_peers_by_psycedu0,
      data_all_test,
      family = cumulative("probit"),
      prior = prior_int_peers_by_psycedu0,
      sample_prior = "only"
      )
    
    pp_check(m_int_peers_by_psycedu_prior0, nsamples = 100)
    
    #testing the actual model
    m_int_peers_by_psycedu0 <- brm(
      formula = f_int_peers_by_psycedu0,
      data = data_all_test,
      family = cumulative("probit"),
      prior = prior_int_peers_by_psycedu0,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_int_peers_by_psycedu0)
    
    
    # Posterior predictive check
    pp_check(m_int_peers_by_psycedu0, nsamples = 100)
    ## Check the model for warnings
    m_int_peers_by_psycedu0
    
    plot(hypothesis(m_int_peers_by_psycedu0,"psyc_educationYes > 0"))
    
    
    mcmc_trace(m_int_peers_by_psycedu0, pars = "b_psyc_educationYes")+ theme_classic()
    mcmc_rank_overlay(m_int_peers_by_psycedu0, pars = "b_psyc_educationYes") + theme_classic()
    
    marginal_effects(m_int_peers_by_psycedu0, "psyc_education", categorical = TRUE)



    #second model (m1):adjacent category model with equal variances and category specific effects#####
    f_int_peers_by_psycedu1 <- bf(act_int_peers ~ 1 + cs(psyc_education))
    
    get_prior(f_int_peers_by_psycedu1,
              data_all_test, 
              family = acat("probit"))
    
    
    prior_int_peers_by_psycedu1 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))
    
    prior_int_peers_by_psycedu1 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    # Testing the prior in the model
    m_int_peers_by_psycedu_prior1 <- brm(
      f_int_peers_by_psycedu1,
      data_all_test,
      family = acat("probit"),
      prior = prior_int_peers_by_psycedu1,
      sample_prior = "only"
    )
    
    pp_check(m_int_peers_by_psycedu_prior1, nsamples = 100)
    
    #testing the actual model
    m_int_peers_by_psycedu1 <- brm(
      formula = f_int_peers_by_psycedu1,
      data = data_all_test,
      family = acat("probit"),
      prior = prior_int_peers_by_psycedu1,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_int_peers_by_psycedu1)
    
    
    # Posterior predictive check
    pp_check(m_int_peers_by_psycedu1, nsamples = 100)
    ## Check the model for warnings
    m_int_peers_by_psycedu1
    
    plot(hypothesis(m_int_peers_by_psycedu1,"psyc_educationYes > 0"))
    plot(hypothesis(m_int_peers_by_psycedu1,"psyc_educationNo > 0"))
    
    
    mcmc_trace(m_int_peers_by_psycedu1, "bcs_psyc_educationYes[1]", "bcs_psyc_educationYes")+ theme_classic()
    mcmc_rank_overlay(m_int_peers_by_psycedu1, pars = "bcs_psyc_educationYes[1]", "bcs_psyc_educationYes")+ theme_classic()
    
    marginal_effects(m_int_peers_by_psycedu1, "psyc_education", categorical = TRUE)
    conditional_effects(m_int_peers_by_psycedu1, "psyc_education", categorical = TRUE)
    
    
    #third model (m2): adjacent category model with equal variances#####
    f_int_peers_by_psycedu2 <- bf(act_int_peers ~ 1 + psyc_education)
    
    get_prior(f_int_peers_by_psycedu2,
              data_all_test, 
              family = acat("probit"))
    
    
    prior_int_peers_by_psycedu2 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))
    
    prior_int_peers_by_psycedu2 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    # Testing the prior in the model
    m_int_peers_by_psycedu_prior2 <- brm(
      f_int_peers_by_psycedu2,
      data_all_test,
      family = acat("probit"),
      prior = prior_int_peers_by_psycedu2,
      sample_prior = "only"
    )
    
    pp_check(m_int_peers_by_psycedu_prior2, nsamples = 100)
    
    #testing the actual model
    m_int_peers_by_psycedu2 <- brm(
      formula = f_int_peers_by_psycedu2,
      data = data_all_test,
      family = acat("probit"),
      prior = prior_int_peers_by_psycedu2,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_int_peers_by_psycedu2)
    
    
    # Posterior predictive check
    pp_check(m_int_peers_by_psycedu2, nsamples = 100)
    ## Check the model for warnings
    m_int_peers_by_psycedu2
    
    plot(hypothesis(m_int_peers_by_psycedu2,"psyc_educationYes > 0"))
    plot(hypothesis(m_int_peers_by_psycedu2,"psyc_educationNo > 0"))
    
    
    mcmc_trace(m_int_peers_by_psycedu2, pars = "b_psyc_eductaionNo", "b_psyc_educationYes")+ theme_classic()
    mcmc_rank_overlay(m_int_peers_by_psycedu2, pars = "b_psyc_educationNo", "b_psyc_educationYes")+ theme_classic()
    
    marginal_effects(m_int_peers_by_psycedu2, "psyc_education", categorical = TRUE)
    conditional_effects(m_int_peers_by_psycedu2, "psyc_education", categorical = TRUE)
    
    #fourth model (m3): cumulative model with unequal variances#####
    f_int_peers_by_psycedu3 <- bf(act_int_peers ~ 1 + psyc_education) +
      lf(disc ~ 0 + psyc_education, cmc = FALSE)
    
    get_prior(f_int_peers_by_psycedu3,
              data_all_test, 
              family = cumulative("probit"))
    
    prior_int_peers_by_psycedu3 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5"),
      prior(normal(0,1), class=b, coef=psyc_educationYes, dpar=disc)) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    p2 <- rnorm(10000, 0,1)
    dens(p)
    dens(inv_logit(p))
    dens(p2)
    dens(inv_logit(p2))
    
    #setting a better prior
    prior_int_peers_by_psycedu3 <- c(
      prior(normal(0,1), class=b, coef=psyc_educationYes),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5"),
      prior(normal(0,1), class=b, coef=psyc_educationYes, dpar=disc))
    
    
    # Testing the prior in the model
    m_int_peers_by_psycedu3 <- brm(
      f_int_peers_by_psycedu3,
      data_all_test,
      family = cumulative("probit"),#using probit link function since I'm working with ordinal regression
      prior = prior_int_peers_by_psycedu3,
      sample_prior = "only"
    )
    
    pp_check(m_int_peers_by_psycedu_prior3, nsamples = 100)
    
    #testing the actual model
    m_int_peers_by_psycedu3 <- brm(
      formula = f_int_peers_by_psycedu3,
      data = data_all_test,
      family = cumulative("probit"),
      prior = prior_int_peers_by_psycedu3,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_int_peers_by_psycedu3)
    
    
    # Posterior predictive check
    pp_check(m_int_peers_by_psycedu3, nsamples = 100)
    ## Check the model for warnings
    m_int_peers_by_psycedu3
    
    
    
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisYes > 0"))
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisNo > 0"))
    # 
    # # 
    # mcmc_trace(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    # mcmc_rank_overlay(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    
    marginal_effects(m_int_peers_by_psycedu3, "psyc_education", categorical = TRUE)
    conditional_effects(m_int_peers_by_psycedu3, "psyc_education", categorical = TRUE)
    #Model comparison#####
    loo(m_int_peers_by_psycedu0, m_int_peers_by_psycedu1, m_int_peers_by_psycedu2, m_int_peers_by_psycedu3)
    loo_model_weights(m_int_peers_by_psycedu0, m_int_peers_by_psycedu1, m_int_peers_by_psycedu2, m_int_peers_by_psycedu3)























    #what kind of education????? WORDCLOUD#####

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(text)
library(readr)
library(dplyr)
library(e1071)
library(mlbench)

#Text mining packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


corpus = Corpus(VectorSource(data_all_test$expl_psyc_education))

#Conversion to Lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)

#Removing Punctuation
corpus = tm_map(corpus, removePunctuation)

#Remove stopwords
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("danish")))
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))
corpus = tm_map(corpus, removeWords, "gennem")
# Stemming
#corpus = tm_map(corpus, stemDocument)

# Eliminate white spaces
corpus = tm_map(corpus, stripWhitespace)
corpus[[1]][1]

DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)
head(dat, 9)

set.seed(100)
wordcloud(words = dat$word, freq = dat$freq, min.freq = 3, max.words=9, random.order=FALSE, rot.per=0.30, colors=brewer.pal(8, "Dark2"))




#Model time


##### act_int_peer_support ~ 1 + choose_peers_w_MI + #####
    #First model (m0): cumulative model with equal variances#####
    data_all_test_new <- na.omit(data_all_test)
    f_act_int_peer_support_by_peers0 <- bf(act_int_peers_support ~ 1 + choose_peers_w_MI)
    
    get_prior(f_act_int_peer_support_by_peers0,
              data_all_test_new, 
              family = cumulative("probit"))
    
    
    prior_act_int_peer_support_by_peers0 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))
    
    #setting a better prior
    prior_act_int_peer_support_by_peers0 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    # Testing the prior in the model
    m_act_int_peer_support_by_peers_prior0 <- brm(
      f_act_int_peer_support_by_peers0,
      data_all_test_new,
      family = cumulative("probit"),
      prior = prior_act_int_peer_support_by_peers0,
      sample_prior = "only"
    )
    
    pp_check(m_act_int_peer_support_by_peers_prior0, nsamples = 100)
    
    #testing the actual model
    m_act_int_peer_support_by_peers0 <- brm(
      formula = f_act_int_peer_support_by_peers0,
      data = data_all_test_new,
      family = cumulative("probit"),
      prior = prior_act_int_peer_support_by_peers0,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_act_int_peer_support_by_peers0)
    
    
    # Posterior predictive check
    pp_check(m_act_int_peer_support_by_peers0, nsamples = 100)
    ## Check the model for warnings
    m_act_int_peer_support_by_peers0
    
    plot(hypothesis(m_act_int_peer_support_by_peers0,"choose_peers_w_MIother > 0"))
    plot(hypothesis(m_act_int_peer_support_by_peers0,"choose_peers_w_MIdistant > 0"))
    
    mcmc_trace(m_act_int_peer_support_by_peers0, pars = "b_choose_peers_w_MIother","b_choose_peers_w_MIdistant" )+ theme_classic()
    mcmc_rank_overlay(m_act_int_peer_support_by_peers0, pars = "b_choose_peers_w_MIother", "b_choose_peers_w_MIdistant") + theme_classic()
    
    marginal_effects(m_act_int_peer_support_by_peers0, "choose_peers_w_MI", categorical = TRUE)
    conditional_effects(m_act_int_peer_support_by_peers0, "choose_peers_w_MI", categorical = TRUE)   
    
    
        #model 0.1: adding guilt to the model (testing the different models now and looking at method after)#####
        f_act_int_peer_support_by_peers_plus_guilt0 <- bf(act_int_peers_support ~ 1 + choose_peers_w_MI + int_MI_guilt)
        
        get_prior(f_act_int_peer_support_by_peers_plus_guilt0,
                  data_all_test_new, 
                  family = cumulative("probit"))
        
        
        prior_act_int_peer_support_by_peers_plus_guilt0 <- c(
          prior(normal(0,1), class=b, coef=int_MI_guilt),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
          prior(student_t(3, 0, 2.5), class=Intercept),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="5"))
          # prior(student_t(3, 0, 2.5), class=sd),
          # prior(student_t(3, 0, 2.5), class=sd, group=int_MI_guilt),
          # prior(student_t(3, 0, 2.5), class=sd, coef=Intercept, group=int_MI_guilt))
        
        ## Testing the prior
        dens(rstudent_t(10000, 3, 0, 2.5))
        p <- rstudent_t(10000, 3, 0, 1)
        dens(p)
        dens(inv_logit(p))
        
        #setting a better prior
        prior_act_int_peer_support_by_peers_plus_guilt0 <- c(
          prior(normal(0,1), class=b, coef=int_MI_guilt),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
          prior(student_t(3, 0, 1), class=Intercept),
          prior(student_t(3, 0, 1), class=Intercept, coef="1"),
          prior(student_t(3, 0, 1), class=Intercept, coef="2"),
          prior(student_t(3, 0, 1), class=Intercept, coef="3"),
          prior(student_t(3, 0, 1), class=Intercept, coef="4"),
          prior(student_t(3, 0, 1), class=Intercept, coef="5"))
          # prior(student_t(3, 0, 1), class=sd),
          # prior(student_t(3, 0, 1), class=sd, group=int_MI_guilt),
          # prior(student_t(3, 0, 1), class=sd, coef=Intercept, group=int_MI_guilt)) 
        
        
        # Testing the prior in the model
        m_act_int_peer_support_by_peers_plus_guilt_prior0 <- brm(
          f_act_int_peer_support_by_peers_plus_guilt0,
          data_all_test_new,
          family = cumulative("probit"),
          prior = prior_act_int_peer_support_by_peers_plus_guilt0,
          sample_prior = "only"
        )
        
        pp_check(m_act_int_peer_support_by_peers_plus_guilt_prior0, nsamples = 100)
        
        #testing the actual model
        m_act_int_peer_support_by_peers_plus_guilt0 <- brm(
          formula = f_act_int_peer_support_by_peers_plus_guilt0,
          data = data_all_test_new,
          family = cumulative("probit"),
          prior = prior_act_int_peer_support_by_peers_plus_guilt0,
          sample_prior = T
        )
        
        ### model quality check
        summary(m_act_int_peer_support_by_peers_plus_guilt0)
        
        
        # Posterior predictive check
        pp_check(m_act_int_peer_support_by_peers_plus_guilt0, nsamples = 100)
        ## Check the model for warnings
        m_act_int_peer_support_by_peers_plus_guilt0
        
        
        hypothesis(m_act_int_peer_support_by_peers_plus_guilt0,"choose_peers_w_MIother > 0")
        hypothesis(m_act_int_peer_support_by_peers_plus_guilt0,"choose_peers_w_MIdistant > 0")
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_guilt0,"choose_peers_w_MIother > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_guilt0,"choose_peers_w_MIdistant > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_guilt0,"int_MI_guilt > 0"))
        
        hypothesis(m_act_int_peer_support_by_peers_plus_guilt0,"int_MI_guilt > 0")
                   
        mcmc_trace(m_act_int_peer_support_by_peers_plus_guilt0, pars =  "b_choose_peers_w_MIdistant", "b_int_MI_guilt")+ theme_classic()
        mcmc_rank_overlay(m_act_int_peer_support_by_peers_plus_guilt0, pars = "b_choose_peers_w_MIdistant", "b_int_MI_guilt") + theme_classic()
        
        marginal_effects(m_act_int_peer_support_by_peers_plus_guilt0, "int_MI_guilt", categorical = TRUE)
        conditional_effects(m_act_int_peer_support_by_peers_plus_guilt0, "choose_peers_w_MI", categorical = TRUE)   
        
            
        #model 0.2: adding blame to the model (testing the different models now and looking at method after)#####
        f_act_int_peer_support_by_peers_plus_blame0 <- bf(act_int_peers_support ~ 1 + choose_peers_w_MI + int_MI_blame_myself)
        
        get_prior(f_act_int_peer_support_by_peers_plus_blame0,
                  data_all_test_new, 
                  family = cumulative("probit"))
        
        
        prior_act_int_peer_support_by_peers_plus_blame0 <- c(
          prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
          prior(normal(0,1), class=b, coef=int_MI_blame_myself),
          prior(student_t(3, 0, 2.5), class=Intercept),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
          # prior(student_t(3, 0, 2.5), class=sd),
          # prior(student_t(3, 0, 2.5), class=sd, group=int_MI_blame_myself),
          # prior(student_t(3, 0, 2.5), class=sd, coef=Intercept, group=int_MI_blame_myself))
          # 
          
        ## Testing the prior
        dens(rstudent_t(10000, 3, 0, 2.5))
        p <- rstudent_t(10000, 3, 0, 1)
        dens(p)
        dens(inv_logit(p))
        
        #setting a better prior
        prior_act_int_peer_support_by_peers_plus_blame0 <- c(
          prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
          prior(normal(0,1), class=b, coef=int_MI_blame_myself),
          prior(student_t(3, 0, 1), class=Intercept),
          prior(student_t(3, 0, 1), class=Intercept, coef="1"),
          prior(student_t(3, 0, 1), class=Intercept, coef="2"),
          prior(student_t(3, 0, 1), class=Intercept, coef="3"),
          prior(student_t(3, 0, 1), class=Intercept, coef="4"),
          prior(student_t(3, 0, 1), class=Intercept, coef="5"))
          # prior(student_t(3, 0, 1), class=sd),
          # prior(student_t(3, 0, 1), class=sd, group=int_MI_blame_myself),
          # prior(student_t(3, 0, 1), class=sd, coef=Intercept, group=int_MI_blame_myself))
          # 
        
        # Testing the prior in the model
        m_act_int_peer_support_by_peers_plus_blame_prior0 <- brm(
          f_act_int_peer_support_by_peers_plus_blame0,
          data_all_test_new,
          family = cumulative("probit"),
          prior = prior_act_int_peer_support_by_peers_plus_blame0,
          sample_prior = "only"
        )
        
        pp_check(m_act_int_peer_support_by_peers_plus_blame_prior0, nsamples = 100)
        
        #testing the actual model
        m_act_int_peer_support_by_peers_plus_blame0 <- brm(
          formula = f_act_int_peer_support_by_peers_plus_blame0,
          data = data_all_test_new,
          family = cumulative("probit"),
          prior = prior_act_int_peer_support_by_peers_plus_blame0,
          sample_prior = T
        )
        
        ### model quality check
        summary(m_act_int_peer_support_by_peers_plus_blame0)
        
        
        # Posterior predictive check
        pp_check(m_act_int_peer_support_by_peers_plus_blame0, nsamples = 100)
        ## Check the model for warnings
        m_act_int_peer_support_by_peers_plus_blame0
        
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame0,"choose_peers_w_MIother > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame0,"choose_peers_w_MIdistant > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame0,"int_MI_blame_myself > 0"))
        
        
        mcmc_trace(m_act_int_peer_support_by_peers_plus_blame0, pars =  "b_choose_peers_w_MIdistant", "b_int_MI_blame_myself")+ theme_classic()
        mcmc_rank_overlay(m_act_int_peer_support_by_peers_plus_blame0, pars = "b_choose_peers_w_MIdistant", "b_int_MI_blame_myself") + theme_classic()
        
        marginal_effects(m_act_int_peer_support_by_peers_plus_blame0, "choose_peers_w_MI", categorical = TRUE)
        conditional_effects(m_act_int_peer_support_by_peers_plus_blame0, "choose_peers_w_MI", categorical = TRUE)   
        
        
        
        #model 0.3 adding guilt and blame to the model (testing the different models now and looking at method after)#####
        f_act_int_peer_support_by_peers_plus_blame_plus_guilt0 <- bf(act_int_peers_support ~ 1 + choose_peers_w_MI + int_MI_blame_myself + int_MI_guilt)
        
        get_prior(f_act_int_peer_support_by_peers_plus_blame_plus_guilt0,
                  data_all_test_new, 
                  family = cumulative("probit"))
        
        
        prior_act_int_peer_support_by_peers_plus_blame_plus_guilt0 <- c(
          prior(normal(0,1), class=b, coef=int_MI_guilt),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
          prior(normal(0,1), class=b, coef=int_MI_blame_myself),
          prior(student_t(3, 0, 2.5), class=Intercept),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
          prior(student_t(3, 0, 2.5), class=Intercept, coef="5"))
          # prior(student_t(3, 0, 2.5), class=sd),
          # prior(student_t(3, 0, 2.5), class=sd, group=int_MI_blame_myself),
          # prior(student_t(3, 0, 2.5), class=sd, coef=Intercept, group=int_MI_blame_myself),
          # prior(student_t(3, 0, 2.5), class=sd, group=int_MI_guilt),
          # prior(student_t(3, 0, 2.5), class=sd, coef=Intercept, group=int_MI_guilt)) 
        
        ## Testing the prior
        dens(rstudent_t(10000, 3, 0, 2.5))
        p <- rstudent_t(10000, 3, 0, 1)
        dens(p)
        dens(inv_logit(p))
        
        #setting a better prior
        prior_act_int_peer_support_by_peers_plus_blame_plus_guilt0 <- c(
          prior(normal(0,1), class=b, coef=int_MI_guilt),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
          prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
          prior(normal(0,1), class=b, coef=int_MI_blame_myself),
          prior(student_t(3, 0, 1), class=Intercept),
          prior(student_t(3, 0, 1), class=Intercept, coef="1"),
          prior(student_t(3, 0, 1), class=Intercept, coef="2"),
          prior(student_t(3, 0, 1), class=Intercept, coef="3"),
          prior(student_t(3, 0, 1), class=Intercept, coef="4"),
          prior(student_t(3, 0, 1), class=Intercept, coef="5"))
          # prior(student_t(3, 0, 1), class=sd),
          # prior(student_t(3, 0, 1), class=sd, group=int_MI_blame_myself),
          # prior(student_t(3, 0, 1), class=sd, coef=Intercept, group=int_MI_blame_myself),
          # prior(student_t(3, 0, 1), class=sd, group=int_MI_guilt),
          # prior(student_t(3, 0, 1), class=sd, coef=Intercept, group=int_MI_guilt)) 
        
        
        # Testing the prior in the model
        m_act_int_peer_support_by_peers_plus_blame_plus_guilt_prior0 <- brm(
          f_act_int_peer_support_by_peers_plus_blame_plus_guilt0,
          data_all_test_new,
          family = cumulative("probit"),
          prior = prior_act_int_peer_support_by_peers_plus_blame_plus_guilt0,
          sample_prior = "only"
        )
        
        pp_check(m_act_int_peer_support_by_peers_plus_psyc_edu_plus_guilt_prior0, nsamples = 100)
        
        #testing the actual model
        m_act_int_peer_support_by_peers_plus_blame_plus_guilt0 <- brm(
          formula = f_act_int_peer_support_by_peers_plus_blame_plus_guilt0,
          data = data_all_test_new,
          family = cumulative("probit"),
          prior = prior_act_int_peer_support_by_peers_plus_blame_plus_guilt0,
          sample_prior = T
        )
        
        ### model quality check
        summary(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0)
        
        
        # Posterior predictive check
        pp_check(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0, nsamples = 100)
        ## Check the model for warnings
        m_act_int_peer_support_by_peers_plus_blame_plus_guilt0
        
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0,"choose_peers_w_MIother > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0,"choose_peers_w_MIdistant > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0,"int_MI_blame_myself > 0"))
        plot(hypothesis(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0,"int_MI_guilt > 0"))
        
        
        mcmc_trace(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0, pars =  "b_choose_peers_w_MIdistant", "b_int_MI_blame_myself")+ theme_classic()
        mcmc_rank_overlay(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0, pars = "b_choose_peers_w_MIdistant", "b_int_MI_blame_myself") + theme_classic()
        
        marginal_effects(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0, "choose_peers_w_MI", categorical = TRUE)
        conditional_effects(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0, "choose_peers_w_MI",categorical = TRUE)   
        
        #Model comparison#####
        m_act_int_peer_support_0.0 <- brms::add_criterion(m_act_int_peer_support_by_peers0, criterion = "loo")
        m_act_int_peer_support_0.1 <- brms::add_criterion(m_act_int_peer_support_by_peers_plus_guilt0, criterion = "loo")
        m_act_int_peer_support_0.2 <- brms::add_criterion(m_act_int_peer_support_by_peers_plus_blame0, criterion = "loo")
        m_act_int_peer_support_0.3 <- brms::add_criterion(m_act_int_peer_support_by_peers_plus_blame_plus_guilt0, criterion = "loo")
       
        brms::loo(m_act_int_peer_support_0.0,
                  m_act_int_peer_support_0.1,
                  m_act_int_peer_support_0.2,
                  m_act_int_peer_support_0.3)
      
        
        brms::loo_model_weights(m_act_int_peer_support_0.0,
                                m_act_int_peer_support_0.1,
                                m_act_int_peer_support_0.2,
                                m_act_int_peer_support_0.3)

        
      #model 0.1 is best. we will continue with this model while testing different methods
        
    
    
    #second model (m1):adjacent category model with equal variances and category specific effects#####
    f_act_int_peer_support_by_peers_plus_guilt1 <- bf(act_int_peers_support ~ 1 + cs(choose_peers_w_MI) + cs(int_MI_guilt))

    get_prior(f_act_int_peer_support_by_peers_plus_guilt1,
              data_all_test_new, 
              family = acat("probit"))
    
    
    prior_act_int_peer_support_by_peers_plus_guilt1 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(normal(0,1), class=b, coef=int_MI_guilt),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5")) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))
    
    prior_act_int_peer_support_by_peers_plus_guilt1 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(normal(0,1), class=b, coef=int_MI_guilt),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5")) 
    
    
    # Testing the prior in the model
    m_act_int_peer_support_by_peers_plus_guilt_prior1 <- brm(
      f_act_int_peer_support_by_peers_plus_guilt1,
      data_all_test_new,
      family = acat("probit"),
      prior = prior_act_int_peer_support_by_peers_plus_guilt1,
      sample_prior = "only"
    )
    
    pp_check(m_act_int_peer_support_by_peers_plus_guilt_prior1, nsamples = 100)
    
    #testing the actual model
    m_act_int_peer_support_by_peers_plus_guilt1 <- brm(
      formula = f_act_int_peer_support_by_peers_plus_guilt1,
      data = data_all_test_new,
      family = acat("probit"),
      prior = prior_act_int_peer_support_by_peers_plus_guilt1,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_act_int_peer_support_by_peers_plus_guilt1)
    
    
    # Posterior predictive check
    pp_check(m_act_int_peer_support_by_peers_plus_guilt1, nsamples = 100)
    ## Check the model for warnings
    m_act_int_peer_support_by_peers_plus_guilt1
    
    
    marginal_effects(m_act_int_peer_support_by_peers_plus_guilt1, "choose_peers_w_MI", categorical = TRUE)
    conditional_effects(m_act_int_peer_support_by_peers_plus_guilt1, "choose_peers_w_MI", categorical = TRUE)
    
    
    #third model (m2): adjacent category model with equal variances#####
    f_act_int_peer_support_by_peers_plus_guilt2 <- bf(act_int_peers_support ~ 1 + choose_peers_w_MI + int_MI_guilt)
    
    get_prior(f_act_int_peer_support_by_peers_plus_guilt2,
              data_all_test_new, 
              family = acat("probit"))
    
    
    prior_act_int_peer_support_by_peers_plus_guilt2 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(normal(0,1), class=b, coef=int_MI_guilt),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5"))  
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    dens(p)
    dens(inv_logit(p))
    
    prior_act_int_peer_support_by_peers_plus_guilt2 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(normal(0,1), class=b, coef=int_MI_guilt),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5"))
    
    
    # Testing the prior in the model
    m_act_int_peer_support_by_peers_plus_guilt_prior2 <- brm(
      f_act_int_peer_support_by_peers_plus_guilt2,
      data_all_test_new,
      family = acat("probit"),
      prior = prior_act_int_peer_support_by_peers_plus_guilt2,
      sample_prior = "only"
    )
    
    pp_check(m_act_int_peer_support_by_peers_plus_guilt_prior2, nsamples = 100)
    
    #testing the actual model
    m_act_int_peer_support_by_peers_plus_guilt2 <- brm(
      formula = f_act_int_peer_support_by_peers_plus_guilt2,
      data = data_all_test_new,
      family = acat("probit"),
      prior = prior_act_int_peer_support_by_peers_plus_guilt2,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_act_int_peer_support_by_peers_plus_guilt2)
    
    
    # Posterior predictive check
    pp_check(m_act_int_peer_support_by_peers_plus_guilt2, nsamples = 100)
    ## Check the model for warnings
    m_act_int_peer_support_by_peers_plus_guilt2
    
    plot(hypothesis(m_act_int_peer_support_by_peers_plus_guilt2,"choose_peers_w_MIother > 0"))
    plot(hypothesis(m_act_int_peer_support_by_peers_plus_guilt2,"choose_peers_w_MIdistant > 0"))
    plot(hypothesis(m_act_int_peer_support_by_peers_plus_guilt2,"int_MI_guilt > 0"))
    
    
    
    mcmc_trace(m_act_int_peer_support_by_peers_plus_guilt2, pars = "b_choose_peers_w_MIdistant", "b_int_MI_guilt")+ theme_classic()
    mcmc_rank_overlay(m_act_int_peer_support_by_peers_plus_guilt2, pars = "b_choose_peers_w_MIdistant", "b_int_MI_guilt")+ theme_classic()
    
    marginal_effects(m_act_int_peer_support_by_peers_plus_guilt2, "choose_peers_w_MI", categorical = TRUE)
    conditional_effects(m_act_int_peer_support_by_peers_plus_guilt2, "choose_peers_w_MI", categorical = TRUE)
    
    #fourth model (m3): cumulative model with unequal variances#####
    f_act_int_peer_support_by_peers_plus_guilt3 <- bf(act_int_peers_support ~ 1 + choose_peers_w_MI + int_MI_guilt) +
      lf(disc ~ 0 + choose_peers_w_MI + int_MI_guilt, cmc = FALSE)
    
    get_prior(f_act_int_peer_support_by_peers_plus_guilt3,
              data_all_test_new, 
              family = cumulative("probit"))
    
    prior_act_int_peer_support_by_peers_plus_guilt3 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(normal(0,1), class=b, coef=int_MI_guilt),
      prior(student_t(3, 0, 2.5), class=Intercept),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="1"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="2"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="3"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="4"),
      prior(student_t(3, 0, 2.5), class=Intercept, coef="5"),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant, dpar=disc),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother, dpar=disc),
      prior(normal(0,1), class=b, coef=int_MI_guilt, dpar=disc)) 
    
    ## Testing the prior
    dens(rstudent_t(10000, 3, 0, 2.5))
    p <- rstudent_t(10000, 3, 0, 1)
    p2 <- rnorm(10000, 0,1)
    dens(p)
    dens(inv_logit(p))
    dens(p2)
    dens(inv_logit(p2))
    
    #setting a better prior
    prior_act_int_peer_support_by_peers_plus_guilt3 <- c(
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother),
      prior(normal(0,1), class=b, coef=int_MI_guilt),
      prior(student_t(3, 0, 1), class=Intercept),
      prior(student_t(3, 0, 1), class=Intercept, coef="1"),
      prior(student_t(3, 0, 1), class=Intercept, coef="2"),
      prior(student_t(3, 0, 1), class=Intercept, coef="3"),
      prior(student_t(3, 0, 1), class=Intercept, coef="4"),
      prior(student_t(3, 0, 1), class=Intercept, coef="5"),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIdistant, dpar=disc),
      prior(normal(0,1), class=b, coef=choose_peers_w_MIother, dpar=disc),
      prior(normal(0,1), class=b, coef=int_MI_guilt, dpar=disc))
    
    
    # Testing the prior in the model
    m_act_int_peer_support_by_peers_plus_guilt_prior3 <- brm(
      f_act_int_peer_support_by_peers_plus_guilt3,
      data_all_test_new,
      family = cumulative("probit"),#using probit link function since I'm working with ordinal regression
      prior = prior_act_int_peer_support_by_peers_plus_guilt3,
      sample_prior = "only"
    )
    
    pp_check(m_act_int_peer_support_by_peers_plus_guilt_prior3, nsamples = 100)
    
    #testing the actual model
    m_act_int_peer_support_by_peers_plus_guilt3 <- brm(
      formula = f_act_int_peer_support_by_peers_plus_guilt3,
      data = data_all_test_new,
      family = cumulative("probit"),
      prior = prior_act_int_peer_support_by_peers_plus_guilt3,
      sample_prior = T
    )
    
    ### model quality check
    summary(m_act_int_peer_support_by_peers_plus_guilt3)
    
    
    # Posterior predictive check
    pp_check(m_act_int_peer_support_by_peers_plus_guilt3, nsamples = 100)
    ## Check the model for warnings
    m_act_int_peer_support_by_peers_plus_guilt3
    
    
    
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisYes > 0"))
    # plot(hypothesis(m_guilt_by_diagnosis1,"diagnosisNo > 0"))
    # 
    # # 
    # mcmc_trace(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    # mcmc_rank_overlay(m_guilt_by_diagnosis1, pars = "b_diagnosisNo", "b_diagnosisYes")+ theme_classic()
    
    marginal_effects(m_act_int_peer_support_by_peers_plus_guilt3, "choose_peers_w_MI", categorical = TRUE)
    conditional_effects(m_act_int_peer_support_by_peers_plus_guilt3, "choose_peers_w_MI", categorical = TRUE)
    #Model comparison#####
    loo(m_act_int_peer_support_by_peers_plus_guilt0, m_act_int_peer_support_by_peers_plus_guilt1, m_act_int_peer_support_by_peers_plus_guilt2, m_act_int_peer_support_by_peers_plus_guilt3)
    loo_model_weights(m_act_int_peer_support_by_peers_plus_guilt0, m_act_int_peer_support_by_peers_plus_guilt1, m_act_int_peer_support_by_peers_plus_guilt2, m_act_int_peer_support_by_peers_plus_guilt3)
    #model 3.0 is best
    
    
#     
# #### wordcloud with diagnoses #####
#     corpus1 = Corpus(VectorSource(data_all_test$choose_diagnosis))
#     
#     #Conversion to Lowercase
#     corpus1 = tm_map(corpus1, PlainTextDocument)
#     corpus1 = tm_map(corpus1, tolower)
#     
#     #Removing Punctuation
#     corpus1 = tm_map(corpus1, removePunctuation)
#     
#     #Remove stopwords
#     corpus1 = tm_map(corpus1, removeWords, c("cloth", stopwords("danish")))
#     corpus1 = tm_map(corpus1, removeWords, c("cloth", stopwords("english")))
#     corpus1 = tm_map(corpus1, removeWords, "kind")
#     corpus1 = tm_map(corpus1, removeWords, "enhver")
#     corpus1 = tm_map(corpus1, removeWords, "form")
#     corpus1 = tm_map(corpus1, removeWords, "uden")
#     corpus1 = tm_map(corpus1, removeWords, "angstlidelseunipolar")
#     corpus1 = tm_map(corpus1, removeWords, "uden")
#     corpus1 = tm_map(corpus1, removeWords, "inkluderer")
#     corpus1 = tm_map(corpus1, removeWords, "tilstand")
#     corpus1 = tm_map(corpus1, removeWords, "mani")
#     
#     
#     # Stemming
#     #corpus1 = tm_map(corpus1, stemDocument)
#     
#     # Eliminate white spaces
#     corpus1 = tm_map(corpus1, stripWhitespace)
#     corpus1[[1]][1]
#     
#     DTM1 <- TermDocumentMatrix(corpus1)
#     mat1 <- as.matrix(DTM1)
#     f1 <- sort(rowSums(mat1),decreasing=TRUE)
#     dat1 <- data.frame(word = names(f1),freq=f1)
#     head(dat1, 6)
#     
#     set.seed(100)
#     wordcloud(words = dat1$word, freq = dat1$freq, min.freq = 3, max.words=6, random.order=FALSE, rot.per=0.30, colors=brewer.pal(8, "Dark2"))
#     
    



















    
    
    
    
    
    
    
    
