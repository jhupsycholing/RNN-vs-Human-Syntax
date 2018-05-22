library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
library(tidyr)

root <- './../'

point_size <- 2
line_size <- 0.7
base_size <- 12

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E69F00")

theme_set(theme_bw(base_size=base_size) +
            theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  legend.key.size=unit(0.7, 'lines'),
                  legend.spacing=unit(0, 'in'),
                  axis.title.x=element_text(vjust=0)))

mult_rnn_load <- function() {
  mult <- read.csv(file.path(root,'cogsci/mult.csv'))
  #mult$dimensions <- factor(paste(mult$e_size, 'dimensions'),
                            #levels=c('50 dimensions','1000 dimensions'))
  mult$is_error <- ifelse(mult$error == 'True', 1, 0)
  mult <- mult %>%
    separate(condition,c('subject','numbers'),sep=1) %>%
    separate(numbers,c('rel_num','prep_num'),sep=1) %>%
    mutate(match=factor(ifelse(prep_num != 'P' & rel_num !='P', 'Match','Mismatch'),
                        levels=c('Match','Mismatch')),
           subject=factor(ifelse(subject=='S','Singular subject','Plural Subject'),
                          levels=c('Singular subject', 'Plural subject')),
           rel_num=factor(ifelse(rel_num == '_',"Null RC noun \n(that ate quickly)",ifelse(rel_num == 'S','Singular RC noun \n(that ate the worm)','Plural RC noun \n(that ate the worms)')),
                          levels=c('Null RC noun \n(that ate quickly)','Singular RC noun \n(that ate the worm)','Plural RC noun \n(that ate the worms)')),
           prep_num=factor(ifelse(prep_num == 'S','Singular PP noun \n(near the tree)','Plural PP noun \n(near the trees)'),
                          levels=c('Singular PP noun \n(near the tree)','Plural PP noun \n(near the trees)')))
    mult %>%
      group_by(item_id) %>%
      summarise(mean_error=mean(is_error)) -> accuracy_by_item
    
    bad_items <- (accuracy_by_item %>% filter(mean_error > 0.2))$item_id
    good_data <- mult %>% filter(!(item_id %in% bad_items))
    print(good_data)
    return(good_data)
    
              
}
mult_flip_rnn_load <- function() {
  mult <- read.csv(file.path(root,'cogsci/mult_flip.csv'))
  #mult$dimensions <- factor(paste(mult$e_size, 'dimensions'),
  #levels=c('50 dimensions','1000 dimensions'))
  mult$is_error <- ifelse(mult$error == 'True', 1, 0)
  mult$condition_copy <- mult$condition
  mult <- mult %>%
    separate(condition,c('numbers','first')) %>%
    separate(condition_copy,c('numbers_copy','first_copy')) %>%
    separate(numbers,c('subject','nums'),sep=1) %>%
    separate(numbers_copy,c('subject_copy','nums_copy'),sep=1) %>%
    separate(nums,c('num1','num2'),sep=1) %>%
    separate(nums_copy,c('rel_num','prep_num'),sep=1) %>%
    mutate(match=factor(ifelse(prep_num != 'P' & rel_num !='P', 'Match','Mismatch'),
                        levels=c('Match','Mismatch')),
           subject=factor(ifelse(subject=='S','Singular subject','Plural Subject'),
                          levels=c('Singular subject', 'Plural subject')),
           rel_num=factor(ifelse(first == 'rel',ifelse(num1=="A","Null RC noun \n(that ate quickly)",ifelse(num1=="S",'Singular RC noun \n(that ate the worm)','Plural RC noun \n(that ate the worms)')),ifelse(num2=="A","Null RC noun \n(that ate quickly)",ifelse(num2=="S",'Singular RC noun \n(that ate the worm)','Plural RC noun \n(that ate the worms)'))),
                          levels=c('Null RC noun \n(that ate quickly)','Singular RC noun \n(that ate the worm)','Plural RC noun \n(that ate the worms)')),
           prep_num=factor(ifelse(first == 'prep',ifelse(num1 == "S","Singular PP noun \n(near the tree)",'Plural PP noun \n(near the trees)'),ifelse(num2 == "S","Singular PP noun \n(near the tree)",'Plural PP noun \n(near the trees)')),
                           levels=c('Singular PP noun \n(near the tree)','Plural PP noun \n(near the trees)')),
           first=factor(ifelse(first=="rel",'RC First','PP First'),
                        levels=c('RC First','PP First')))
  mult$prep_num <- relevel(mult$prep_num,c('Singular PP noun \n(near the tree)'))
  mult %>%
    group_by(item_id) %>%
    summarise(mean_error=mean(is_error)) -> accuracy_by_item
  
  bad_items <- (accuracy_by_item %>% filter(mean_error > 0.2))$item_id
  good_data <- mult %>% filter(!(item_id %in% bad_items))
  print(good_data)
  return(good_data)
  
  
}


bc_rnn_load <- function() {
  pprc <- read.csv(file.path(root, 'cogsci/pprc.csv'))
  pprc$dimensions <- factor(paste(pprc$e_size, 'dimensions'), 
                            levels=c('50 dimensions', '1000 dimensions'))
  pprc$is_error <- ifelse(pprc$error == 'True', 1, 0)
  #Introduce noise:
  #pprc$is_error <- ifelse(pprc$is_error | rbinom(nrow(pprc), 1, 0.05), 1, 0)
  pprc <- pprc %>%
    separate(condition, c('phrase_type', 'numbers')) %>%
    separate(numbers, c('subject', 'local_noun'), sep=1) %>%
    mutate(match=factor(ifelse(subject == local_noun, 'Match', 'Mismatch'),
                        levels=c('Match', 'Mismatch')),
           subject=factor(ifelse(subject == 's', 'Singular subject', 'Plural subject'),
                          levels=c('Singular subject', 'Plural subject')),
           phrase_type=ifelse(phrase_type == 'prep', 'Prepositional phrase', 'Relative clause'),
           phrase_and_match=sprintf('%s (%s)', phrase_type, match))
           

  pprc %>% 
    group_by(item_id) %>%
    summarise(mean_error=mean(is_error)) -> accuracy_by_item

  bad_items <- (accuracy_by_item %>% filter(mean_error > 0.2))$item_id
  good_data <- pprc %>% filter(!(item_id %in% bad_items))
  return(good_data)
}

mult_rnn_plot <- function(good_data) {
  dodge <- position_dodge(width=0.9)
  cbPalettelabels=c('Singular PP noun (near the tree)', 'Plural PP noun (near the trees)')
  ggplot(good_data, aes(x=rel_num, y=is_error, fill=prep_num)) +
    stat_summary(fun.y='mean', geom='bar', size=point_size, position=dodge) +
    stat_summary(fun.data='mean_cl_boot', geom='errorbar', width=0.15,
                 size=line_size, position=dodge) +
    scale_y_continuous('Error rate', labels=percent, breaks=c(0, 0.1, 0.2, 0.3)) +
    scale_x_discrete('') + 
    scale_fill_manual('Number of PP noun', breaks=c('Singular PP noun \n(near the tree)', 'Plural PP noun \n(near the trees)'), values=cbPalettelabels) +
    #scale_fill_discrete('Number of local noun', breaks=c(F, T), 
    #                    labels=c("Different from subject", "Same as subject")) +
    theme(legend.position="bottom", 
          legend.direction="vertical") +
    coord_cartesian(ylim=c(0, .12))
}

mult_flip_rnn_plot <- function(good_data) {
  dodge <- position_dodge(width=0.9)
  ggplot(good_data, aes(x=rel_num, y=is_error, fill=prep_num)) +
    stat_summary(fun.y='mean', geom='bar', size=point_size, position=dodge) +
    stat_summary(fun.data='mean_cl_boot', geom='errorbar', width=0.15,
                 size=line_size, position=dodge) +
    scale_y_continuous('Error rate', labels=percent, breaks=c(0, 0.1, 0.2, 0.3)) +
    scale_x_discrete('') + 
    scale_fill_manual('Number of PP noun', breaks=c('Singular PP noun \n(near the tree)', 'Plural PP noun \n(near the trees)'), values=cbPalette,labels=c('Singular PP noun (near the tree)', 'Plural PP noun (near the trees)')) +
    #scale_fill_discrete('Number of local noun', breaks=c(F, T), 
    #                    labels=c("Different from subject", "Same as subject")) +
    theme(legend.position="bottom", 
          legend.direction="vertical") +
    coord_cartesian(ylim=c(0, .30)) +
    facet_grid(first ~ .)
}

bc_rnn_plot <- function(good_data) {
  dodge <- position_dodge(width=0.9)
  ggplot(good_data, aes(x=match, y=is_error, fill=phrase_type)) +
    stat_summary(fun.y='mean', geom='bar', size=point_size, position=dodge) +
    stat_summary(fun.data='mean_cl_boot', geom='errorbar', width=0.15,
                 size=line_size, position=dodge) +
    scale_y_continuous('Error rate', labels=percent, breaks=c(0, 0.1, 0.2, 0.3)) +
    scale_x_discrete('') + 
    scale_fill_manual('Modifier type', breaks=c('Prepositional phrase', 'Relative clause'), values=cbPalette) +
    #scale_fill_discrete('Number of local noun', breaks=c(F, T), 
    #                    labels=c("Different from subject", "Same as subject")) +
    theme(legend.position="bottom", 
          legend.direction="vertical") +
    coord_cartesian(ylim=c(0, 0.35)) +
    facet_grid(subject ~ dimensions)
}

mult_rep_load_data <- function() {
  full_data <- read.csv(file.path(root, 'scripts_and_data/exp5_results.csv'))
  full_data$Error <- as.integer(as.logical(!full_data$Correct))
  full_data$ID <- as.character.hexmode(factor(full_data$ID,levels=unique(full_data$ID)))
  full_data$Item <- as.factor(full_data$Item)
  filler_acc <- by(full_data,full_data$ID,function(x) {
    y <- subset(x, x$Condition == "filler", select =c('Correct'))
    apply(y, 2,mean, na.rm = TRUE)
  }
  )
  filler_acc <- subset(filler_acc, filler_acc[] < .8,select=c('ID'))
  vec <- unlist(dimnames(filler_acc)[[1]])
  full_data <- full_data[!full_data$ID %in% vec, ]
  return(full_data)
}

bc_rep_load_data <- function() {
  exp1_data <- read.csv(file.path(root, 'scripts_and_data/exp_results.csv'))
  exp2_data <- read.csv(file.path(root, 'scripts_and_data/exp2_results.csv'))
  exp3_data <- read.csv(file.path(root, 'scripts_and_data/exp4_results.csv'))
  full_data <- rbind(exp1_data, exp2_data, exp3_data)
  levels(full_data$Exp) <- c("Unspeeded","SPR + Speeded","RSVP + Speeded")
  full_data$Error <- as.integer(as.logical(!full_data$Correct))
  full_data$ID <- as.character.hexmode(factor(full_data$ID,levels=unique(full_data$ID)))
  full_data$Item <- as.factor(full_data$Item)
  filler_acc <- by(full_data,full_data$ID,function(x) {
    y <- subset(x, x$Condition == "filler", select =c('Correct'))
    apply(y, 2,mean, na.rm = TRUE)
  }
  )
  filler_acc <- subset(filler_acc, filler_acc[] < .8,select=c('ID'))
  vec <- unlist(dimnames(filler_acc)[[1]])
  full_data <- full_data[!full_data$ID %in% vec, ]
  return(full_data)
}

mult_rep_plot <- function(d) {
  dodge <- position_dodge(width=0.9)
  dd <- subset(d, Condition != "filler")
  dd$rel_num <- factor(ifelse(dd$Att1 == 'A', "Null RC noun \n(that ate quickly)", ifelse(dd$Att1 == "S","Singular RC noun \n(that ate the worm)","Plural RC noun \n(that ate the worms)")), levels=c('Null RC noun \n(that ate quickly)','Singular RC noun \n(that ate the worm)', 'Plural RC noun \n(that ate the worms)'))
  dd$prep_num <- factor(ifelse(dd$Att2 == "S","Singular PP noun \n(near the tree)","Plural PP noun \n(near the trees)"),levels=c('Singular PP noun \n(near the tree)','Plural PP noun \n(near the trees)'))
  ggplot(dd, aes(x=rel_num, y=Error, fill=prep_num)) +
    stat_summary(fun.y='mean', geom='bar', size=point_size, position=dodge) +
    stat_summary(fun.data='mean_cl_boot', geom='errorbar', width=0.15,
                 size=line_size, position=dodge) +
    scale_y_continuous('Error rate', labels=percent, breaks=c(0, 0.1, 0.2, 0.3)) +
    scale_x_discrete('') + 
    scale_fill_manual('Number of PP noun', breaks=c('Singular PP noun \n(near the tree)', 'Plural PP noun \n(near the trees)'), values=cbPalette,labels=c('Singular PP noun (near the tree)', 'Plural PP noun (near the trees)')) +
    #scale_fill_discrete('Number of local noun') +
    theme(legend.position="bottom", legend.direction="vertical") +
    coord_cartesian(ylim=c(0, 0.35))
}

bc_rep_plot <- function(d) {
  dodge <- position_dodge(width=0.9)
  dd <- subset(d, Condition != "filler")
  dd$Match <- factor(c("X", "Mismatch", "Match")[as.numeric(dd$Match)], 
                     levels=c("Match", "Mismatch"))
  dd$num <- factor(ifelse(dd$Matrix_num == 's', 'Singular subject', 'Plural subject'), levels=c('Singular subject', 'Plural subject'))
  dd$phrase_type_nice <- ifelse(dd$phrase_type == 'rel', 'Relative clause', 'Prepositional phrase')
  ggplot(dd, aes(x=Match, y=Error, fill=phrase_type_nice)) +
    stat_summary(fun.y='mean', geom='bar', size=point_size, position=dodge) +
    stat_summary(fun.data='mean_cl_boot', geom='errorbar', width=0.15,
                 size=line_size, position=dodge) +
    scale_y_continuous('Error rate', labels=percent, breaks=c(0, 0.1, 0.2, 0.3)) +
    scale_x_discrete('') + 
    scale_fill_manual('Modifier type', breaks=c('Prepositional phrase', 'Relative clause'), values=cbPalette) +
    #scale_fill_discrete('Number of local noun') +
    theme(legend.position="bottom", legend.direction="vertical") +
    coord_cartesian(ylim=c(0, 0.35)) +
    facet_grid(num ~ Exp)
}

generate_plots <- function() {
  writeup_root <- '~/online_agreement/cogsci'
  
  bc_rep <- bc_rep_load_data()
  p <- bc_rep_plot(bc_rep)
  ggsave(file.path(writeup_root, 'bc_rep.pdf'), p, width=5, height=5)
  
  bc_rnn <- bc_rnn_load()
  p <- bc_rnn_plot(bc_rnn)
  ggsave(file.path(writeup_root, 'bc_rnn.pdf'), p, width=5, height=5)
  
  mult_rnn <- mult_rnn_load()
  p <- mult_rnn_plot(mult_rnn)
  ggsave(file.path(writeup_root, 'mult_rnn.pdf'), p, width=5, height=5)
  
  mult_rep <- mult_rep_load_data()
  p <- mult_rep_plot(mult_rep)
  ggsave(file.path(writeup_root, 'mult_rep.pdf'), p, width=5, height=5)
  
  mult_flip_rnn <- mult_flip_rnn_load()
  p <- mult_flip_rnn_plot(mult_flip_rnn)
  ggsave(file.path(writeup_root, 'mult_flip_rnn.pdf'), p, width=5, height=5)
}