theme_tie <- function(
    base_size = 11,
    base_family = "Poppins",
    base_line_size = base_size / 22,
    base_rect_size = base_size / 22
) {
  bc <- c("#FAFAFA", "#333333", "#333333")
  half_line <- base_size / 2
  ggplot2::theme(
    line = ggplot2::element_line(
      colour = bc[3],
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),
    rect = ggplot2::element_rect(
      fill = bc[1],
      colour = bc[3],
      size = base_rect_size,
      linetype = 1
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "plain",
      colour = bc[3],
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),
    title = NULL,
    aspect.ratio = NULL,
    
    axis.title = NULL,
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = half_line), vjust = 1),
    axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half_line), vjust = 0),
    axis.title.x.bottom = NULL,
    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = half_line), vjust = 1),
    axis.title.y.left = NULL,
    axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(l = half_line), vjust = 0),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), colour = bc[3]),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = 0.8 * half_line / 2), vjust = 0),
    axis.text.x.bottom = NULL,
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.text.y.left = NULL,
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 0.8 * half_line / 2), hjust = 0),
    axis.ticks = ggplot2::element_line(colour = bc[3]),
    axis.ticks.x = NULL,
    axis.ticks.x.top = NULL,
    axis.ticks.x.bottom = NULL,
    axis.ticks.y = NULL,
    axis.ticks.y.left = NULL,
    axis.ticks.y.right = NULL,
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.x.top = NULL,
    axis.line.x.bottom = NULL,
    axis.line.y = NULL,
    axis.line.y.left = NULL,
    axis.line.y.right = NULL,
    
    legend.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    legend.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    legend.spacing = ggplot2::unit(2 * half_line, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    legend.key.size = ggplot2::unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
    legend.text.align = NULL,
    legend.title = ggplot2::element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.just = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::unit(2 * half_line, "pt"),
    
    panel.background = ggplot2::element_rect(fill = bc[1], colour = NA),
    panel.border = ggplot2::element_rect(fill = NA, colour = bc[3], size = 0.5, linetype = "solid"),
    panel.spacing = ggplot2::unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid = ggplot2::element_line(colour = bc[2]),
    panel.grid.major = ggplot2::element_line(colour = bc[2], size = ggplot2::rel(0.30)),
    panel.grid.minor = ggplot2::element_line(colour = bc[2], size = ggplot2::rel(0.10)),
    panel.grid.major.x = NULL,
    panel.grid.major.y = NULL,
    panel.grid.minor.x = NULL,
    panel.grid.minor.y = NULL,
    panel.ontop = FALSE,
    
    plot.background = ggplot2::element_rect(colour = bc[1]),
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(1.25),
      face = "bold",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1),
      face = "italic",
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.75),
      face = "italic",
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "plot",
    plot.tag = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),
    
    strip.background = ggplot2::element_rect(fill = bc[1], colour = bc[3]),
    strip.background.x = NULL,
    strip.background.y = NULL,
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = ggplot2::element_text(
      colour = bc[3],
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
    ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),
    
    complete = TRUE
  )
}

ggplot2::theme_set(theme_tie(base_size = 18, base_family = "Poppins"))

if (nzchar(system.file(package = "ggtext"))) {
  ggplot2::theme_update(
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(face = "italic"),
    plot.caption = ggtext::element_markdown(face = "italic"),
    axis.title.x = ggtext::element_markdown(),
    axis.text.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    axis.text.y = ggtext::element_markdown()
  )
}
ggplot2::update_geom_defaults("point", list(colour = ggplot2::theme_get()$line$colour))


tie_colours = list(
  tie_colors_d = c("#005e73", "#00C1D4", "#FF7E15", "#7200FE", "#143BFF", "#A8968C", "#FFDE36","#FF4F4F","#5AFFC5",  "#D0D0CE", "#FFAEA2"),
  tie_colors_c = c("#005e73", "#00C1D4", "#5AFFC5")
)

# tie_colors_c = c("#7200FE", "#143BFF", "#005e73", "#00C1D4", "#5AFFC5", "#D0D0CE", "#FFDE36", "#FF7E15", "#A8968C", "#FFAEA2", "#FF4F4F")



tie_palletes = function(n, type = c("discrete", "continuous")) {
  type = match.arg(type)
  if (type == "discrete") {
    palette = tie_colours[["tie_colors_d"]]
  } else {
    palette = tie_colours[["tie_colors_c"]]
  }
  if (missing(n)) {
    n = length(palette)
  }
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               if (n < 12) {
                 discrete = palette[1:n]
               } else {
                 discrete = grDevices::colorRampPalette(palette)(n)
               }
  )
  structure(out, class = "palette")
}


scale_colour_tie_d = function(name) {
  ggplot2::scale_colour_manual(values = tie_palletes(n = 10, type = "discrete"))
}


scale_fill_tie_d = function(name) {
  ggplot2::scale_fill_manual(values = tie_palletes(n = 10, type = "discrete"))
}


scale_colour_tie_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = tie_palletes(n = 3, type = "continuous"))
}


scale_fill_tie_c = function(name) {
  ggplot2::scale_fill_gradientn(colours = tie_palletes(n = 3, type = "continuous"))
}



options(
  ggplot2.discrete.colour = function(...) scale_colour_tie_d(),
  ggplot2.discrete.fill = function(...) scale_fill_tie_d(),
  ggplot2.continuous.colour = function(...) scale_colour_tie_c(),
  ggplot2.continuous.fill = function(...) scale_fill_tie_c()
)





library(tidyverse)
library(data.table)
library(ggplot2)
library(gganimate)
library(ggtext)

df = data.table(X = as.integer(1:200>100))
df[, T := .5 + 2 * X + rnorm(.N)][,
                                  Y := -.5*T + 4*X + 1 + rnorm(.N)][, time := "1"][,
                                                                                   `:=`(mean_T = mean(T), mean_Y = mean(Y)), by = X]

#Calculate correlations
before_cor <- paste("1. Start with raw data. Correlation between T and Y: ",round(cor(df$T,df$Y),3),sep='')
after_cor <- paste("6. Analyze the residuals. Correlation between T and Y controlling for X: ",
                   round(cor(df$T-df$mean_T,df$Y-df$mean_Y),3),sep='')


#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_T=NA, mean_Y = NA, time=before_cor),
  #Step 2: Add x-lines
  df %>% mutate(mean_Y = NA,time = '2. Estimate E[T|X]: differences in T explained by X.'),
  #Step 3: X de-meaned
  df %>% mutate(T = T - mean_T, mean_T = 0, mean_Y = NA, time = "3. Debias: Remove differences in T explained by X."),
  #Step 4: Remove X lines, add Y
  df %>% mutate(T = T - mean_T, mean_T = NA, time = "4. Estimate E[Y|X]: differences in Y explained by X."),
  #Step 5: Y de-meaned
  df %>% mutate(T = T - mean_T, Y = Y - mean_Y, mean_T = NA, mean_Y = 0,time = "5. Denoise: Remove differences in Y explained by X."),
  #Step 6: Raw demeaned data only
  df %>% mutate(T = T - mean_T, Y = Y - mean_Y, mean_T = NA, mean_Y = NA,time = after_cor))

p = ggplot(dffull,aes(y=Y,x=T,color=as.factor(X)))+geom_point()+
  geom_vline(aes(xintercept=mean_T,color=as.factor(X)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(X)))+
  guides(color=guide_legend(title="Covariate X"))+
  # ggtitle("<span style='font-size: 22pt;'>The Relationship between Y and T, Controlling for a Binary Covariate X \n{next_state}</font>") +
  # theme(plot.title = element_markdown()) + 
  labs(title = 'Relationship between Y and T, controlling for a binary covariate X \n{next_state}') +
  theme(
    plot.title = element_text(size=26, color = "#484848", vjust = -5),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey")
    ) + 
  # theme_bw() + 
  transition_states(time,
                    transition_length=c(500,500,500,500,500,500),
                    state_length=c(500,500,500,500,500,500),
                    wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p, duration = 30, fps = 15, height = 600, width =1000, renderer = gifski_renderer())

anim_save("/Users/christophihl/Dropbox/7_Lehre/04_Causal Data Science/_slides/_images/controlForX.gif")




### matching


df <- data.frame(xaxisTime=runif(60),Treatment=c(rep("Treated",5),rep("Control",55))) %>%
  mutate(Y = 3+.4*xaxisTime+1*(Treatment=="Treated")+rnorm(60),
         state="1")

#Make sure the treated obs aren't too close together, that makes it confusing
df[df$Treatment=="Treated",]$xaxisTime <- c(1:5/6)+(runif(5)-.5)*.1

caliper <- .02

df <- df %>%
  mutate(bins = c(rep(filter(df,Treatment=="Treated")$xaxisTime-caliper,6),
                  rep(filter(df,Treatment=="Treated")$xaxisTime+caliper,6))) %>%
  #There has to be a less clunky way to do this
  rowwise() %>%
  mutate(matchmeas = min(abs(xaxisTime-filter(df,Treatment=="Treated")$xaxisTime))) %>%
  mutate(match = matchmeas < caliper) %>%
  group_by(Treatment,match) %>%
  mutate(mean_Y = ifelse(match==1,mean(Y),NA)) %>%
  ungroup()


#Check how many matches we have before proceeding; regenerate randomized data
#until we have a decent number
table(filter(df,Treatment=="Control")$match)

dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(bins=NA,mean_Y=NA,state='1. Start with raw data.'),
  #Step 2: Add Y-lines
  df %>% mutate(mean_Y=NA,state='2. Look for Controls with similar X values to the Treatments.'),
  #Step 3: Drop unmatch obs
  df %>% mutate(Y = ifelse(match==1,Y,NA),mean_Y=NA,state="3. Keep Controls only if they're similar enough."),
  #Step 4: Take means
  df %>% mutate(Y = ifelse(match==1,Y,NA),bins=NA,state="4. Among what's kept, see what the treatment explains."),
  #Step 5: Eliminate everything but the means
  df %>% mutate(Y = ifelse(match==1,mean_Y,NA),bins=NA,state="5. Ignore everything not explained by treatment."),
  #Step 6: Get treatment effect
  df %>% mutate(Y = NA,bins=NA,state="6. The treatment effect is the remaining difference."))


p <- ggplot(dffull,aes(y=Y,x=xaxisTime,color=Treatment,size=Treatment))+geom_point()+
  geom_vline(aes(xintercept=bins), color='#FF7E15')+
  geom_hline(aes(yintercept=mean_Y,color=Treatment))+
  geom_segment(aes(x=.5,xend=.5,
                   y=ifelse(state=="6. The treatment effect is the remaining difference.",
                            filter(df,Treatment=="Treated")$mean_Y[1],NA),
                   yend=filter(df,Treatment=="Control",match==TRUE)$mean_Y[1]),size=1.5,color='#FF7E15')+
  scale_size_manual(values=c(2,3))+xlab("X")+
  guides(fill=guide_legend(title="Group"))+
  labs(title = 'The Effect of Treatment on Y while Matching on X (with a caliper) \n{next_state}') +
  theme(
    plot.title = element_text(size=26, color = "#484848", vjust = -5),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                    colour = "grey"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey")
  ) + 
  transition_states(state,transition_length=c(12,16,16,16,16,16),state_length=c(50,36,30,30,30,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

# animate(p, height = 600, width =1000)
animate(p, duration = 30, fps = 15, height = 600, width = 1000, renderer = gifski_renderer())

anim_save("/Users/christophihl/Dropbox/7_Lehre/04_Causal Data Science/_slides/_images/matchfig.gif")



