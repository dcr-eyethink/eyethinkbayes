bayes_mm_interact <- function(bmm,interacts=NULL,binom=F,title=NULL,pred_values=NULL,
                              ribbon=T,ladder=F,spark=F,slope_mpes=T,mfx_sideplot=F,ylim=NULL,
                              contrast_sparkline=F,
                              ...){

  #' Output plot and means table for interactions. Can plot discrete or continuous factors. Can be passed plotting variables for pirateye
  #' @param interacts interactions you want to plot with : separating, gives all if void. Remember that punctuation gets stripped from condition names
  #' @param bmm needs a bayes mixed model object (or a list with one at start)
  #' @param pred_values for continuous variables, a vector of values where predictions are made. Defaults to 10 equally spaced steps
  #' @param ribbon shades area where no evidence lines are different
  #' @param ladder writes MPEs at each predicted value
  #' @param spark plots the MPEs at each predicted value below main plot
  #' @param mfx_sideplot puts a density plot of main effects to the side of a continuous variable
  #' @param ... pirateye plotting parameters
  #' @export

  if (names(bmm[1])=="bmm"){bmm <- bmm$bmm}

  if(is.null(interacts)){interacts <- insight::find_interactions(bmm)$conditional}


  data <- bmm$data
  dvname <- insight::find_terms(bmm)$response

  plot_inter <- list()


  for (int_terms in interacts){
    i <- unlist(strsplit(int_terms,split = ":"))

    nv <- NULL
    fv <- NULL
    for (c in i){  if(!is.factor(data[[c]]))  {nv <- c}else{fv <- c(fv,c)} }

    if(!is.null(nv)){
      if (length(nv)>1){return("We have two continuous variables. I can't handle that")}else{

        ##########################################
        # we have a continuous variable
        ##########################################

                #  we have a single continuous variable, get the slopes and plot lines

        nv_pred <- nv
        if (!is.null(pred_values)){
          nv_pred <-c(paste("age","=","[",paste(pred_values,collapse = ","),"]"))
        }

        means <- modelbased::estimate_means(bmm,at =c(fv,nv_pred))
        cat("\nInteractions: means of levels of  ", i[1], " within levels of ",i[-1])
        print(means)
        setDT(means)
        setnames(means,"Mean",dvname)
        #clipr::write_clip(means)


        conslop <- data.table(modelbased::estimate_contrasts(bmm, at=c(nv_pred,fv[-1]),contrast=fv[1]))

        print(conslop)

        if (slope_mpes){
          slopes <- data.table(modelbased::estimate_slopes(bmm, trend = nv, at=fv))
          slopes$lab <- scales::percent(slopes$pd)
          slopes <- means[,.SD[.N/2],by=fv][slopes,on=fv]
        }else{slopes <- NULL}


        p <- do.call(pirateye,resolve.args(...,data=data,
                                           pred_means =means,xlabs =slopes,pred_line = T,
                                           dv=dvname,plot_condition =c(fv[1],nv,fv[-1]),
                                           line=F,violin=F,dots=F,w=6,error_bars = F,ylim=ylim))

        if (length(unique(data[[fv[1]]]))==2){
          ## we can plot a contrast region

          facet_var <- NULL

          if(length(fv)>1){
            facet_var <- fv[-1]
            fv <- fv[1]
          }

          rib_f <- paste(c(nv,facet_var),collapse = "+")
          rib_f <- paste(rib_f,"~",fv)


          ribdata <- dcast(means,formula = rib_f,value.var = dvname)[conslop,on=c(nv,facet_var)]
          ribdata$condx <- ribdata[[nv]]

          if(!is.null(facet_var)){ribdata$condfacet <- ribdata[[facet_var]]}

          ribdata[,ymin:=min(get(levels(means[[fv[1]]])[1]), get(levels(means[[fv[1]]])[2]) ),by=c(nv,facet_var)]
          ribdata[,ymax:=max(get(levels(means[[fv[1]]])[1]), get(levels(means[[fv[1]]])[2]) ),by=c(nv,facet_var)]

          ribdata[,y:=mean(c(ymin,ymax)),by=c(nv,facet_var)]

          ribdata[,ytop:=Difference*0.4+y,by=c(nv,facet_var)]
          ribdata[,ybot:=y- Difference*0.4,by=c(nv,facet_var)]


          if(ribbon){
            p <-    p+ ggnewscale::new_scale_fill()+
              geom_ribbon(data=ribdata,inherit.aes = F,
                          aes(x=condx,ymin=ymin,ymax=ymax,
                              fill=ifelse(pd<.90,"black", NA)),alpha=.2)+scale_fill_identity()
          }

          if(ladder){

            p <- p+  geom_segment(data=ribdata,inherit.aes = F,colour="dark grey",
                                  arrow = arrow(ends = "both",length = unit(.02,units ="npc" )),
                                  aes(x=condx,xend=condx,y=ybot,yend=ytop))+
              geom_label(data=ribdata,inherit.aes = F,
                         aes(x=condx,y=y,label=scales::percent(pd,accuracy = 1)),colour="dark grey")
          }


          if(spark){

            conslop[,linecol:=ifelse(pd<.9,"yellow","red")]


            #spark <- ggplot(conslop,aes_string(x=nv,y="pd",colour="pd",group=1))+
            spark <- ggplot(conslop,aes(x=.data[[nv]],y=pd,colour=pd,group=1))+
              geom_hline(yintercept = .9,colour="grey")+
              geom_line(linewidth=1.5)+facet_wrap(paste(fv[-1],"~.")) #+scale_color_identity()

            spark <- spark+ scale_color_gradientn(colours = c("darkblue","blue","blue", "darkgreen", "green"),
                                                  values = c(0.5,.8,.80001, .9, 1))

            # spark <-  spark ++theme_void()+ theme(
            #   strip.background = element_blank(),
            #   strip.text.x = element_blank()
            # )
            spark <- spark+  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                                name="MPE", limits=c(0.5, 1))+theme_bw()+
              theme(axis.title.x = element_blank(),
                    strip.background = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    strip.text.x = element_blank(),axis.text.x = element_blank(),
                    panel.border = element_blank(),legend.position = "none")


            p <- cowplot::plot_grid(plotlist = list(p,spark),ncol = 1,rel_heights = c(6,1),
                                    align = "v",axis="b")

          }


          if (mfx_sideplot){


            if(is.null(ylim)){
              ylim <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
            }

            mfx <- bayes_mm_mainFX(bmm,mainfxs =fv  )$data_mainfx$predicted

            mfxp <- ggplot(data=mfx,aes(colour=levels,x=ratnorm,fill=levels))+
              geom_density(linewidth=0,alpha=.4)+
              geom_vline(data=mfx[,mean(ratnorm),by=levels],
                         aes(xintercept=V1,colour=levels),linewidth=2)


            mfxp <- mfxp + theme_bw()+xlim(ylim)+coord_flip()+scale_y_reverse()+theme_bw()+
              theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    strip.background = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    strip.text.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x =  element_blank(),
                    strip.text.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y =  element_blank(),
                    panel.border = element_blank(),legend.position = "none")


            p <-  cowplot::plot_grid(plotlist = list(mfxp,p+ theme(legend.position = "none",
                                                                   axis.title.y = element_blank())),nrow = 1,rel_widths  = c(1,6),
                                     align = "h",axis="l")
          }


        }
      }

    }else{

      ##########################################
      # we have a factor variable
      ##########################################
            # we have all categorical factors, so get contrasts and plot means

      means <- modelbased::estimate_means(bmm,at =i)
      cat("\nInteractions: means of levels of  ", i[1], " within levels of ",i[-1])
      print(means)
      setDT(means)
      setnames(means,"Mean",dvname)
      #clipr::write_clip(means)

      cons <-  modelbased::estimate_contrasts(bmm,contrast = i[1],at=i[-1])
      cat("\nInterations: contrasts between levels of  ", i[1], " within levels of ",i[-1])
      print(cons)


      setDT(cons)
      cons[,mpes:=scales::percent(round(cons$pd,4))]

      ## get post distributions and reshape into long
      bmm_fx <- emmeans::emmeans(bmm,specs = i)
      post <- data.table(as.matrix(emmeans::as.mcmc.emmGrid(bmm_fx)))
      post <- melt(post,measure.vars = colnames(post),value.name = dvname)

      post[,c(i):=tstrsplit(variable,split=" ")[seq(from=2, to=(2*length(i)), by=2)],by=variable]

      ## get contrast post distributions for sparkline
      bcontrasts <- pairs(bmm_fx)
      bcontrasts <- data.frame(as.matrix(emmeans::as.mcmc.emmGrid(bcontrasts)))
      if (binom){bcontrasts <- psych::logistic(bcontrasts)}
      bcontrasts <- melt.data.table(data.table(bcontrasts),measure.vars = colnames(bcontrasts),
                                   value.name = "diff_est")
      bcontrasts[,variable:=gsub(x=variable,pattern="contrast.",replacement=""),by=variable]
      bcontrasts[,variable:=gsub(x=variable,pattern="...",replacement=".",fixed = T),by=variable]

      if (length(i)==2){
        bcontrasts[,c("condl1","condl2"):=tstrsplit(variable,split=".",fixed=T)[c(2,4)],by=variable]
        bcontrasts <- bcontrasts[condl1==condl2]
        bcontrasts$cond <- bcontrasts$condl1
      }else{
        bcontrasts[,c("acondl1","acondl2","bcondl1","bcondl2"):=tstrsplit(variable,split=".",fixed=T)[c(2,5,3,6)],by=variable]
        bcontrasts <- bcontrasts[acondl1==acondl2 & bcondl1==bcondl2]
        bcontrasts[,cond:= paste(bcondl1,acondl2),by=.(acondl1,bcondl2)]
}

      xlabs <- NULL
      if (dim(cons)[1]>0){
        xlabs <- cons
        xlabs$lab <- xlabs$mpes
      }



      p <- do.call(pirateye,resolve.args(data = data.table(bmm$data), plot_condition=i,
                                         pred = post,pred_means = means,
                                         dv = dvname,dots=F,error_bars=F,line = F,ylim=ylim,
                                         title=paste("Interact BayesPirate",title),...,splitV = T,xlabs=xlabs  ))

      if (contrast_sparkline){
        # ? check to see if we have exclusively 2 level conditions, otherwise this isn't going to work
        bcplot <-  bayes_contrastsparkline(bcontrasts = bcontrasts)
        p <-  cowplot::plot_grid(plotlist = list(p,bcplot),ncol = 1,rel_heights = c(6,1),
                                           align = "v",axis="b")
      }


    }

    plot_inter[[paste(i,collapse = ":")]] <- p

  }

  #put it all together to return
  #return(list(plot_inter=plot_inter,pred = post,pred_means = means))
  return(plot_inter)
}
