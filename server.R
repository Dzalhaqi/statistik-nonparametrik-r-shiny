#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #-----------------------------------------------------------------------------------------
    # Wilcoxon test (Comparing two related conditions) data
    #-----------------------------------------------------------------------------------------
    sebelum <- c(25,27,20,21,18,19,20,22,24,25,24,27,23,25,22)
    sesudah <- c(26,26,22,24,22,21,24,21,26,26,25,28,25,27,25)
    
    data_wilcox = data.frame(sebelum,sesudah)
    
    
    #-----------------------------------------------------------------------------------------
    # Mann Whitney Signed Rank Test data
    #-----------------------------------------------------------------------------------------
    data_mw = read.csv("data_mw.csv")
    
    
    #-----------------------------------------------------------------------------------------
    # Mcnemar Test data
    #-----------------------------------------------------------------------------------------
    data_mc = read.csv("smoker.csv", sep = ',')
    
    
    #-----------------------------------------------------------------------------------------
    # Spearman Test data
    #-----------------------------------------------------------------------------------------
    data_sp = read.csv('spearmans-correlation-data.csv',sep = ',')
    
    #-----------------------------------------------------------------------------------------
    # Cochran Test data
    #-----------------------------------------------------------------------------------------
    data_cochran = read.csv('les.csv', sep = ',')
    
    
    #-----------------------------------------------------------------------------------------
    # kruskall-Wallis Test data
    #-----------------------------------------------------------------------------------------
    data_kw_base = read.csv('kruskal-wallis-test-data.csv', sep = ',')
    
    value <- c(data_kw_base$category_a,data_kw_base$category_b,data_kw_base$category_c)
    category <- c(rep(1,25),rep(2,25),rep(3,25))
    
    data_kw <- data.frame(category,value)
    
    
    #-----------------------------------------------------------------------------------------
    # Friedman Test data
    #-----------------------------------------------------------------------------------------
    data_fried_base <- read.csv('Video_R.csv', sep = ',')
    data_fried_base_table <- data.frame(data_fried_base$Person,data_fried_base$TotalAGen,data_fried_base$TotalBdoc,data_fried_base$TotalCOld,data_fried_base$TotalDDEMO)
    
    colnames(data_fried_base_table) = c('Person','Video A', 'Video B', 'Video C', 'Video D')
    
    video_score <- c(data_fried_base$TotalAGen,data_fried_base$TotalBdoc,data_fried_base$TotalCOl,data_fried_base$TotalDDEMO)
    video_category <- c(rep(1,20),rep(2,20),rep(3,20),rep(4,20))
    
    data_fried <- data.frame(data_fried_base$Person,video_category,video_score)
    data_fried
    

    # logo PENS
    output$logo = renderImage({
        list(
            src = "pens.png",
            alt = "logo PENS"
        )
    }, deleteFile = FALSE)
    
    #-----------------------------------------------------------------
    # binomial's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    x_binom <- reactive({
        x_binom <- as.numeric(input$x.binom.test)
    })
    
    n_binom <- reactive({
        n_binom <- as.numeric(input$n.binom.test)
    })
    
    p_binom <- reactive({
        p_binom <- as.numeric(input$p.binom.test)
    })
    
    ci_binom <- reactive({
        ci_binom <- as.numeric(input$ci.binom.test)
    })
    
    alternative_binom <- reactive({
        alternative_binom <- input$alternative.binom.test
    })
    
    output$binom.test.out <- renderPrint({
        x <- x_binom()
        n <- n_binom()
        result <- binom.test(x, n, p = p_binom(), alternative = alternative_binom(), conf.level = ci_binom())
        print(result)
    })
    
    output$binom.exact <- renderPlot({
        x <- x_binom()
        n <- n_binom()
        binom.exact(x, n, p = p_binom(), alternative = alternative_binom(), conf.level = ci_binom(), tsmethod = 'central', plot = TRUE)
    })
    
    outputOptions(output, 'binom.exact', suspendWhenHidden = TRUE)
    
    #-----------------------------------------------------------------
    # wilcoxon's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    paired_wcx <- reactive({
        paired <- as.logical(input$paired_wcx)
    })
    
    ci_wcx <- reactive({
        ci <- as.numeric(input$ci_wcx)
    })
    
    correct_wcx <- reactive({
        correct <- as.logical(input$correct_wcx)
    })
    
    alternative_wcx <- reactive({
        alternative <- input$alternative_wcx
    })
    
    
    output$wilcox.test.out <- renderPrint({
        result <- wilcox.test(sebelum, sesudah, paired = paired_wcx(), correct = correct_wcx() ,conf.level = ci_wcx(), alternative = alternative_wcx())
        print(result)
    })
    
    output$summary_wcx <- renderPrint({
        summary_data_wilcox <- summary(data_wilcox)
        print(summary_data_wilcox)
    })
    
    output$table_data_wcx <- renderDataTable({
        datatable(
            data_wilcox,
            options = list(scrollX = TRUE)
        )
    })
    
    wilcox.boxplot <- function(){
        boxplot(data_wilcox, las=1, main = 'Perbadingan berat badan anak')
        beeswarm(data_wilcox, col = 4, pch = 16, vert = TRUE,  add = TRUE)
    }
    
    output$wilcox.boxplot.out <- renderPlot({
        wilcox.boxplot()
    })
    
    #-----------------------------------------------------------------
    # mann-whitney's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    paired_mw <- reactive({
        paired <- as.logical(input$paired_mw)
    })
    
    ci_mw <- reactive({
        ci <- as.numeric(input$ci_mw)
    })
    
    correct_mw <- reactive({
        correct <- as.logical(input$correct_mw)
    })
    
    alternative_mw <- reactive({
        alternative <- input$alternative_mw
    })
    
    output$mw.test.out <- renderPrint({
        result <- wilcox.test(data_mw$cortisol_level_sebelum, data_mw$cortisol_level_sesudah, paired = paired_mw(), correct = correct_mw(), conf.level = ci_mw(), alternative = alternative_mw())
        print(result)
    })
    
    
    mw.boxplot <- function(){
        boxplot(data_mw, las=1, main = 'Perbadingan hormon kortisol yang dihasilkan oleh mahasiswa (mcg/dL).')
        beeswarm(data_mw, col = 4, pch = 16, vert = TRUE,  add = TRUE)
    }
    
    output$mw.boxplot.out <- renderPlot({
        mw.boxplot()
    })
    
    output$table_data_mw <- renderDataTable({
        datatable(
            data_mw,
            options = list(scrollX = TRUE)
        )
    })
    
    output$summary_mw <- renderPrint({
        summary_data_mw <- summary(data_mw)
        print(summary_data_mw)
    })
    
    
    #-----------------------------------------------------------------
    # mcnemar's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    output$data_mc = renderDataTable({
        datatable(
            data_mc,
            options = list(scrollx = TRUE)
        )
    })
    
    contigency_data_mc <- reactive({
        x <- table(data_mc)
        x <- addmargins(x)
        print(x)
    })
    
    output$contigency.out <- renderPrint({
        contigency_data_mc()
    })
    
    mc_test <- reactive({
        smoker <- table(data_mc)
        res1 <- mcnemar.test(smoker)
        res2 <- mcnemar.exact(smoker)
        
        McNemarChi <- paste("McNemar's chi-squared = ", round(res1[[1]][[1]],3), ", ", "df = ", res1[[2]][[1]], sep = "")
        cat(sprintf(McNemarChi), "\n")
        print(res2)
        
    })
    
    output$mc.test.out <- renderPrint({
        mc_test()
    })
    
    makepPlot <- function(){
        
        x <- table(data_mc)
        
        levI <- nrow(x) 
        levJ <- ncol(x) 
        matrix_invers <- as.vector(t(x))
        
        total <- c()
        total_rep <- c()
        for(i in 1:levI) 
        {
            ds <- c()
            for(j in 1:levJ)
            {
                ds <- c(ds, matrix_invers[(i-1)*levJ+j])
            }
            total <- c(total, sum(ds))
            total_rep <- c(total_rep, rep(sum(ds), levJ))
        }
        percentage <- matrix_invers/total_rep 
        
        a <- c()
        for(i in levI:1) 
        {
            for(j in 1:levJ)
            {
                a <- c(a, percentage[(i-1)*levJ+j] )
            }
        }
        
        b <- matrix(c(a), nc=levJ, by=1)
        rownames(b) <- rev(rownames(x))
        colnames(b) <- colnames(x)
        
        par(mar=c(5,6,2,4))
        barplot(t(b), hor=1, las=1, xlab="Percentage", col=gray.colors(ncol(x)))
        legend("bottomright", legend=colnames(b), fill=gray.colors(ncol(x)))
    }
    
    output$mc.pPlot <- renderPlot({
        print(makepPlot())
    })
    
    makemPlot <- function(){
        x <- table(data_mc)
        mosaic(x, gp = shading_max, legend=FALSE)
    }
    
    output$mc.mPlot <- renderPlot({
        print(makemPlot())
    })
    
    #-----------------------------------------------------------------
    # spearman's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    output$data_sp <- renderDataTable({
        datatable(
            data = data_sp,
            options = list(scrollx = TRUE)
        )
    })
    
    output$sp.bs <- renderPrint({
        data_spearman = as.matrix(data_sp)
        describe(data_spearman)
    })
    
    correl <- reactive({
        x <- as.matrix(data_sp)
        round(cor(cbind(x), method = 'spearman', use = "pairwise.complete.obs"),3)
    })
    
    output$sp.cor <- renderPrint({
        correl()
    })
    
    ci.sp <- reactive({
        ci.sp <- as.numeric(input$ci.sp) 
    })
    
    correl.test <- reactive({
        temperature <- data_sp$temperature 
        ice_cream_sales <- data_sp$ice_cream_sales
        cor.test(temperature,ice_cream_sales,conf.level = ci.sp())
    })
    
    output$correl.test <- renderPrint({
        correl.test()
    })
    
    makecorPlot <- function(){
        x <- as.matrix(data_sp)
        pairs.panels(x, method = 'spearman')
    }
    
    output$corPlot <- renderPlot({
        makecorPlot()
    })
    
    #-----------------------------------------------------------------
    # kruskall-wallis's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    output$data_kw_base <- renderDataTable({
        datatable(
            data = data_kw_base,
            options = list(scrollX = TRUE)
        )
    })
    
    KW.bs <- reactive({
        describeBy(data_kw[,2], data_kw[,1])
    })
    
    output$KW.bs.out <- renderPrint({
        KW.bs()
    })
    
    KW.ranking <- reactive({
        ranked <- rank(data_kw[,2])
        data <- data.frame(data_kw[,1], ranked)
        n <- round(tapply(data[,2], data[,1], length),2)
        m <- round(tapply(data[,2], data[,1], mean),2)
        t <- round(tapply(data[,2], data[,1], sum),2)
        ranks <- data.frame(n, m, t)
        colnames(ranks) <- c("n","Rank Mean","Rank Sum")
        print(ranks)
    })
    
    output$KW.ranking.out <- renderPrint({
        KW.ranking()
    })
    
    KW.test <- reactive({
        result <- kruskal.test(data_kw[,2] ~ data_kw[,1])
        print(result)
    })
    
    output$KW.test.out <- renderPrint({
        KW.test()
    })
    
    KW.boxPlot <- function(){
        dat <- data_kw
        boxplot(data_kw$value ~ data_kw$category, las=1, horizontal = TRUE, main = 'Perbadingan kategori pasien berdasarkan jumlah hormon kortisol dengan satuan microgram per deciliter (mcg/dL).')
        beeswarm(dat[,2] ~ dat[,1], col = 4, pch = 16, vert = TRUE,  add = TRUE)
    }
    
    output$KW.boxPlot <- renderPlot({
        KW.boxPlot()
    })
    
    #-----------------------------------------------------------------
    # cochran's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    output$data_cochran <- renderDataTable({
        datatable(
            data = data_cochran,
            options = list(scrollx = TRUE)
        )
    }) 
    
    data.cochran <- reactive({
        
        dat <- data_cochran
        dat[,1] <- factor(dat[,1])
        data.long <- melt(dat, idvars=dat[,1])
        x <- t(table(data.long$variable, data.long$value))
        x <- addmargins(x)
        
        print(x)
    })
    
    output$data.cochran.out <- renderPrint({
        data.cochran()
    })
    
    cochran.test <- reactive({
        dat <- data_cochran
        
        dat[,1] <- factor(dat[,1])
        data.long <- melt(dat, idvars=dat[,1])
        q <- symmetry_test(data.long[,3] ~ factor(data.long[,2]) | factor(data.long[,1]), data=data.long, teststat="quad")
        
        CochranQChi <- paste("Cochran's Q chi-squared = ", round(q@statistic@teststatistic,3), ", ", "df = ", q@statistic@df, sep = "")
        cat(sprintf(CochranQChi), "\n")
        
        P.CochranQChi <- paste("p-value = ", pvalue(q), sep = "")
        cat(sprintf(P.CochranQChi), "\n", "\n")
        
        
        cat("Effect size for Cochran's Q test:", "\n")
        eta.squared.q <- q@statistic@teststatistic / (nrow(dat) * ((ncol(dat)-1)-1))
        ESQ <- paste("Eta-squared Q = ", round(eta.squared.q,3), sep = "")
        cat(sprintf(ESQ), "\n", "\n", "\n")
    })
    
    output$cochran.test.out <- renderPrint({
        cochran.test()
    })
    
    makepPlot.ch <- function(){
        
        dat <- data_cochran
        
        dat[,1] <- factor(dat[,1])
        data.long <- melt(dat, idvars=dat[,1])
        x <- t(table(data.long$variable, data.long$value))
        n <- nrow(dat)
        prp <- round(((x/n)*100), 1)
        prp.rev <- apply(prp, 1, rev)
        
        par(mar=c(5,6,2,4))
        barplot(t(prp.rev), hor=1, las=1, xlab="Percentage",main = 'Proporsi data')
        legend("bottomright", legend=rownames(x), fill=gray.colors(nrow(x)))
    }
    
    output$pPlot.ch <- renderPlot({
        print(makepPlot.ch())
    })
    
    #-----------------------------------------------------------------
    # friedman's ui widget and data preprocessing
    #-----------------------------------------------------------------
    
    output$data_fried_base <- renderDataTable({
        datatable(
            data = data_fried_base_table,
            options = list(scrollx = TRUE)
        )
    }) 
    
    fried.bs <- reactive({
        describeBy(data_fried[,3], data_fried[,2])
    })
    
    output$fried.bs.out <- renderPrint({
        fried.bs()
    })
    
    fried.ranking <- reactive({
        ranked <- rank(data_fried[,3])
        data <- data.frame(data_fried[,2], ranked)
        n <- round(tapply(data[,2], data[,1], length),2)
        m <- round(tapply(data[,2], data[,1], mean),2)
        t <- round(tapply(data[,2], data[,1], sum),2)
        ranks <- data.frame(n, m, t)
        colnames(ranks) <- c("n","Rank Mean","Rank Sum")
        print(ranks)
    })
    
    output$fried.ranking.out <- renderPrint({
        fried.ranking()
    })
    
    fried.test <- reactive({
        data_video_score = as.matrix(data_fried)
        result <- friedman.test(data_video_score)
        print(result)
    })
    
    output$fried.test.out <- renderPrint({
        fried.test()
    })
    
    Friedman.boxPlot <- function(){
        dat <- data_fried_base_table[,-1]
        boxplot(dat, las=1, horizontal = TRUE, main = 'Perbadingan Score Tiap Video')
        beeswarm(dat, col = 4, pch = 16, vert = TRUE,  add = TRUE)
    }
    
    output$Friedman.boxPlot <- renderPlot({
        Friedman.boxPlot()
    })
    

})
