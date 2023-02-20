
# library for the application
library(shiny)
library(shinydashboard)
library(DT)
library(png)
library(readxl)
library(dplyr)
library(beeswarm)
library(exact2x2)
library(vcd)
library(psych)
library(reshape2)
library(coin)
library(dashboardthemes)
library(bslib)




# Define UI for application that draws a histogram
shinyUI(
    #-----------------------------------------------------------------------------------------
    # User Interface Code
    #-----------------------------------------------------------------------------------------
dashboardPage(
    dashboardHeader(
        title = shinyDashboardLogo(
            theme = "blue_gradient",
            boldText = "SDT PENS",
            badgeText = "Beta")),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cover", tabName = "intro", icon = icon("address-card-o")),
            menuItem("Binomial Test", tabName = "binomial", icon = icon("table")),
            menuItem("Wilcoxon Test", tabName = "wilcoxon", icon = icon("bar-chart")),
            menuItem("Mann-Whitney Test", tabName = "mw", icon = icon("object-group")),
            menuItem("McNemar Test", tabName = "mcnemar", icon = icon("chart-area")),
            menuItem("Spearman Correlation Test", tabName = "spearman", icon = icon("lightbulb-o")),
            menuItem("Kruskal-Wallis Test", tabName = "kw", icon = icon("chart-line")),
            menuItem("Cochran Test", tabName = "cochran", icon = icon("wave-square")),
            menuItem("Friedman Test", tabName = "friedman", icon = icon("bezier-curve"))
        )
    ),
    ## Body content
    dashboardBody(
        shinyDashboardThemes('blue_gradient'),
        #-----------------------------------------------------------------------------------------
        # ui for cover tab
        #-----------------------------------------------------------------------------------------
        tabItems(
            tabItem(tabName = "intro",
                    fluidRow(
                        box(
                            status = 'primary',
                            width = "100%",
                            column(
                                align = "center",
                                width = 12,
                                h1("Statistics Non-Parametric"),
                                br(),
                                tags$img(src = "https://www.pens.ac.id/wp-content/uploads/2018/02/logoweb.png", width = "800px", height = "250px"),
                                h2("Dibimbing Oleh"),
                                h4("Pak Ronny Susetyoko S.Si., M.Si."),
                                h2("Disusun Oleh"),
                                h4("Muhammad Dzalhaqi - 3321600023"),
                                h4("D-4 Sains Data Terapan")
                            ))
                    )
                    
            ),
            #-----------------------------------------------------------------------------------------
            # ui for binomial tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "binomial",
                    fluidRow(
                        column(
                            12, 
                            box(
                                status = 'primary',
                                width = '100%',
                                column(
                                    12,
                                    align = 'center',
                                    h2("Binomial Test for Statistcs Non-Parametric")
                                ),
                                column(
                                    12, 
                                    align = 'justify',
                                    HTML("<b> Description : </b> <p> Uji Binomial adalah uji non parametrik yang digunakan untuk menguji hipotesis bila dalam populasi terdiri atas dua kelompok kelas dengan tipe data binomial dan ukuran sampelnya kecil. Uji Binomial menggantikan uji statistik t jika asumsi n kecil dan populasi normal sebagai syarat uji t tidak dipenuhi.</p>"),
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            box(
                                status = 'primary',
                                width = '100%',
                                column(
                                    12,
                                    align = 'center',
                                    h2("Sample Case")
                                ),
                                column(
                                    12,
                                    align = 'justify',
                                    p("Terdapat berbagai jenis kayu untuk pembuatan furnitur rumah tangga, seperti Kayu Jati, Kayu Mahogani, Kayu Trembesi, Kayu Sungkai, Kayu Mindi. Sebelum pandemi datang Kayu Jati menjadi jenis kayu yang paling laris dengan total penjualan sebesar 47% dari keseluruhan penjualan, tetapi Semenjak pandemi terjadi pemesanan jenis Kayu Jati disinyalir mengalami penurunan karena memang Kayu Jati merupakan jenis kayu yang paling mahal. Oleh karena itu, dilakukan penelitian dari toal penjualan setelah pandemi dan didapatkan data bahwa dari 137 juta log kayu yang terjual, pemesanan dengan jenis Kayu Jati sebesar 67 juta log."),
                                    p("Kasus ini dapat dibawa ke persoalan binomial yang mana p merupakan proporsi Kayu Jati yang dimpesan. Sehingga jenis kayu selain itu berarti bukan Kayu Jati.")
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                collapsible = TRUE,
                                title = 'Options',
                                column(
                                    12, 
                                    align = 'center',
                                    numericInput('x.binom.test', 'Nilai x', 67, 0),
                                    numericInput('n.binom.test', 'Nilai n', 137, 0),
                                    sliderInput('p.binom.test', 'Nilai p', 0,1,0.47),
                                    sliderInput('ci.binom.test', 'Conf.level', 0,1,0.95),
                                    selectInput('alternative.binom.test', 'Alternative', c('Greater' = 'greater','Two Sided' = 'two.sided', 'Less' = 'less'), selected = 'Greater')
                                )
                            )
                        ),
                        column(
                            8,
                            align = 'center',
                            column(
                                12, 
                                box(
                                    status = 'primary',
                                    width = '100%',
                                    title = 'Result and Plot',
                                    collapsible = TRUE,
                                    verbatimTextOutput('binom.test.out'),
                                    conditionalPanel(
                                        condition = 'input.alternative.binom.test' == 'Two Sided',
                                        plotOutput('binom.exact')
                                    )
                                )
                            )
                        )
                    )
            ),
            #-----------------------------------------------------------------------------------------
            # ui for wilcoxon tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "wilcoxon",
                    fluidRow(
                        column(12,
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   column(
                                       align = "center",
                                       width = 12,
                                       h2("Wilcoxon Test for Statistics Non-Parametric")
                                   ),
                                   column(
                                       12,
                                       align = 'justify',
                                       HTML("<b> Description : </b> <p> Uji wilcoxon merupakan uji non parametrik yang digunakan untuk mengukur ada tidaknya perbedaan nilai rata-rata dua kelompok sampel yang saling berpasangan (dependen).</p>")
                                   )
                               )       
                        )
                    ),
                    fluidRow(
                        column(12,
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   column(
                                       align = "center",
                                       width = 12,
                                       h3("Sample Case"),
                                   ),
                                   column(12,
                                          align = 'justify',
                                          p("Sebuah perusahaan Pharmasi sedang mengembangkan suplemen penambahan berat badan pada anak-anak. Perusahaan ingin mengetahui khasiat suplemen tersebut sebelum dipasarkan secara komersial. Untuk itu perusahaan mencoba obat tersebut secara kontinu terhadap 15 orang siswa sekolah dasar yang sudah diukur terlebih dahulu berat badannya.Setelah 3 bulan kemudian siswa-siswa tersebut ditimbang berat badannya lagi untuk mngetahui apakah ada peningkatan berat badannya yang nyata.")
                                   )
                               ),       
                        )
                    ),
                    fluidRow(
                        column(12,
                               column(4,
                                      align = "center",
                                      box(
                                          status = 'primary',
                                          width = "100%%",
                                          title = 'Dataset',
                                          collapsible = TRUE,
                                          column(
                                              width = 12,
                                              dataTableOutput("table_data_wcx") 
                                          )
                                      ), 
                               ),
                               column(8,
                                      align = 'center',
                                      box(
                                          status = 'primary',
                                          collapsible = TRUE,
                                          width = "100%",
                                          title = 'Plot',
                                          plotOutput("wilcox.boxplot.out")
                                      ),
                               ),
                               
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            align = "center",
                            box(
                                status = 'primary',
                                collapsible = TRUE,
                                title = 'Options',
                                br(),
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 3,
                                    selectInput("paired_wcx", "Paired", c("TRUE" = TRUE, "FALSE" = FALSE), selected = "TRUE")
                                ),
                                column(
                                    align = "center",
                                    width = 3,
                                    selectInput("correct_wcx", "Correct", c("TRUE" = TRUE, "FALSE" = FALSE), selected = "TRUE")
                                ),
                                column(
                                    align = "center",
                                    width = 3,
                                    selectInput("alternative_wcx", "Alternative", c("Two Sided" = "two.sided","Greater" = "greater", "Less" = "less"), selected = "Greater")
                                    
                                ),
                                column(
                                    align = "center",
                                    width = 3,
                                    sliderInput("ci","Conf.level", 0.5,1,0.95)
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            align = "center",
                            box(
                                status = 'primary',
                                title = 'Result and Descriptive Statistics',
                                collapsible = TRUE,
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 6,
                                    h4("Result Wilcoxon Test"),
                                    verbatimTextOutput("wilcox.test.out")
                                ),
                                column(
                                    align = "center",
                                    width = 6,
                                    h4("Descriptive Statistics"),
                                    verbatimTextOutput("summary_wcx")
                                ),
                            )
                        )
                        
                    )
            ),
            #-----------------------------------------------------------------------------------------
            # ui for Mann-Whitney tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "mw",
                    fluidRow(
                        column(12,
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   column(
                                       align = "center",
                                       width = 12,
                                       h2("Mann-Whitney Test for Statistics Non-Parametric")
                                   ),
                                   column(
                                       12, 
                                       align = 'justify',
                                       HTML("<b>Description : </b> <p>Uji Mann-Whitney merupakan uji non parametrik yang
digunakan untuk mengetahui perbedaan median 2 kelompok
bebas dengan skala data pengukuran adalah ordinal atau
interval maupun rasio, namun data tersebut tidak berdistribusi
normal. Sintaksis uji tersebut di R adalah sama dengan uji
Wilcoxon.
</p>")
                                   )
                               )
                        )
                    ),
                    fluidRow(
                        column(12,
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   column(
                                       align = "center",
                                       width = 12,
                                       h3("Sample Case")
                                   ),
                                   column(
                                       12,
                                       align = 'justify',
                                       p("Peneliti melakukan eksperimen kepada beberapa mahasiswa untuk melihat total hormon kortisol (hormon pengendali stres) yang dihasilkan saat sebelum dan sesudah diberi tugas besar dengan deadline yang singkat")
                                   )
                               )
                        )
                    ),
                    fluidRow(
                        column(12,
                               column(4,
                                      align = "center",
                                      box(
                                          status = 'primary',
                                          width = "100%%",
                                          title = 'Dataset',
                                          collapsible = TRUE,
                                          dataTableOutput("table_data_mw") 
                                      ), 
                               ),
                               column(8,
                                      align = "center",
                                      box(
                                          status = 'primary',
                                          width = "100%",
                                          title = 'Plot',
                                          collapsible = TRUE,
                                          plotOutput("mw.boxplot.out")
                                      ),
                               ),
                               
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12,
                            align = "center",
                            box(
                                status = 'primary',
                                title = 'Options',
                                collapsible = TRUE,
                                br(),
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 3,
                                    selectInput("paired_mw", "Paired", c("TRUE" = TRUE, "FALSE" = FALSE), selected = "TRUE")
                                ),
                                column(
                                    align = "center",
                                    width = 3,
                                    selectInput("correct_mw", "Correct", c("TRUE" = TRUE, "FALSE" = FALSE), selected = "TRUE")
                                ),
                                column(
                                    align = "center",
                                    width = 3,
                                    selectInput("alternative_mw", "Alternative", c("Two Sided" = "two.sided","Greater" = "greater", "Less" = "less"), selected = "Greater")
                                    
                                ),
                                column(
                                    align = "center",
                                    width = 3,
                                    sliderInput("ci_mw","Conf.level", 0.5,1,0.95)
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 12, 
                            align = "center",
                            box(
                                status = 'primary',
                                title = 'Result and Descriptive Statistics',
                                collapsible = TRUE,
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 6,
                                    h3("Result Mann-Whitney Test"),
                                    verbatimTextOutput("mw.test.out")
                                ),
                                column(
                                    align = "center",
                                    width = 6,
                                    h3("Descriptive Statistics"),
                                    verbatimTextOutput("summary_mw")
                                ),
                            )
                        )
                        
                    )
            ),
            #-----------------------------------------------------------------------------------------
            # ui for McNemar tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "mcnemar",
                    fluidRow(
                        column(12,
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   column(
                                       align = "center",
                                       width = 12,
                                       h2("McNemar's Test for Statistics Non-Parametric")
                                   ),
                                   column(
                                       12,
                                       align = 'justify',
                                       HTML(
                                           '<b>Description : </b> <p>Uji McNemar merupakan uji non parametrik yang digunakan
untuk menguji dua kelompok data berpasangan (dependen).
Pada uji McNemar, sekelompok subjek penelitian atau
responden memberikan suatu penilaian sebelum dan sesudah
perlakuan (treatment). Data tersebut mempunyai skala
pengukuran nominal atau ordinal. Jika data termasuk skala 
pengukuran interval/rasio dan diasumsikan berdistribusi
normal maka menggunakan uji-t </p>'
                                       )
                                   )
                               )
                        )
                    ),
                    fluidRow(
                        column(12,
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   column(
                                       align = "center",
                                       width = 12,
                                       h3("Sample Case")
                                   ), 
                                   column(
                                       12, 
                                       align = 'justify',
                                       p("Seorang peneliti ingin menyelidiki dampak intervensi terhadap merokok. Dalam studi hipotetis ini, 50 peserta direkrut untuk ambil bagian, terdiri dari 25 perokok dan 25 non-perokok. Semua peserta menonton video emotif yang menunjukkan dampak kematian akibat kanker terkait merokok terhadap keluarga. Dua minggu setelah intervensi video ini, peserta yang sama ditanya apakah mereka tetap perokok atau bukan perokok.")
                                   )
                               )
                        )
                    ),
                    fluidRow(
                        column(4,
                               align = 'center',
                               box(
                                   status = 'primary',
                                   title = 'Dataset',
                                   width = "100%",
                                   collapsible = TRUE,
                                   dataTableOutput("data_mc")
                               )
                        ),
                        column(8,
                               align = 'center',
                               box(
                                   status = 'primary',
                                   title = 'Contigency and Result',
                                   width = "100%",
                                   collapsible = TRUE,
                                   column(12,
                                          h4('Contigency'),
                                          align = "center",
                                          verbatimTextOutput("contigency.out")
                                   ),
                                   column(12,         
                                          h4('Result'),
                                          align = "center",
                                          verbatimTextOutput("mc.test.out")
                                   )
                               )
                        )
                    ),
                    fluidRow(
                        column(12,
                               align = "center",
                               box(
                                   status = 'primary',
                                   title = 'Plot',
                                   width = "100%",
                                   collapsible = TRUE,
                                   column(6,
                                          align = "center",
                                          h4('Stacked Bar'),
                                          plotOutput("mc.pPlot"),
                                   ),
                                   column(6,
                                          align = "center",
                                          h4('Mosaic'),
                                          plotOutput("mc.mPlot")   
                                   )
                               )
                        )
                    )
            ),
            #-----------------------------------------------------------------------------------------
            # ui for Spearman tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "spearman",
                    fluidRow(
                        column(
                            12, 
                            box(
                                status = 'primary',
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 12,
                                    h2("Spearman Correlation Test for Statistics Non-Parametric")
                                ),
                                column(
                                    12,
                                    align = 'justify',
                                    HTML(
                                        '<b>Description : </b> <p>Uji korelasi Spearman adalah uji statistik non parametrik yang
bertujuan untuk mengetahui hubungan antara dua atau lebih
variabel berskala ordinal.</p>'
                                    )
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            box(
                                status = 'primary',
                                width = '100%',
                                column(
                                    12,
                                    align = 'center',
                                    h2('Sample Case')
                                ),
                                column(
                                    12,
                                    align = 'justify',
                                    p('Penelitian dilakukan terhadap penjualan es krim berdasarkan rata-rata suhu di suatu tempat, dari data tersebut dapat dilihat korelasinya dengan menggunakan uji statistik non parametrik dengan menggunakan metode korelasi spearman.')
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                title = 'Dataset',
                                collapsible = TRUE,
                                dataTableOutput('data_sp')
                            )
                        ),
                        column(
                            8,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                title = 'Result',
                                collapsible = TRUE,
                                column(
                                    12, 
                                    align = 'center',
                                    h4('Descriptive Statistic'),
                                    verbatimTextOutput('sp.bs'),
                                    h4('Correlation'),
                                    verbatimTextOutput('sp.cor'),
                                    sliderInput('ci.sp','Conf.level',0.5,1,0.95),
                                    h4('Correlation Test'),
                                    verbatimTextOutput('correl.test')
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            align = 'center',
                            12,
                            box(
                                status = 'primary',
                                width = '100%',
                                title = 'Plot',
                                collapsible = TRUE,
                                plotOutput('corPlot')
                            )
                        )
                    )
                    
            ),
            #-----------------------------------------------------------------------------------------
            # ui for Kruskall-waliis tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "kw",
                    fluidRow(
                        column(
                            12,
                            box(
                                status = 'primary',
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 12,
                                    h2("Kruskal-Wallis Test for Statistics Non-Parametric")
                                ),
                                column(
                                    12,
                                    align = 'justify',
                                    HTML(
                                        '<b>Description : </b> <p>Uji kruskal Wallis adalah uji statistik non parametrik yang dapat
digunakan untuk menguji apakah ada perbedaan yang
signifikan antara kelompok variabel. Karena untuk melihat
perbedaan yang signifikan antar kelompok, uji ini jelas
digunakan untuk melihat perbandingan lebih dari 2 kelompok
populasi dengan data berbentuk ranking. Uji ini juga disebut
sebagai uji kruskal-wallis H, atau H-test, yang merupakan
perluasan uji 2 sampel wilcoxon untuk k > 2 sampel.
Uji Kruskal Wallis merupakan uji alternatif untuk uji F dan uji
one way Anova (Analysis od Variance) untuk pengujian
kesamaan beberapa nilai tengah dan analisis ragam yang dapat
kita gunakan jika asumsi kenormalan tidak terpenuhi atau
berdistribusi bebas.</p>'
                                    )
                                )
                            )
                        ),
                    ),
                    fluidRow(
                        column(
                            12,
                            box(
                                status = 'primary',
                                width = '100%',
                                column(
                                    align = 'center',
                                    width = 12,
                                    h2('Sample Case')
                                ),
                                column(
                                    align = 'justify',
                                    width = 12,
                                    p('Penelitian dilakukan terhadap 75 pasien dengan 3 kategori di rumah sakit dengan mengukur tingkat hormon kortisol yang dihasilkan. pasien dengan kategori A menandakan bahwa pasien memiliki sindrom cushing, pasien dengan kategori B menandakan bahwa pasien memiliki penyakit jantung koroner, pasien dengan kategori C menandakan bahwa pasien dengan pengontrolan hormon kortisol'),
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            4,
                            align = 'center',
                            box(
                                status = 'primary',
                                title = 'Dataset',
                                width = '100%',
                                collapsible = TRUE,
                                dataTableOutput('data_kw_base')
                            )
                        ),
                        column(
                            8,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                title = 'Descriptive Statistics and Result',
                                collapsible = TRUE,
                                column(
                                    12, 
                                    align =  'center',
                                    h4('Descriptive Statistics'),
                                    verbatimTextOutput('KW.bs.out'),
                                    h4('Rank'),
                                    verbatimTextOutput('KW.ranking.out'),
                                    h4('Result'),
                                    verbatimTextOutput('KW.test.out')
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            align = 'center',
                            box(
                                status = 'primary',
                                title = 'Plot',
                                width = '100%',
                                collapsible = TRUE,
                                plotOutput('KW.boxPlot')
                            )
                        )
                    )
                    
            ),
            #-----------------------------------------------------------------------------------------
            # ui for Cochran tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "cochran",
                    fluidRow(
                        column(
                            12,
                            align = "center",
                            box(
                                status = 'primary',
                                width = "100%",
                                column(
                                    12, 
                                    align = 'center',
                                    h2("Cochran Test for Statistics Non-Parametric")
                                ), 
                                column(
                                    12,
                                    align = 'justify',
                                    HTML(
                                        '<b>Description :</b> <p>Uji Cochran merupakan perluasan dari Uji McNemar dan
digunakan untuk menguji apakah tiga atau lebih sampel
berbeda signifikan dalam hal proporsi atau frekuensinya. Uji ini
menggunakan data minimal berskala nominal yang bersifat
dikotomi (misalnya: sukses atau gagal, suka atau tidak suka,
setuju atau tidak setuju).
</p>'
                                    )
                                )
                            ),
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            box(
                                status = 'primary',
                                width = '100%',
                                column(
                                    12,
                                    align = 'center',
                                    h2("Sample Case")
                                ),
                                column(
                                    12,
                                    align = 'justify',
                                    p("Penelitian dilakukan terhadap siswa SMA yang mengikuti kegiatan bimbingan belajar di bimbel Nusa Cendikia. Para siswa diberikan tes sebelum dan sesudah mengikuti bimbingan mengenai soal SBMPTN. Angka 1 merepresentasikan bahwa siswa memiliki range nilai yang cukup untuk masuk ke PTN tujuan (lolos) berdasarkan prediksi dan angka 0 merepresentasikan bahwa siswa belum mencapai range nilai minimun untuk masuk ke PTN tujuan (tidak lolos) berdasarkan prediksi")
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(4,
                               align = 'center',
                               box(
                                   status = 'primary',
                                   width = "100%",
                                   collapsible = TRUE,
                                   title = 'Dataset',
                                   dataTableOutput("data_cochran")
                               )
                        ),
                        column(
                            8,
                            align = 'center',
                            fluidRow(
                                box(
                                    status = 'primary',
                                    title = 'Data Composition and Result',
                                    width = "100%",
                                    collapsible = TRUE,
                                    column(
                                        12, 
                                        h4('Data Composition'),
                                        verbatimTextOutput("data.cochran.out"),
                                        h4('Result'),
                                        verbatimTextOutput("cochran.test.out")
                                    )
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                collapsible = TRUE,
                                title = 'Plot',
                                plotOutput('pPlot.ch')
                            )
                        )
                    )
            ),
            #-----------------------------------------------------------------------------------------
            # ui for Friedman tab
            #-----------------------------------------------------------------------------------------
            tabItem(tabName = "friedman",
                    fluidRow(
                        column(
                            12,
                            box(
                                status = 'primary',
                                width = "100%",
                                column(
                                    align = "center",
                                    width = 12,
                                    h2("Friedman Test for Statistics Non-Parametric")
                                ),
                                column(
                                    12,
                                    align = 'justify',
                                    HTML(
                                        '<b>Description : </b> <p>Uji Friedman merupakan uji statistik non parametrik yang
digunakan untuk rancangan acak kelompok lengkap. Tujuan uji
Friedman adalah untuk melihat ada atau tidaknya perbedaan
pengaruh antar perlakuan.</p>'
                                    )
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12, 
                            box(
                                status = 'primary',
                                width = '100%',
                                column(
                                    12, 
                                    align = 'center',
                                    h2("Sample Case")
                                ),
                                column(
                                    12, 
                                    align = 'justify',
                                    p("Scott Smith (University of Sheffield) mengevaluasi penggunaan metode terbaik untuk menginformasikan publik tentang kondisi medis tertentu. Ada tiga video (Video umum baru A, video profesi kedokteran baru B, video lama C dan demonstrasi menggunakan alat peraga D). Dia ingin melihat apakah metode baru lebih populer sehingga mengumpulkan data menggunakan sebagian besar pertanyaan gaya Likert tentang berbagai hal seperti pemahaman dan kesan umum. Dari data ini didapatkan skor untuk video A, B, C, dan D")
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            5,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                title = 'Dataset',
                                collapsible = TRUE,
                                dataTableOutput('data_fried_base')
                            )
                        ),
                        column(
                            7,
                            align = 'center',
                            box(
                                status = 'primary',
                                width = '100%',
                                title = 'Result',
                                collapsible = TRUE,
                                column(
                                    12, 
                                    align = 'center',
                                    h4('Descriptive Statistics'),
                                    verbatimTextOutput('fried.bs.out'),
                                    h4('rank'),
                                    verbatimTextOutput('fried.ranking.out'),
                                    h4('Result'),
                                    verbatimTextOutput('fried.test.out')
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            align = 'center',
                            box(
                                status = 'primary',
                                title = 'Plot',
                                collapsible = TRUE,
                                width = '100%',
                                plotOutput('Friedman.boxPlot')
                            )
                        )
                    )
            )
            
        ),
    )
    
)
)
