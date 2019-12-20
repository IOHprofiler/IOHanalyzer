for (f in list.files('ui', pattern = '.R', full.names = T)) {
  source(f)
}

# The side bar layout ---------------------------------------------
sidebar <- dashboardSidebar(
  useShinyjs(),
  hr(),
  sidebar_menu(),
  hr(),
  DIM_fID_panel()
)

body <- dashboardBody(
  tags$style(HTML('.popover-title {color:black;}
                   .popover-content {color:black;}
                   .main-sidebar {z-index:auto;}
                   .fa-exclamation-triangle {color:#E87722})')
  ),
  # javascript, headers ----------------------
  # to show text on the header (heading banner)
  tags$head(tags$style(HTML(
    '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
  tags$head(tags$style(HTML(
    '.box-title {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
  tags$head(
    tags$style(HTML(
      "label { font-size:120%; }"
    ))
  ),
  if (suppressWarnings(require("dashboardthemes", quietly = T))) {
    shinyDashboardThemes(
      theme = "grey_light"
      )
  },
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">Performance Evaluation for Iterative 
        Optimization Heuristics</span>\');
      })
     ')),
  tags$script("
      Shiny.addCustomMessageHandler('background-color', function(color) {
              document.body.style.backgroundColor = color;
              document.body.innerText = color;
              });
              "),
  # tags$script("Shiny.addCustomMessageHandler('set_trace_input', function(color) {
  #               Shiny.setInputValue('ERTPlot_Traces', document.getElementById('ERT_PER_FUN').data.map(trace => trace.visible != 'legendonly'));
  #               document.body.style.backgroundColor = color;
  #               document.body.innerText = color;
  #             });"),
  tags$script(HTML('
       window.setInterval(function() {
        var elem = document.getElementById("process_data_promt");
        if (typeof elem !== "undefined" && elem !== null) elem.scrollTop = elem.scrollHeight;
       }, 20);
  ')),
  tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('manipulateMenuItem', function(message){
        var aNodeList = document.getElementsByTagName('a');

        for (var i = 0; i < aNodeList.length; i++) {
          if(aNodeList[i].getAttribute('data-value') == message.tabName || aNodeList[i].getAttribute('href') == message.tabName) {
            if(message.action == 'hide'){
              aNodeList[i].setAttribute('style', 'display: none;');
            } else {
              aNodeList[i].setAttribute('style', 'display: block;');
            };
          };
        }
      });
    "))),
  tags$script(HTML('
       window.setInterval(function() {
         var elem = document.getElementById("upload_data_promt");
         if (typeof elem !== "undefined" && elem !== null) elem.scrollTop = elem.scrollHeight;
       }, 20);
  ')),

  # using MathJax
  HTML("<head><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML'
       async></script></head>"),
  use_bs_tooltip(),
  use_bs_popover(),
  # tabitems ----------------------
  tabItems(
    tabItem(tabName = 'about', includeMarkdown('markdown/about.md')),
    tabItem(tabName = 'dataformat', includeMarkdown('markdown/dataformat.md')),

    # data uploading functionalities -----------------
    tabItem(tabName = 'upload',
            fluidRow(
              column(width = 12, welcome_bar())
            ),
            fluidRow(
              column(width = 6,
                     upload_box(collapsible = F)
              ),
              column(width = 6,
                     repository_box(collapsible = F)
              )
            ),

            fluidRow(
              column(width = 6,
                     upload_prompt_box(collapsible = F)
              ),
              column(width = 6,
                     data_list_box(collapsible = F)
              )
            )
    ),

    # General data overview ----------------------
    tabItem(tabName = 'overview',
            fluidRow(
              column(width = 12,
                     general_overview_box_all(collapsed = F),
                     general_overview_box_single(collapsed = F)
              )
            )
    ),
    
    # RT (RunTime): Data Summary -----------------
    tabItem(tabName = 'ERT_data',
      fluidRow(
        column(width = 12,
               rt_overview_box(collapsed = F),
               rt_stats_box(collapsed = F),
               rt_sample_box()
          )
      )
    ),

    # RT: Expected Convergence Curve ---------------------------------------------
    tabItem(tabName = 'ERT_convergence_single',
            fluidRow(
              column(
                width = 12,
                ERT_box(collapsed = F),
                ERT_comparison_box_dim()
              )
            )
    ),

    tabItem(tabName = 'ERT_convergence_aggr',
            fluidRow(
              column(
                width = 12,
                ERT_agg_box(height = '800px', collapsed = F),
                ERT_comparison_box()
              )
            )
    ),
    
    # RT: histograms, violin plots ------------------------------------------
    tabItem(tabName = 'RT_PMF',
          fluidRow(
            column(
              width = 12,
              rt_histogram_box(collapsed = F),
              rt_pmf_box()
            )
          )
    ),

    # RT ECDF ------------------------------------------
    tabItem(tabName = 'RT_ECDF_single',
            fluidRow(
              column(
                width = 12,
                rt_ecdf_single_target_box(collapsed = F),
                rt_ecdf_agg_targets_box(),
                rt_ecdf_auc_box()
              )
            )
    ),
    tabItem(tabName = 'RT_ECDF_aggr',
            fluidRow(
              column(
                width = 12,
                rt_ecdf_agg_fct_box(collapsed = F)
              )
            )
    ),

    # Parameter tab -------
    tabItem(tabName = 'RT_PARAMETER',
            fluidRow(
              column(
                width = 12,
                rt_par_summary_box(collapsed = F),
                rt_par_expected_value_box(),
                rt_par_sample_box()
              )
            )
    ),

    tabItem(tabName = 'RT_Statistics_single',
            fluidRow(
              column(
                width = 12,
                rt_heatmap_box()
              )
            )
    ),
    tabItem(tabName = 'RT_Statistics_aggr',
            fluidRow(
              column(
                width = 12,
                rt_glicko2_box(collapsed = F)
              )
            )
    ),    

    # FCE: Data Summary -----------------
    tabItem(tabName = 'FCE_DATA',
            fluidRow(
              column(
                width = 12,
                fv_overview_box(collapsed = F),
                fv_stats_box(collapsed = F),
                fv_sample_box()
              )
            )
    ),

    # FCE: Expected Convergence Curve ---------------------------------------------
    tabItem(tabName = 'FCE_convergence_single',
            fluidRow(
              column(
                width = 12,
                fv_per_fct_box(collapsed = F)
               )
            )
    ),

    tabItem(tabName = 'FCE_convergence_aggr',
            fluidRow(
              column(
                width = 12,
                fv_agg_box(collapsed = F),
                fv_comparison_box()
              )
            )
    ),
    # FCE: historgrams, p.d.f. --------
    tabItem(tabName = 'FCE_PDF',
            fluidRow(
              column(
                width = 12,
                fv_histgram_box(collapsed = F),
                fv_pdf_box()
              )
            )
    ),

    # FCE: empirical c.d.f. ------------------------------------------
    tabItem(tabName = 'FCE_ECDF',
            fluidRow(
              column(
                width = 12,
                fv_ecdf_single_budget_box(collapsed = F),
                fv_ecdf_agg_budgets_box(),
                fv_ecdf_auc_box()
              )
            )
    ),
    
    # Parameter tab -------
    tabItem(tabName = 'FCE_PARAMETER',
            fluidRow(
              column(
                width = 12,
                fv_par_expected_value_box(collapsed = F),
                fv_par_summary_box(),
                fv_par_sample_box()
              )
            )
    ),

    tabItem(tabName = 'FCE_Statistics_single',
            fluidRow(
              column(
                width = 12,
                fv_heatmap_box(collapsed = F)
              )
            )
    ),

    tabItem(tabName = 'FCE_Statistics_aggr',
            fluidRow(
              column(
                width = 12,
                fv_glicko2_box(collapsed = F)
              )
            )
    ),
    tabItem(tabName = 'Settings',
            fluidRow(
              column(
                width = 12,
                color_settings_box(),
                general_settings_box()
              )
            )
    )
    # ,
    # 
    # tabItem(tabName = 'Report',
    #         fluidRow(
    #           column(
    #             width = 12,
    #             main_report_box()
    #           )
    #         )
    # )
  )
)

# -----------------------------------------------------------
dashboardPage(title = 'IOHanalyzer', header, sidebar, body)
