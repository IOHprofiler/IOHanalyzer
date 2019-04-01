#
# This is the user interface of the Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

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

  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">Performance Evaluation for Iterative Optimization Heuristics</span>\');
      })
     ')),

  tags$script(HTML('
       window.setInterval(function() {
        var elem = document.getElementById("process_data_promt");
                   elem.scrollTop = elem.scrollHeight;
                   }, 20);
  ')),

  tags$script(HTML('
       window.setInterval(function() {
                   var elem = document.getElementById("upload_data_promt");
                   elem.scrollTop = elem.scrollHeight;
                   }, 20);
  ')),

  # using MathJax
  HTML("<head><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML'
       async></script></head>"),

  # tabitems ----------------------
  tabItems(
    tabItem(tabName = 'about', includeMarkdown('RMD/about.Rmd')),

    tabItem(tabName = 'readme', includeMarkdown('RMD/README.md')),

    # data uploading functionalities -----------------
    tabItem(tabName = 'upload',
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
    tabItem(tabName = 'ERT_convergence',
            fluidRow(
              column(
                width = 12,
                ERT_box(collapsed = F),
                ERT_agg_box(height = '800px'),
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
    tabItem(tabName = 'RT_ECDF',
            fluidRow(
              column(
                width = 12,
                rt_ecdf_single_target_box(collapsed = F),
                rt_ecdf_agg_targets_box(),
                rt_ecdf_agg_fct_box(),
                rt_ecdf_auc_box()
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
    tabItem(tabName = 'FCE_convergence',
            fluidRow(
              column(
                width = 12,
                fv_per_fct_box(),
                fv_agg_box(),
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
                fv_ecdf_single_budget_box(),
                fv_ecdf_agg_budgets_box(),
                fv_ecdf_auc_box()
              )
            )
    ),

    # Parameter tab -------
    tabItem(tabName = 'PARAMETER',
            fluidRow(
              column(
                width = 12,
                par_expected_value_box(),
                par_summary_box(),
                par_sample_box()
              )
            )
    )
  )
)

# -----------------------------------------------------------
dashboardPage(title = 'IOHanalyzer', header, sidebar, body)
