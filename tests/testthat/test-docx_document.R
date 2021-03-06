# from dput(output_str)
output_str <- c(
  "<!DOCTYPE html>", "", "<html xmlns = \"http://www.w3.org/1999/xhtml\">",
  "", "<head>", "", "<meta charset = \"utf-8\">", "<meta http-equiv = \"Content-Type\" content = \"text/html; charset = utf-8\" />",
  "<meta http-equiv = \"Content-Style-Type\" content = \"text/css\" />",
  "<meta name = \"generator\" content = \"pandoc\" />", "", "<meta name = \"author\" content = \"MG\" />",
  "", "", "<title>Test</title>", "", "<script src = \"FTP_test_files/jquery-1.11.0/jquery.min.js\"></script>",
  "<meta name = \"viewport\" content = \"width = device-width, initial-scale = 1.0\" />",
  "<link href = \"FTP_test_files/bootstrap-2.3.2/css/bootstrap.min.css\" rel = \"stylesheet\" />",
  "<link href = \"FTP_test_files/bootstrap-2.3.2/css/bootstrap-responsive.min.css\" rel = \"stylesheet\" />",
  "<script src = \"FTP_test_files/bootstrap-2.3.2/js/bootstrap.min.js\"></script>",
  "", "<style type = \"text/css\">code{white-space: pre;}</style>",
  "<link rel = \"stylesheet\"", "      href = \"FTP_test_files/highlight/default.css\"",
  "      type = \"text/css\" />", "<script src = \"FTP_test_files/highlight/highlight.js\"></script>",
  "<style type = \"text/css\">", "  pre:not([class]) {", "    background-color: white;",
  "  }", "</style>", "<script type = \"text/javascript\">", "if (window.hljs && document.readyState && document.readyState === \"complete\") {",
  "   window.setTimeout(function() {", "      hljs.initHighlighting();",
  "   }, 0);", "}", "</script>", "", "", "<link rel = \"stylesheet\" href = \"custom.css\" type = \"text/css\" />",
  "", "</head>", "", "<body>", "", "<style type = \"text/css\">",
  ".main-container {", "  max-width: 940px;", "  margin-left: auto;",
  "  margin-right: auto;", "}", "</style>", "<div class = \"container-fluid main-container\">",
  "", "", "<div id = \"header\">", "<h1 class = \"title\" style = \"margin: 24pt 0pt 0pt 0pt;\">Test</h1>",
  "<h4 class = \"author\" style = \"margin: 10pt 0pt 0pt 0pt;\"><em>MG</em></h4>",
  "<h4 class = \"date\" style = \"margin: 10pt 0pt 0pt 0pt;\"><em>Sunday, July 13, 2014</em></h4>",
  "</div>", "", "", "<p>All analyses were performed using R (ver. 3.1.1)[R Core Team, 2013] and packages rms (ver. 4.2-0) [F. Harrell, 2014] for analysis, Gmisc for plot and table output (ver. 0.6.6), and knitr (ver ) [Xie, 2013] for reproducible research.</p>",
  "<div id = \"results\" class = \"section level1\">", "<h1 style = \"margin: 24pt 0pt 0pt 0pt;\">Results</h1>",
  "<p>We found 205 patients with malignant melanoma between the years 1962 and 1977. Patients were followed until the end of 1977, the median follow-up time was 5.5 years (range 0.0 to 15.2 years). Males were more common than females and had also a higher mortality rate.</p>",
  "<table class = 'gmisc_table' style = 'border-collapse: collapse;'  id = 'table_1'>",
  "    <thead>", "    <tr><td colspan = '6' style = 'text-align: left;'>",
  "    ", "Table 1: Baseline characteristics", "</td></tr>", "    <tr>",
  "        <th style = 'border-top: 2px solid grey;'></th>", "        <th colspan = '2' style = 'font-weight: 900; border-top: 2px solid grey; text-align: center;'>",
  " ", "</th><th style = 'border-top: 2px solid grey;; border-bottom: hidden;'>",
  " ", "</th>", "        <th colspan = '2' style = 'font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey;'>",
  "Death", "</th>", "    </tr>", "    <tr>", "        <th style = 'border-bottom: 1px solid grey; '>",
  " ", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "Total", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "Alive", "</th>", "        <th style = 'border-bottom: 1px solid grey;' colspan = '1'>",
  " ", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "Melanoma", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "Non-melanoma", "</th>", "    </tr>", "    </thead><tbody>",
  "    <tr style = 'background-color:#ffffff;'><td colspan = '6' style = 'font-weight: 900;'>",
  "Sex", "</td></tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Male", "</td>",
  "        <td style = 'text-align: right;'>", "126 (61%)", "</td>",
  "        <td style = 'text-align: right;'>", "91 (72%)", "</td>",
  "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "28 (22%)", "</td>", "        <td style = 'text-align: right;'>",
  "7 (6%)", "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Female", "</td>",
  "        <td style = 'text-align: right;'>", "79 (39%)", "</td>",
  "        <td style = 'text-align: right;'>", "43 (54%)", "</td>",
  "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "29 (37%)", "</td>", "        <td style = 'text-align: right;'>",
  "7 (9%)", "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'><td colspan = '6' style = 'font-weight: 900; '>",
  "Age<sup>†</sup>", "</td></tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Mean (SD)", "</td>",
  "        <td style = 'text-align: right;'>", "52 (± 17)", "</td>",
  "        <td style = 'text-align: right;'>", "50 (± 16)", "</td>",
  "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "55 (± 18)", "</td>", "        <td style = 'text-align: right;'>",
  "65 (± 11)", "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'><td colspan = '6' style = 'font-weight: 900; '>",
  "Ulceration", "</td></tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Absent", "</td>",
  "        <td style = 'text-align: right;'>", "115 (56%)", "</td>",
  "        <td style = 'text-align: right;'>", "92 (80%)", "</td>",
  "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "16 (14%)", "</td>", "        <td style = 'text-align: right;'>",
  "7 (6%)", "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Present", "</td>",
  "        <td style = 'text-align: right;'>", "90 (44%)", "</td>",
  "        <td style = 'text-align: right;'>", "42 (47%)", "</td>",
  "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "41 (46%)", "</td>", "        <td style = 'text-align: right;'>",
  "7 (8%)", "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'><td colspan = '6' style = 'font-weight: 900; '>",
  "Thickness<sup>‡</sup>", "</td></tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'border-bottom: 2px solid grey; text-align: left;'>",
  "  Mean (SD)", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "2.9 (±3.0)", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "2.2 (±2.3)", "</td>", "        <td style = 'border-bottom: 2px solid grey;' colspan = '1'>",
  " ", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "4.3 (±3.6)", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "3.7 (±3.6)", "</td>", "    </tr>", "    </tbody>", "    <tfoot><tr><td colspan = 6>",
  "    ", "<sup>†</sup> Age at the time of surgery. <br/><sup>‡</sup> Tumour thicknes, also known as Breslow thickness, measured in mm.",
  "</td></tr></tfoot>", "</table>", "", "<div id = \"main-results\" class = \"section level2\">",
  "<h2 style = \"margin: 10pt 0pt 0pt 0pt;\">Main results</h2>",
  "<table class = 'gmisc_table' style = 'border-collapse: collapse;'  id = 'table_2'>",
  "    <thead>", "    <tr><td colspan = '9' style = 'text-align: left;'>",
  "    ", "Table 2: Adjusted and unadjusted estimates for melanoma specific death.",
  "</td></tr>", "    <tr>", "        <th style = 'border-top: 2px solid grey;'></th>",
  "        <th colspan = '2' style = 'font-weight: 900; border-top: 2px solid grey; text-align: center;'>",
  " ", "</th><th style = 'border-top: 2px solid grey;; border-bottom: hidden;'>",
  " ", "</th>", "        <th colspan = '2' style = 'font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey;'>",
  "Crude", "</th><th style = 'border-top: 2px solid grey;; border-bottom: hidden;'>",
  " ", "</th>", "        <th colspan = '2' style = 'font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey;'>",
  "Adjusted", "</th>", "    </tr>", "    <tr>", "        <th style = 'font-weight: 900; border-bottom: 1px solid grey; '>",
  "Variable", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "Total", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "Event", "</th>", "        <th style = 'border-bottom: 1px solid grey;' colspan = '1'>",
  " ", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "HR", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "2.5 % to 97.5 %", "</th>", "        <th style = 'border-bottom: 1px solid grey;' colspan = '1'>",
  " ", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "HR", "</th>", "        <th style = 'border-bottom: 1px solid grey; text-align: center;'>",
  "2.5 % to 97.5 %", "</th>", "    </tr>", "    </thead><tbody>",
  "    <tr style = 'background-color:#ffffff;'><td colspan = '9' style = ''>",
  "Sex", "</td></tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Male", "</td>",
  "        <td style = 'text-align: right;'>", "126", "</td>", "        <td style = 'text-align: right;'>",
  "28 (22%)", "</td>", "        <td style = '' colspan = '1'>", " ",
  "</td>", "        <td style = 'text-align: right;'>", "1", "</td>",
  "        <td style = 'text-align: center;'>", "ref", "</td>", "        <td style = '' colspan = '1'>",
  " ", "</td>", "        <td style = 'text-align: right;'>", "1",
  "</td>", "        <td style = 'text-align: center;'>", "ref", "</td>",
  "    </tr>", "    <tr style = 'background-color:#ffffff;'>", "        <td style = 'text-align: left;'>",
  "  Female", "</td>", "        <td style = 'text-align: right;'>",
  "79", "</td>", "        <td style = 'text-align: right;'>", "29 (37%)",
  "</td>", "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "1.94", "</td>", "        <td style = 'text-align: center;'>",
  "1.15 to 3.26", "</td>", "        <td style = '' colspan = '1'>",
  " ", "</td>", "        <td style = 'text-align: right;'>", "1.54",
  "</td>", "        <td style = 'text-align: center;'>", "0.91 to 2.60",
  "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "Age", "</td>", "        <td style = 'text-align: right;'>",
  "52 (± 17)", "</td>", "        <td style = 'text-align: right;'>",
  "55 (± 18)", "</td>", "        <td style = '' colspan = '1'>", " ",
  "</td>", "        <td style = 'text-align: right;'>", "1.02", "</td>",
  "        <td style = 'text-align: center;'>", "1.00 to 1.04", "</td>",
  "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "1.01", "</td>", "        <td style = 'text-align: center;'>",
  "1.00 to 1.03", "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'><td colspan = '9' style = ' '>",
  "Ulceration", "</td></tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'text-align: left;'>", "  Absent", "</td>",
  "        <td style = 'text-align: right;'>", "115", "</td>", "        <td style = 'text-align: right;'>",
  "16 (14%)", "</td>", "        <td style = '' colspan = '1'>", " ",
  "</td>", "        <td style = 'text-align: right;'>", "1", "</td>",
  "        <td style = 'text-align: center;'>", "ref", "</td>", "        <td style = '' colspan = '1'>",
  " ", "</td>", "        <td style = 'text-align: right;'>", "1",
  "</td>", "        <td style = 'text-align: center;'>", "ref", "</td>",
  "    </tr>", "    <tr style = 'background-color:#ffffff;'>", "        <td style = 'text-align: left;'>",
  "  Present", "</td>", "        <td style = 'text-align: right;'>",
  "90", "</td>", "        <td style = 'text-align: right;'>", "41 (46%)",
  "</td>", "        <td style = '' colspan = '1'>", " ", "</td>", "        <td style = 'text-align: right;'>",
  "4.36", "</td>", "        <td style = 'text-align: center;'>",
  "2.44 to 7.77", "</td>", "        <td style = '' colspan = '1'>",
  " ", "</td>", "        <td style = 'text-align: right;'>", "3.20",
  "</td>", "        <td style = 'text-align: center;'>", "1.75 to 5.88",
  "</td>", "    </tr>", "    <tr style = 'background-color:#ffffff;'>",
  "        <td style = 'border-bottom: 2px solid grey; text-align: left;'>",
  "Breslow thickness", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "3 (± 3)", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "4 (± 4)", "</td>", "        <td style = 'border-bottom: 2px solid grey;' colspan = '1'>",
  " ", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "1.17", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: center;'>",
  "1.10 to 1.25", "</td>", "        <td style = 'border-bottom: 2px solid grey;' colspan = '1'>",
  " ", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: right;'>",
  "1.12", "</td>", "        <td style = 'border-bottom: 2px solid grey; text-align: center;'>",
  "1.04 to 1.20", "</td>", "    </tr>", "    </tbody>", "</table>",
  "", "<p>After adjusting for the three variables, age, sex, tumor thickness and ulceration, only the latter two remained significant (p-value &lt; 0.001 and 0.004), see table 1 and figure I.</p>",
  "<figure>", "<img src = 'FTP_test_files/figure-html/Regression_forestplot.png' style = 'display: block'>",
  "<figcaption>", "Figure I: A forest plot comparing the regression coefficients.",
  "</figcaption></figure>", "", "<p>There was no strong indication for non-linearity for any of the continuous variables although the impact of thickness did seem to lessen above 4 mm, see figure II.</p>",
  "<figure>", "<img src = 'FTP_test_files/figure-html/spline_plot.png' width = '384'height = ''width.px = '384'height.px = '384' style = 'display: block'>",
  "<figcaption>", "Figure II: The adjusted and unadjusted restricted cubic spline for tumor thickness. Solid line and confidence interval indicate the adjusted line while the dashed is the unadjusted line. The grey area at the bottom indicates the density.",
  "</figcaption></figure>",
  "", "", "</div>", "</div>", "", "",
  "</div>", "", "<script>", "", "// add bootstrap table styles to pandoc tables",
  "$(document).ready(function () {", "  $('tr.header').parent('thead').parent('table').addClass('table table-condensed');",
  "});", "", "</script>", "", "<!-- dynamically load mathjax for compatibility with --self-contained -->",
  "<script>", "  (function () {", "    var script = document.createElement(\"script\");",
  "    script.type = \"text/javascript\";", "    script.src  = \"https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config = TeX-AMS-MML_HTMLorMML\";",
  "    document.getElementsByTagName(\"head\")[0].appendChild(script);",
  "  })();", "</script>", "", "</body>", "</html>"
)

test_that("Proper script removal", {
  expect_false(all(grepl("--s", prFtpOtherRemoval(output_str))))
  expect_false(all(grepl("Content-Style-Type", prFtpOtherRemoval(output_str))))
  expect_false(all(grepl("<script", prFtpScriptRemoval(output_str))))
})