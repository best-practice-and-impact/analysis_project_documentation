from quarto import render

out_filename = "Assumptions_log.html"

render(
    input = "assumptions_log.qmd", 
    output_format = "html",
    output_file = out_filename, 
    )