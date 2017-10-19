module LatexParser.Export exposing (exportDocument)


prefix : String
prefix =
    """
\\documentclass[11pt, oneside]{article}
\\usepackage{geometry}
\\geometry{letterpaper}

\\usepackage{graphicx}
\\usepackage{wrapfig}
\\graphicspath{ {images/} }

\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
}

%SetFonts

%SetFonts

%%%%%%
\\newcommand{\\code}[1]{{\\tt #1}}
\\newcommand{\\ellie}[1]{\\href{#1}{Link to Ellie}}
% \\newcommand{\\image}[3]{\\includegraphics[width=3cm]{#1}}

\\newcommand{\\imagecenter}[3]{
    \\begin{figure}[h]
    \\includegraphics[width=7cm]{#1}
    \\centering
    \\end{figure}
}

\\newcommand{\\imagefloatright}[3]{
    \\begin{wrapfigure}{r}{0.25\\textwidth}
    \\includegraphics[width=3cm]{#1}
    \\centering
    \\end{wrapfigure}
}

\\newcommand{\\imagefloatleft}[3]{
     \\begin{wrapfigure}{l}{0.25\\textwidth}
    \\includegraphics[width=7cm]{#1}
    \\centering
    \\end{wrapfigure}
}

\\newcommand{\\italic}[1]{{\\sl #1}}
\\newcommand{\\strong}[1]{{\\bf #1}}
\\newcommand{\\subheading}[1]{{\\bf #1}\\par}
\\newcommand{\\xlinkPublic}[2]{\\href{{http://www.knode.io/\\#@public#1}}{#2}}


%%The http://www.knode.io/@public

\\newtheorem{theorem}{Theorem}

\\parindent0pt
\\parskip10pt


"""


prepareText : String -> String
prepareText =
    text
        text


exportDocument : Document -> String
exportDocument document =
    prefix ++ (prepareText document.rendered_text) ++ suffix


suffix =
    """

\\end{document}
"""
