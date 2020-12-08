#!/usr/bin/env perl
#$kanji            = defined $ENV{"LATEXENC"} ? "-kanji=$ENV{\"LATEXENC\"}" : "-kanjii=utf8";
#$latex            = "platex -synctex=1 -halt-on-error -file-line-error";
#$latex_silent     = "platex -synctex=1 -halt-on-error -interaction=batchmode";
#$bibtex           = "pbibtex";
#$biber = 'biber --bblencoding=utf8 -u -U --output_safechars';
#$makeindex        = "touch -m %D";
#$makeindex        = "mendex %O -o %D %S";
#$dvipdf           = "dvipdfmx %O -o %D %S";
#$dvips = 'dvips %O -z -f %S | convbkmk -u > %D';
#$ps2pdf = 'ps2pdfwr %O %S %D';
# @default_files = ('file.tex');  # determine the target file.
$kanji  = "-kanji=$ENV{\"LATEXENC\"}" if defined $ENV{"LATEXENC"};
$latex  = "platex -synctex=1 -file-line-error -halt-on-error -interaction=nonstopmode $kanji";
$bibtex = 'pbibtex $kanji';
$biber = 'biber --bblencoding=utf8 -u -U --output_safechars';
$makeindex        = "mendex %O -o %D %S";
$dvipdf = 'perl -e "exec(\'dvipdfmx\', \$ARGV[0])"';
$max_repeat = 5;
$pdf_mode= 3; # generates pdf via dvipdfmx
# $pdf_mode= 1; # using pdflatex

# Prevent latexmk from removing PDF after typeset.
# This enables Skim to chase the update in PDF automatically.
$pvc_view_file_via_temporary = 0;

if ($^O eq "linux"){
    #Use Okular in Linux OS
    $pdf_previewer = "okular";
}
else {
    # Use Skim as a previewer in MacOS
    $pdf_previewer    = "open -ga /Applications/Skim.app";
}

$clean_ext = "snm nav vrb synctex.gz";
