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
$kanji  = "-kanji=$ENV{\"LATEXENC\"}" if defined $ENV{"LATEXENC"};
$latex  = "platex -interaction=nonstopmode $kanji";
$bibtex = 'pbibtex $kanji';
$makeindex        = "mendex %O -o %D %S";
$dvipdf = 'perl -e "exec(\'dvipdfmx\', \$ARGV[0])"';
$max_repeat = 5;
$pdf_mode= 3; # generates pdf via dvipdfmx

# Prevent latexmk from removing PDF after typeset.
# This enables Skim to chase the update in PDF automatically.
$pvc_view_file_via_temporary = 0;

#Use Evince
$pdf_previewer = "evince";
# Use Skim as a previewer
#$pdf_previewer    = "open -ga /Applications/Skim.app";
$clean_ext = "snm nav vrb synctex.gz";