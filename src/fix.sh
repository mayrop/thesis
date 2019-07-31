mogrify -trim figures/*

echo "Fixing corrplot margins"
convert "figures/corrplot.png" -bordercolor white -border 100 -bordercolor white -border 1 "figures/corrplot.png"
convert "figures/corrplot.png" -bordercolor black -border 2 -bordercolor white -border 1 "figures/corrplot.png"
