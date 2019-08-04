mogrify -trim figures/*

mogrify -trim logo-trimmed.jpg

echo "Fixing corrplot margins"
convert "figures/corrplot.png" -bordercolor white -border 100 -bordercolor white -border 1 "figures/corrplot.png"
convert "figures/corrplot.png" -bordercolor black -border 2 -bordercolor white -border 1 "figures/corrplot.png"

echo "Fixing density plots margins"
convert "figures/density_plots.png" -bordercolor white -border 100 -bordercolor white -border 1 "figures/density_plots.png"
convert "figures/density_plots.png" -bordercolor black -border 2 -bordercolor white -border 1 "figures/density_plots.png"

convert "logo-trimmed.png" -bordercolor white -border 100 -bordercolor white -border 1 "logo-trimmed.png"
