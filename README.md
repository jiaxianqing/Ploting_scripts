# Ploting_scripts
R scripts for image ploting


* `draw.blocks.R`

```
# 
perl block2draw.pl -t 5 -d 5 \
    -s samples.list \
    -b blocks.csv \
    -g gene.bed \
    -c centromere.bed \
    -p test.draw.pos.csv \
        > test.draw.csv

Rscript draw.blocks.R \
    test.draw.csv \
    test.draw.pos.csv \
    N/N,D/N,D/D,Gene,Centromere \
    15,100 \
    test.draw.pdf

```
