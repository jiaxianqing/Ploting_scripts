# Ploting_scripts
R scripts for image ploting

---

* `plot.blocks.R`

```
# Prepare input data
perl block2draw.pl -t 5 -d 5 \
    -s samples.list \ # sorted sample list
    -b blocks.csv \ # block file
    -g gene.bed \ # gene info, optional
    -c centromere.bed \ # gene info, optional
    -p test.draw.pos.csv \ # for ploting
     > test.draw.csv # for ploting

# Plot blocks
Rscript plot.blocks.R \
    test.draw.csv \
    test.draw.pos.csv \
    N/N,D/N,D/D,Gene,Centromere \ # tract name and order
    15,100 \ # width and height
    test.draw.pdf
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/plot.block.png"  div align = "center" width="75%" height="75%" />

---



