
            require('ggplot2')
            set.seed(8525)
            figtype = 'col'

            plotdata = read.table(	paste0('new/T', '.txt'),
                                    header = TRUE, row.names = NULL, check.names = FALSE, sep = "	")
            cnames = make.unique(colnames(plotdata))
            colnames(plotdata) = cnames

            bQuote = function(s) paste0('`', s, '`')

            png(paste0('new/T', '.', figtype, '.png'),
                height = 2000, width = 2000, res = 300)
            if (length(cnames) > 2) {
                aes_for_geom = aes_string(fill = bQuote(cnames[3]))
                aes_for_geom_color = aes_string(color = bQuote(cnames[3]))
                plotdata[,3] = factor(plotdata[,3], levels = rev(unique(as.character(plotdata[,3]))))
            } else {
                aes_for_geom = NULL
                aes_for_geom_color = NULL
            }
            p = ggplot(plotdata, aes_string(y = bQuote(cnames[1]), x = bQuote(cnames[2])))
            xticks = theme(axis.text.x = element_text(angle = 60, hjust = 1))
            if (figtype == 'scatter') {
                p = p + geom_point(aes_for_geom_color)
            # } else if (figtype == 'line') {
            # 	p = p + geom_line(aes_for_geom)
            } else if (figtype == 'bar') {
                p = ggplot(plotdata, aes_string(x = bQuote(cnames[2])))
                p = p + geom_bar(aes_string(fill = bQuote(cnames[1]))) + xticks
            } else if (figtype == 'col') {
                p = p + geom_col(aes_for_geom) + xticks
            } else if (figtype == 'pie') {
                library(ggrepel)
                if (length(cnames) > 2) {
                    p = p + geom_col(aes_for_geom) + coord_polar("y", start=0) +
                        geom_label_repel(
                            aes_for_geom,
                            y = cumsum(plotdata[,1]) - plotdata[,1]/2,
                            label = paste0(unlist(round(plotdata[,1]/sum(plotdata[,1])*100,1)), '%'),
                            show.legend = FALSE)
                } else {
                    plotdata[,1] = factor(plotdata[,1], levels = rev(unique(as.character(plotdata[,1]))))
                    fills = rev(levels(plotdata[,1]))
                    sums  = sapply(fills, function(f) sum(plotdata[,1] == f))
                    p = ggplot(plotdata, aes_string(x = bQuote(cnames[2]))) +
                        geom_bar(aes_string(fill = bQuote(cnames[1]))) + coord_polar("y", start=0) +
                        geom_label_repel(
                            inherit.aes = FALSE,
                            data = data.frame(sums, fills),
                            x = 1,
                            y = cumsum(sums) - sums/2,
                            label = paste0(unlist(round(sums/sum(sums)*100,1)), '%'),
                            show.legend = FALSE)
                }
                p = p + theme_minimal() + theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.y =element_blank())
            } else if (figtype == 'violin') {
                p = p + geom_violin(aes_for_geom) + xticks
            } else if (figtype == 'boxplot') {
                p = p + geom_boxplot(aes_for_geom) + xticks
            } else if (figtype == 'histogram' || figtype == 'density') {
                plotdata[,2] = as.factor(plotdata[,2])
                p = ggplot(plotdata, aes_string(x = bQuote(cnames[1])))
                params = list(alpha = .6)
                if (cnames[2] != '1') {
                    params$mapping = aes_string(fill = bQuote(cnames[2]))
                }
                p = p + do.call(paste0("geom_", figtype), params)
            } else if (figtype == 'freqpoly') {
                plotdata[,2] = as.factor(plotdata[,2])
                p = ggplot(plotdata, aes_string(x = bQuote(cnames[1])))
                if (cnames[2] != '1') {
                    params$mapping = aes_string(color = bQuote(cnames[2]))
                }
                p = p + do.call(paste0("geom_", figtype), params)
            } else {
                stop(paste('Unknown plot type:', figtype))
            }
            p = p + scale_x_discrete(name ="Chromosome", \
limits=c("1","2","3","4","5","6","7","8","9","10","X")) + \
ylab("# Variants")ls
            print(p)
            dev.off()
        