options(stringsAsFactors = FALSE)
library(png)
library(ggplot2)

generate_star_image = function(model,ratingGiven){
  img = readPNG(paste("images/",ratingGiven,".png",sep = ""))
  file_name = paste(model,ratingGiven,".png",sep="_")
  file_path = paste("output",file_name,sep = "/")
  writePNG(img,file_path)
}

plotComparsion = function(product_name){
  outputData = read.csv("output/results.csv",header = TRUE)
  #Reference - For Plotting this Type of Graph
  #http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
  p = ggplot(data=outputData,aes(x=Model,y=Values,fill=Factors)) +
    geom_bar(stat="identity", position=position_dodge()) +
    xlab("Model Name") +
    ylab("Factors") +
    ggtitle(paste("Prediction For",product_name,"Product\n",sep = " ")) +
    geom_text(aes(label=Values), vjust=1.6, color="black",
              position = position_dodge(0.9), size=6.5) +
    theme(plot.title = element_text(color="red", size=25, face="bold.italic"),
                  plot.margin = margin(2, 2, 2, 2, "cm"),
                  axis.text.x = element_text(size = 15,color = "black"),
                  axis.text.y = element_text(size = 15,color = "black"),
                  axis.title.x = element_text(color="blue", size=14, face="bold",vjust = -8),
                  axis.title.y = element_text(color="#993333", size=14, face="bold"))
  print("Generating Comparsion image by Plotted Graph")
  ggsave('output/comparsion.png', width = 14, height = 9, dpi = 1000)
  print("Comparsion Image Between 3 Models is now available in output folder")
  print("Finish!!")
}