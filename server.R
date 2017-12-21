###### function to calculate chi-squared test for 2 multiple-response variables
source("R/mi.test_with_improvements.R")


###### Reading data
### a column of the data is respondents vote data for a pair <brand label> | <attribute label>
brandData <- fread("data/carRatings.csv")
attribute.labels.grouped <- c(fread("data/attributeLabels.txt"))
attribute.labels <- unlist(attribute.labels.grouped, use.names = FALSE)
attribute.groups.labels <- names(attribute.labels.grouped) 
brand.labels <- readLines("data/brandLabels.txt")


###### Tabulating the input data
tabData <- matrix(colSums(brandData), nrow = length(brand.labels), byrow = T)
dimnames(tabData) <- list(brand.labels, attribute.labels)

###### Data for wordcloud
brandsFreq <- data.frame(word = brand.labels, freq = rowSums(tabData),
                         freq.style = "#d3d3d3", stringsAsFactors = FALSE)
attributesFreq <- data.frame(word = attribute.labels, freq = colSums(tabData),
                             freq.style = "#d3d3d3", stringsAsFactors = FALSE)


###### Data for valueBoxes
ratersNmb <- nrow(brandData)
brandsNmb <- length(brand.labels)
attributesNmb <- length(attribute.labels)


###### Data to heatMap
heatmapData <- lapply(1:ncol(tabData), function(i)  data.table(car = 0:(nrow(tabData)-1), attribute = ncol(tabData) - i, votes = tabData[,i]))
heatmapData <- rbindlist(heatmapData)
heatmapData <-list_parse2(heatmapData)


####### Brand pairs list
brands.pairs <- t(combn(brand.labels, 2))
dimnames(brands.pairs)[[2]] <- c("from", "to")


function(input, output, session) {
  
  ###################### Overview tab
 
  output$listOfBrands <- renderWordcloud2({
    wordcloud2(brandsFreq, size = 0.35, minRotation = 0, maxRotation = 0)
  })
  
  output$listOfAttributes <- renderWordcloud2({
    wordcloud2(attributesFreq, size = 0.35, minRotation = 0, maxRotation = 0)
  })
  
  output$usersCard <- renderValueBox({
    valueBox(value = ratersNmb, 
             "RESPONDENTS", icon = icon("users"))
  })
  output$brandsCard <- renderValueBox({
    valueBox(value = brandsNmb, 
             "CAR MODELS", icon = icon("car"))
  })
  output$attributesCard <- renderValueBox({
    valueBox(value = attributesNmb, 
             "ATTRIBUTES", icon = icon("thumbs-up"))
  })
  

  output$heatmap <- renderHighchart({
    hc <- highchart() %>% 
      hc_chart(type = "heatmap") %>% 
      hc_xAxis(categories = brand.labels) %>% 
      hc_yAxis(categories = rev(attribute.labels)) %>% 
      hc_add_series(name = "votes", data = heatmapData)
    hc_colorAxis(hc, minColor = "#FFFFFF", maxColor = "#109618") %>% 
      hc_tooltip(formatter = JS("function(){
                return this.series.xAxis.categories[this.point.x] + ' is ' +
                       this.series.yAxis.categories[this.point.y] + ': <b>' +
                       this.point.value +'</b> votes';
                                ; }"))
  })
  
  

  ######################### Car ratings differences tab
  dataToCharts <- reactive({
    brandCount <- tabData[input$brandToPolarChart,]
    brandRate <- brandCount/ratersNmb
    if (input$comparedWithBrand == "Sample Average"){
      referenceRate <- colSums(tabData)/(ratersNmb*brandsNmb)
      binom.p.values <- sapply(1:attributesNmb, function(i)
                        binomial.mid.p(brandCount[i], ratersNmb, referenceRate[i])
                        )
    }
    else{
      referenceRate <- tabData[input$comparedWithBrand,]/ratersNmb
      brand1 <- brandData[, paste(input$brandToPolarChart, attribute.labels, sep = " | "),
                          with = FALSE]
      brand2 <- brandData[, paste(input$comparedWithBrand, attribute.labels, sep = " | "),
                          with = FALSE]
      binom.p.values <- apply(brand1 - brand2, 2, function(x){
                                                    n <- sum(abs(x))
                                                    n12 <- (n+sum(x))/2
                                                    binomial.mid.p(n12, n, 0.5)
                                                  }
                        )
      }
    diff <- round((brandRate - referenceRate)*100,1)
    diffAbove <- diffBelow <- rep(0L, attributesNmb)
    diffAbove[diff > 0] <- diff[diff > 0]
    diffBelow[diff < 0] <- -diff[diff < 0]
    
    rateTable <- rbind(data.table(brand = input$brandToPolarChart,
                                  attribute = attribute.labels,
                                  rate = round(brandRate*100, 1)),
                       data.table(brand = input$comparedWithBrand,
                                  attribute = attribute.labels,
                                  rate = round(referenceRate*100, 1)))
    rateTable[, brand := factor(brand, levels = unique(c(input$brandToPolarChart, input$comparedWithBrand)))]
    
    list(table = rateTable,
         above = data.table(x = 0:(attributesNmb-1), y = diffAbove, p.value = binom.p.values),
         below = data.table(x = 0:(attributesNmb-1), y = diffBelow, p.value = binom.p.values))
  })
  
  output$polarChart <- renderHighchart({
    dat <- dataToCharts()
    datAbove <- copy(dat$above)
    datAbove[p.value >= 1 - input$binomConfLevel, y := 0]
    datBelow <- copy(dat$below)
    datBelow[p.value >= 1 - input$binomConfLevel, y := 0]
    hc <- highchart() %>%
      hc_title(text = paste(input$brandToPolarChart, "compared with", input$comparedWithBrand)) %>% 
      hc_chart(polar = TRUE, type = "column") %>% 
      hc_xAxis(categories = attribute.labels) %>%
      hc_yAxis(tickAmount = 3) %>% 
      hc_add_series(datAbove, name = "Above compared", color = "#3366cc") %>% 
      hc_add_series(datBelow, name = "Below compared", color = "#dc3912") %>% 
      hc_plotOptions(column = list(stacking = "normal"))
    if (input$groupingPolarChart){
      hc %>% hc_add_series(data = factor(attribute.groups.labels,
                                         levels = attribute.groups.labels),
                           name = "Group", type = "pie", size = '88%', innerSize = '97%') %>%
        hc_plotOptions(pie = list(colors = brewer.pal(min(length(attribute.labels.grouped), 8), "Set2"),
                                  dataLabels = list(enabled = FALSE))
        )
    }
    else{
      hc
    }
  })
  
  output$barChart <- renderHighchart({
    dat <- dataToCharts()$table
    hchart(dat, "bar", hcaes(x = attribute, y = rate, group = brand),
              color = c("#109618", "#FF9900")) %>% 
      hc_title(text = paste(input$brandToPolarChart, "and", input$comparedWithBrand, "ratings")) %>% 
      hc_xAxis(title = NULL) %>% 
      hc_yAxis(title = list(text = "ratings (in percent)", align = "high"),
               gridLineWidth = 0)
  })


  ###################### Dependencies between cars
  
  chisqRS2Data <- reactive({
      validate(need(length(input$attributesToChiSqRS2Network)>0, "Please select at least one attribute"))
      selectedColumns <- c(t(outer(brand.labels, input$attributesToChiSqRS2Network, paste, sep = " | ")))
      dat <- brandData[, ..selectedColumns]
      I <- J <- length(input$attributesToChiSqRS2Network)
      chi.sq.rs2 <- apply(brands.pairs, 1, function(x) {
        selectedColumns <- c(paste(x[1], input$attributesToChiSqRS2Network, sep = " | "),
                             paste(x[2], input$attributesToChiSqRS2Network, sep = " | "))
        multiple.chiRS2.test(dat[, ..selectedColumns], I, J, lodds.calc = TRUE)
      })
      chi.sq.rs2.p.values <- sapply(chi.sq.rs2, function(x) 
                                                  if(is.null(x$p.value.rs2)){
                                                    1
                                                  }
                                                  else{
                                                    if (x$p.value.rs2 == 0)
                                                      .Machine$double.xmin
                                                    else
                                                      x$p.value.rs2
                                                  })
      adj.matrix <- matrix(0, length(brand.labels), length(brand.labels),
                           dimnames = list(brand.labels, brand.labels))
      adj.matrix[lower.tri(adj.matrix)] <- chi.sq.rs2.p.values
      chiSqS <- rbindlist(lapply(chi.sq.rs2, function(x) x$X.sq.S.ij))
      setkey(chiSqS, Brand1, Brand2)

      list(chiSqRS2pvalues = adj.matrix, chiSqS = chiSqS)
  })
  
  plotsData <- reactive({
     dat <- chisqRS2Data()
     adj.matrix <- dat$chiSqRS2pvalues
     adj.matrix[adj.matrix > 1 - input$chiSqRS2ConfLevel] <- 0
     brands.graph <- graph_from_adjacency_matrix(adj.matrix, mode = "undirected", weighted = T)
     if (input$minSpanForest){
       mst.brands.graph <- mst(brands.graph)
       graph <- toVisNetworkData(mst.brands.graph)
       nodes.values <- degree(mst.brands.graph) 
     }
     else{
       graph <- toVisNetworkData(brands.graph)
       nodes.values <- degree(brands.graph)
     }
     graph$nodes <- as.data.table(graph$nodes)
     graph$nodes[, c("value", "hidden") := 
                       list(nodes.values+2, FALSE)]
     graph$nodes <- rbind(graph$nodes,
                              data.table(id = ' ', label = ' ',
                                         value = 1,
                                         hidden = TRUE))
     graph$edges <- as.data.table(graph$edges)
     graph$edges[, width := 1L]
     list(graph = graph, chiSqS= dat$chiSqS)
   })

  output$chiSqRS2Network <- renderVisNetwork({
    vis.graph <- plotsData()$graph
    visNetwork(nodes = vis.graph$nodes, edges = vis.graph$edges,
               height = "100%", width = '100%',
               main = HTML(paste("Car models dependencies<br><font size='3'>attributes set:",
                                 paste(input$attributesToChiSqRS2Network, collapse = ", "),
                                 "</font>"))) %>% 
      visInteraction(hideEdgesOnDrag = TRUE) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 0, hideColor = "#0099C6"),
                 nodesIdSelection = list(enabled = TRUE,
                                         style = 'width: 0px; height: 0px; border:none;')) %>%
      visNodes(shadow = TRUE, color = list(background = '#0099C6', highlight = '#FF9900')) %>% 
      visEdges(color = '#0099C6') %>% 
      visLayout(randomSeed = 123)
  })
  
  dataToTreeMap <- reactive({
    validate(need(nchar(input$chiSqRS2Network_selected) > 0, "Please select a node of the network to display the plot"))
    dat <- plotsData()
    brand <- input$chiSqRS2Network_selected
    To <- dat$graph$edges[from == brand, to]
    From <- dat$graph$edges[to == brand, from]
    validate(need(length(To) + length(From) > 0, "The selected node is isolated"))
    if (length(From) > 0){
      chiSqS <- dat$chiSqS[.(From, brand)]
      setcolorder(chiSqS, c("Brand2", "Attribute2", "Brand1", "Attribute1",
                            "statistics", "lodds.ratio"))
      setnames(chiSqS, 1:4, c("Brand1", "Attribute1", "Brand2", "Attribute2"))
      if(length(To) > 0){
        chiSqS <- rbind(dat$chiSqS[.(brand, To)], chiSqS)
      }
    }
    else{
      chiSqS <- dat$chiSqS[.(brand, To)]
    }
    
    Level3 <- chiSqS[, list(nodeID = paste(paste(Brand1, Attribute1, sep = " | "), paste(Brand2, Attribute2, sep = " | "), sep = " - "),
                            parentID = paste(paste(Brand1, Attribute1, sep = " | "), Brand2, sep = " - "),
                            statistics, lodds.ratio
    )]
    if (input$brandLevel){
      Level2 <- chiSqS[, list(nodeID = paste(paste(Brand1, Attribute1, sep = " | "), Brand2, sep = " - "),
                              parentID = paste(Brand1, Brand2, sep = " - "),
                              statistics = sum(statistics), lodds.ratio = NA),
                       by = c("Brand1", "Attribute1", "Brand2")]
      
      Level1 <- Level2[, list(nodeID = paste(Brand1, Brand2, sep = " - "),
                              parentID = Brand1,
                              statistics = sum(statistics), lodds.ratio = NA),
                       by = c("Brand1", "Brand2")]
      Level2[, c("Brand1", "Attribute1", "Brand2") := NULL]
      Level1[, c("Brand1", "Brand2") := NULL]
    }
    else{
      Level2 <- chiSqS[, list(nodeID = paste(paste(Brand1, Attribute1, sep = " | "), Brand2, sep = " - "),
                              parentID = paste(Brand1, Attribute1, sep = " | "),
                              statistics = sum(statistics), lodds.ratio = NA),
                       by = c("Brand1", "Attribute1", "Brand2")]
      
      Level1 <- Level2[, list(nodeID = paste(Brand1, Attribute1, sep = " | "),
                              parentID = Brand1,
                              statistics = sum(statistics), lodds.ratio = NA),
                       by = c("Brand1", "Attribute1")]
      Level2[, c("Brand1", "Attribute1", "Brand2") := NULL]
      Level1[, c("Brand1", "Attribute1") := NULL]
    }
    root <- data.table(nodeID = brand, parentID = NA,
                       statistics = sum(Level1$statistics), lodds.ratio = NA)
    output <- rbind(root, Level1, Level2, Level3)
    
    # color scale of TreeMap should be symmetric,
    # that is zero lodds.ratio is placed at the scale center and has white color.
    lodds.ratio.range <- range(chiSqS[, lodds.ratio])
    
    if (lodds.ratio.range[1]*lodds.ratio.range[2] < 0){
      if(abs(lodds.ratio.range[1]) < lodds.ratio.range[2]){
        k <- -lodds.ratio.range[2]/lodds.ratio.range[1]
        output[lodds.ratio < 0, lodds.ratio:= k*lodds.ratio]
      }
      else{
        k <- -lodds.ratio.range[1]/lodds.ratio.range[2]
        output[lodds.ratio > 0, lodds.ratio:= k*lodds.ratio]
      }
    }
    else{
      if(lodds.ratio.range[1] > 0){
        lodds.ratio.toSym <- -lodds.ratio.range[2]
      }
      else{
        lodds.ratio.toSym <- -lodds.ratio.range[1]
      }
      output <- rbind(output,
                      data.table(nodeID = "nonexist", parentID = output[.N, parentID],
                                 statistics = 1e-10, lodds.ratio = lodds.ratio.toSym))
    }
    
    output
  })
  
  output$treeMap <- renderGvis({
    gvisTreeMap(dataToTreeMap(), "nodeID", "parentID",
                "statistics", "lodds.ratio",
                options=list(width=550, height=800,
                             fontSize=16,
                             minColor='#DC3912',
                             midColor = 'white',
                             maxColor='#3366CC',
                             maxDepth = 1, maxPostDepth = 2,
                             useWeightedAverageForAggregation = TRUE,
                             showScale = TRUE))
  })
}