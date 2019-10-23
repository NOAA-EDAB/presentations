# Try #1 at interactive chord diagram for summer flounder conceptual model
# S. Gaichas 25, July 2019
# based on original codel by G. DePiper, circulargraphcode_Summer_Flounder_v2.R

interactive_chord_sfconsmod <- function(edges){
  
  ## Examine unweighted adjacency matrix
  ## Examine unweighted adjacency matrix
  FLUKE <- adjacency.matrix(edges, labels=TRUE)
  
  FLUKE_Drivers <- c("Economic Drivers","Temperature","Shifts in Preferences","Community Vulnerability","Freshwater Influx","Nutrient Influx","Ocean Acidification",
                     "Ocean Features","Oceanographic Transport","Dissolved Oxygen","Water Diversion")
  FLUKE_Habitat <- c("Estuarine Habitat","Food Web Changes","Offshore Habitat","Habitat Alteration","Habitat Disturbance","Loose Inert Substrate","Salinity","Water Clarity",
                     "Aquatic Vegetation")
  FLUKE_Biota <- c("Fluke Distributional Shift","Fluke Recruitment","Fluke SSB","Adults & Spawners","Age & Size Structure",
                   "Growth","Maturation","Natural Mortality","Sex Ratio")
  FLUKE_Species <- c("Other Species Distributional Shifts","Protected Species")
  FLUKE_Management <- c("Allocation","Communication","Enforcement","Management Control","Other Regulations",
                        "Permit Access","Regulatory Complexity","Risk Buffering")
  FLUKE_Benefits <- c("Commercial Profits","Consumer Surplus","Recreational Value","Seafood","Recreational Profits")
  FLUKE_Science <- c("Assessment Process","Data Quality","Predictability of Recreational Fishing","Stock Assessment")
  FLUKE_Fishery <- c("Compliance","Discards","Fishery Distributional Shift","Fishery Resilience",
                     "Fleet Diversity","Landings","Perceived Inequity","Technical Interactions")
  
  FLUKE_C <- brewer.pal(8,"Dark2")
  
  FLUKE_Colors <- data.frame(row.names(FLUKE))
  colnames(FLUKE_Colors) <- "Focus"
  
  FLUKE_Colors$Color <- FLUKE_C[1]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Habitat] <- FLUKE_C[2]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Biota] <- FLUKE_C[3]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Species] <- FLUKE_C[4]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Management] <- FLUKE_C[5]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Benefits] <- FLUKE_C[6]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Science] <- FLUKE_C[7]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Fishery] <- FLUKE_C[8]
  
  FLUKE_Groups <- FLUKE_Colors
  FLUKE_Groups$Group <- "Drivers"
  FLUKE_Groups$Rank <- 1
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Habitat] <- "Habitat"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Habitat] <- 2
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Biota] <- "Fluke Dynamics"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Biota] <- 3
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Species] <- "Other Biota"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Species] <- 4
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Management] <- "Management"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Management] <- 5
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Benefits] <- "Benefits"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Benefits] <- 6
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Science] <- "Science"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Science] <- 7
  FLUKE_Groups$Group[FLUKE_Groups$Focus%in%FLUKE_Fishery] <- "Fishing Fleets"
  FLUKE_Groups$Rank[FLUKE_Groups$Focus%in%FLUKE_Fishery] <- 8
  
  FLUKE_edges <- cbind(FLUKE,FLUKE_Colors)
  FLUKE_Colors <- FLUKE_Colors[order(FLUKE_Colors$Color,FLUKE_Colors$Focus),]
  FLUKE_Colors <- matrix(FLUKE_Colors$Color,dimnames=list(FLUKE_Colors$Focus,"Color"))
  FLUKE_edges <-  FLUKE_edges[order( FLUKE_edges$Color,FLUKE_edges$Focus),]
  
  FLUKE_edges$Color <- NULL
  FLUKE_edges$Focus <- NULL
  
  
  FLUKE_edges <- data.matrix(FLUKE_edges)
  Border_mat <- matrix(1,nrow=nrow(FLUKE_edges),ncol=ncol(FLUKE_edges))
  rownames(Border_mat) <- rownames(FLUKE_edges)
  colnames(Border_mat) <- colnames(FLUKE_edges)
  #Border_mat[Grand_Banks_edges < 0] = 2
  Border_Col <- matrix("white",nrow=nrow(FLUKE_edges),ncol=ncol(FLUKE_edges))
  rownames(Border_Col) <- rownames(FLUKE_edges)
  colnames(Border_Col) <- colnames(FLUKE_edges)
  #Border_Col[Grand_Banks_edges < 0] = "black"
  
  Border_w <- matrix(.0001,nrow=nrow(FLUKE_edges),ncol=ncol(FLUKE_edges))
  rownames(Border_w) <- rownames(FLUKE_edges)
  colnames(Border_w) <- colnames(FLUKE_edges)
  
  #need to redo the order of the columns and rows for this function
  #also transpose it and no negative values
  FLUKE_Colors <- data.frame(row.names(FLUKE))
  colnames(FLUKE_Colors) <- "Focus"
  
  FLUKE_Colors$Color <- FLUKE_C[1]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Habitat] <- FLUKE_C[2]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Biota] <- FLUKE_C[3]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Species] <- FLUKE_C[4]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Management] <- FLUKE_C[5]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Benefits] <- FLUKE_C[6]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Science] <- FLUKE_C[7]
  FLUKE_Colors$Color[FLUKE_Colors$Focus%in%FLUKE_Fishery] <- FLUKE_C[8]
  
  FLUKE_edges <- cbind(FLUKE,FLUKE_Colors)
  FLUKE_Colors <- FLUKE_Colors[order(FLUKE_Colors$Color,FLUKE_Colors$Focus),]
  FLUKE_Colors <- matrix(FLUKE_Colors$Color,dimnames=list(FLUKE_Colors$Focus,"Color"))
  FLUKE_edges <-  FLUKE_edges[order( FLUKE_edges$Color,FLUKE_edges$Focus),]
  
  FLUKE_edges$Color <- NULL
  FLUKE_edges$Focus <- NULL
  
  rn <- row.names(FLUKE_edges)
  FLUKE_edges2 <- FLUKE_edges[rn]
  
  FLUKE_edges <- data.matrix(FLUKE_edges)
  FLUKE_edges2 <- data.matrix(FLUKE_edges2)
  FLUKE_edges2 <- t(FLUKE_edges2)
  FLUKE_edges2 <- abs(FLUKE_edges2)
  
  #########Submodels functions
  Submodel_edges <- function (y) {
    New_edges <- FLUKE_edges2[,colnames(FLUKE_edges2)%in%y]
    New_edges <- New_edges[rownames(New_edges)%in%y,]
    return(New_edges)
  }
  
  Submodel_color <- function (y) {
    New_colors <-  as.vector(FLUKE_Colors[rownames(FLUKE_Colors)%in%y,])
    return(New_colors)
  }
  
  New_Figure <- function (x) {
    
    New_edges <- Submodel_edges(x)
    New_colors <- Submodel_color(x)
    New_groups <- FLUKE_Groups[which(FLUKE_Groups$Focus%in%x),]
    New_groups <- unique(New_groups[,2:4])
    New_groups <- New_groups[order(New_groups$Rank),]
    New_groups <- New_groups[,1:2]
    Figure_name <- deparse(substitute(x))
    Figure_title <- gsub("_"," ",Figure_name)
    
    chorddiag(New_edges, 
              type = "directional",
              width = 900,
              height = 900,
              margin = 120,
              groupColors = New_colors,
              groupedgeColor = New_colors,
              chordedgeColor = New_colors,
              groupPadding = 1, groupThickness = 0.1,
              showTicks = F, groupnameFontsize = 12, groupnamePadding = 10
    )
  }  
  
  New_Figure(colnames(FLUKE_edges2))
  
}
