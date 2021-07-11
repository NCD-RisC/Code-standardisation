library(RColorBrewer)
library(scales)

# Text
legend_names <- c("htn140" = "Hypertension", "diag140" = "Diagnosis", "treat140" = "Treatment", "ctrl140" = "Control",
                  "td140"  = "Treated among diagnosed", "ct140" = "Controlled among treated")

# format legend text for change
label_percentage_point <- function(x, accuracy = 1) {
    addon <- ifelse(x>0, "+", ifelse(x==0, "  ", ""))
    return(paste0(addon, round(x*100, digits = log10(accuracy))))
}

# fill scales
get_fill_scale <- function(var, minqt, maxqt, type) {
    if (type == "levels") return(get_fill_scale_level(var, minqt, maxqt))
    if (type == "changes") return(get_fill_scale_change(var, minqt, maxqt))
    return(NA)
}

get_fill_scale_level <- function(var, minqt, maxqt) {
    myPalette_htn <- colorRampPalette(c(colorRampPalette(c("#240a00", "#4a1400", "firebrick4", colorRampPalette(c("orangered3", "#fff5cf"))(3)))(6), "#fffae8"))
    myPalette_oth  <- colorRampPalette(c(colorRampPalette(c("#040029", colorRampPalette(c("slateblue4", "royalblue3", "dodgerblue2", "skyblue2", "#FEF7DB"))(6)))(10), "#fffae8"))
    
    if (grepl("htn", var)) {
        x1  <- 1-rev(rescale(4^seq(.1,.9,length=12)))[-1]
        x2  <- rescale(5^seq(.1,.9,length=12))[-1]
        x3  <- 1-rev(rescale(10^seq(.1,.9,length=10)))[-1]
        vls <- seq(0,1,by=0.1) #rescale(c(x1*1, 1+x2, 2+x3))
        pt_col <- rev(myPalette_htn(24))
        intv <- 0.05
    } else if (grepl("treat|diag", var)) {
        x1  <- 1- rev(rescale(5^seq(.1,.9,length=20)))
        x2  <- rescale(10^seq(.1,.9,length=20))[-1]
        x3  <- 1- rev(rescale(6^seq(.1,.9,length=6)))[-1]
        vls <- rescale(c(x1*1, 1+x2, 2+x3*0.3))
        pt_col <- rev(myPalette_oth(24))
        intv <- 0.05
    } else {
        x1  <- 1- rev(rescale(5^seq(.1,.9,length=10)))
        x2  <- rescale(6^seq(.1,.9,length=10))[-1]
        x3  <- 1- rev(rescale(3^seq(.1,.9,length=10)))[-1]
        vls <- rescale(c(x1*1, 1+x2, 2+x3))
        pt_col <- rev(myPalette_oth(24))
        intv <- 0.05
    }
    max_brks <- floor(maxqt/intv)*intv
    min_brks <- ceiling(minqt/intv)*intv
    brks <- seq(min_brks, max_brks, by = intv)
    brks <- c(minqt, brks[seq(1,length(brks), by = 2)], maxqt)
    if (brks[2] - brks[1] < intv) brks <- brks[-2]
    if (brks[2] - brks[1] >= 3*intv) brks <- c(brks[1], (brks[1] + brks[2])/2, brks[2:length(brks)])
    if (brks[length(brks)] - brks[length(brks)-1] < intv) brks <- brks[-(length(brks)-1)]
    if (brks[length(brks)] - brks[length(brks)-1] >= 2.5*intv) brks <- c(brks[1:(length(brks)-1)], (brks[length(brks)-1]+ brks[length(brks)])/2, brks[length(brks)])
    
    if (var == "untx160140") brks <- c(minqt, 0.1, 0.15, 0.2, 0.25, 0.3, maxqt)
    if (var == "undx160140") brks <- c(minqt, 0.1, 0.15, 0.2, 0.25, maxqt)
    
    scale_fill_gradientn(
        name    = legend_names[var],
        breaks  = brks,
        limits  = c(minqt, maxqt),
        labels  = percent(brks, accuracy = 1), 
        colours = pt_col,
        values  = vls,
        na.value = "gray")
}

get_fill_scale_change <- function(var, minqt, maxqt) {
    change_base_palette1 <- c(colorRampPalette(c("#002e12", colorRampPalette(c("seagreen", "greenyellow", "lightcyan", "#fffdf5"))(6)))(8), rev(colorRampPalette(c("#54000b", colorRampPalette(c("red4", "orangered2", "#ffefe8", "#fffdf5"))(6)))(8))[-1])
    myPalette_change_htn <- colorRampPalette(change_base_palette1)
    
    change_base_palette2 <- c(colorRampPalette(c("#002e12", colorRampPalette(c("forestgreen", "seagreen", "mediumseagreen", "greenyellow", "lightcyan", "#fffdf5"))(6)))(8), rev(colorRampPalette(c("#54000b", colorRampPalette(c("red4", "red3", "tomato4", "orangered2", "#ffefe8", "#fffdf5"))(6)))(8))[-1])
    myPalette_change_oth <- colorRampPalette(change_base_palette2)
    
    n <- 2001
    if (grepl("htn", var)) {
        a = 0.45
        x1  <- 1-rev(rescale(8^seq(.1,.9,length=100*a)))
        x2  <- rescale(8^seq(.1,.9,length=100))[-1]
        vls0 <- c(x1*a, a+x2)
        vls0 <- c(vls0, 1+a + 1+a-rev(vls0)[-1])
        pt_col <- myPalette_change_htn(n)
        intv <- 0.05
    } else if (grepl("treat", var)) {
        a = 0.45
        x1  <- 1-rev(rescale(8^seq(.1,.9,length=100*a)))
        x2  <- rescale(8^seq(.1,.9,length=100))[-1]
        vls0 <- c(x1*a, a+x2)
        vls0 <- c(vls0, 1+a + 1+a-rev(vls0)[-1])
        pt_col <- rev(myPalette_change_oth(n))
        intv <- 0.1
    } else if (grepl("diag", var)) {
        a = 0.2
        x1  <- 1-rev(rescale(8^seq(.1,.9,length=100*a)))
        x2  <- rescale(18^seq(.1,.9,length=100*(1-a)))[-1]
        vls0 <- c(x1*a, a+x2*(1-a))
        vls0 <- c(vls0, 1 + 1-rev(vls0)[-1])
        pt_col <- rev(myPalette_change_oth(n))
        intv <- 0.1
    } else {
        a = 0.6
        x1  <- 1-rev(rescale(20^seq(.1,.9,length=100*a*0.8)))
        x2  <- rescale(4^seq(.1,.9,length=100))[-1]
        vls0 <- c(x1*a, a+x2)
        vls0 <- c(vls0, 1+a + 1+a-rev(vls0)[-1])
        pt_col <- rev(myPalette_change_oth(n))
        intv <- 0.1
    }
    
    if (maxqt > abs(minqt)) {
        highlim <- (n-1)/2
        lowlim  <- ceiling((n-1)/2*(abs(minqt)/maxqt))
    } else {
        lowlim <- (n-1)/2
        highlim  <- ceiling((n-1)/2*(maxqt/abs(minqt)))
    }
    
    vls     <- approx(vls0, n=n)$y
    
    if (minqt <0) {
        upperpart <- seq((n-1)/2, (n-1)/2 + highlim)
        lowerpart <- seq((n-1)/2 - lowlim, (n-1)/2)
        pt_col  <- pt_col[c(lowerpart, upperpart[-1])]
        
        vls1    <- rescale(vls[lowerpart]) * (lowlim / (highlim + lowlim - 1))
        vls2    <- rescale(vls[upperpart]) * (highlim / (highlim + lowlim - 1)) + (lowlim / (highlim + lowlim - 1))
        vls     <- rescale(c(vls1, vls2[-1]))
    } else {
        lowlim  <- ceiling((n-1)/2*(0.01/maxqt))   # use a slightly off white colour (minqt = 0.01) for the low limit if the low limit is larger than zero (in the case of diagnosis)
        pt_col  <- pt_col[seq((n-1)/2 + lowlim, (n-1)/2 + highlim)]
        vls     <- rescale(vls[seq((n-1)/2 + lowlim, (n-1)/2 + highlim)])
    }
    
    max_brks <- floor(maxqt/intv)*intv
    min_brks <- ceiling(minqt/intv)*intv
    brks <- seq(min_brks, max_brks, by = intv)
    brks <- c(minqt, brks, maxqt)
    if (brks[2] - brks[1] < intv & brks[2] == 0) brks <- brks[-1]
    if (brks[2] - brks[1] < 0.5*intv) 
        if (brks[2] != 0) brks <- brks[-2] else brks <- brks[-1]
    if (brks[2] - brks[1] >= 1.5*intv & brks[1] != 0) brks <- c(brks[1], (brks[1] + brks[2])/2, brks[2:length(brks)])
    if (brks[length(brks)] - brks[length(brks)-1] < 0.5*intv)
        if (brks[length(brks)-1] !=0 ) brks <- brks[-(length(brks)-1)] else brks <- brks[-length(brks)]
    if (brks[length(brks)] - brks[length(brks)-1] >= 1.5*intv & brks[length(brks)] != 0) brks <- c(brks[1:(length(brks)-1)], (brks[length(brks)-1]+ brks[length(brks)])/2, brks[length(brks)])
    
    scale_fill_gradientn(
        name    = "Percentage point change",
        breaks  = brks,
        limits  = c(minqt, maxqt),
        labels  = label_percentage_point(brks),
        colours = pt_col,
        values  = vls,
        na.value = "gray")
}