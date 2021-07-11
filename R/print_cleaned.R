print_cleaned <- function(var) {
    print(paste("Percentage Cleaned (No. of cleaned/No. of non-NAs):",var,"(%)"))
    if (length(clnList)==0) {
        print("No records cleaned")
    } else {
        cln.table <- table(data[clnList,]$id_study)/table(data[!is.na(data[var]),]$id_study)*100
        print(sort(round(cln.table[which(cln.table!=Inf&cln.table>0)],2), decreasing=TRUE))
    }
}