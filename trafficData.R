require(data.table)

data <- fread("Traffic_Violations.csv", select = c("Driver State", "Violation Type"))

print(ncol(data))
print(nrow(data))

md <- data[ which(data$'Driver State'=='MD'), ]
oos <- data[ which(data$'Driver State'!='MD'), ]

md_count <- table(unlist(md))
md_count[order(as.numeric(md_count))]

oos_count <- table(unlist(oos))
oos_count[order(as.numeric(oos_count))]

md_tickets <- data[ which(data$'Violation Type'=='Citation' & data$'Driver State'=='MD'), ]
oos_tickets <- data[ which(data$'Violation Type'=='Citation' & data$'Driver State'!='MD'), ]
md_warnings <- data[ which(data$'Violation Type'=='Warning' & data$'Driver State'=='MD'), ]
oos_warnings <- data[ which(data$'Violation Type'=='Warning' & data$'Driver State'!='MD'), ]
md_repair <- data[ which((data$'Violation Type'=='SERO' | data$'Violation Type'=='ESERO') & data$'Driver State'=='MD'), ]
oos_repair <- data[ which((data$'Violation Type'=='SERO' | data$'Violation Type'=='ESERO') & data$'Driver State'!='MD'), ]

md_ticket_occ <- table(unlist(md_tickets))
md_ticket_occ[order(as.numeric(md_ticket_occ))]

oos_ticket_occ <- table(unlist(oos_tickets))
oos_ticket_occ[order(as.numeric(oos_ticket_occ))]

md_warning_occ <- table(unlist(md_warnings))
md_warning_occ[order(as.numeric(md_warning_occ))]

oos_warning_occ <- table(unlist(oos_warnings))
oos_warning_occ[order(as.numeric(oos_warning_occ))]

md_repair_occ <- table(unlist(md_repair))
md_repair_occ[order(as.numeric(md_repair_occ))]

oos_repair_occ <- table(unlist(oos_repair))
oos_repair_occ[order(as.numeric(oos_repair_occ))]
