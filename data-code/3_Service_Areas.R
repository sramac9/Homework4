##############################################################################
## Read in service area data */
##############################################################################


## Read in monthly files, append to yearly file, fill in missing info, and collapse down to yearly file
for (y in 2010:2015) { {
    ## Pull service area data by contract/month
    ma.path=paste0("data/input/monthly-ma-contract-service-area/MA_Cnty_SA_",y,"_01.csv")
    service.area=read_csv(ma.path,skip=1,
                          col_names=c("contractid","org_name","org_type","plan_type","partial","eghp",
                                      "ssa","fips","county","state","notes"),
                          col_types = cols(
                            contractid = col_character(),
                            org_name = col_character(),
                            org_type = col_character(),
                            plan_type = col_character(),
                            partial = col_logical(),
                            eghp = col_character(),
                            ssa = col_double(),
                            fips = col_double(),
                            county = col_character(),
                            notes = col_character()
                          ), na='*')
    service.area = service.area %>%
      mutate(year=y)
  }
  
  ## Fill in missing fips codes (by state and county)
  # Fill missing fips codes (by state and county)
service.area = service.area %>%
  group_by(state, county) %>%
  fill(fips)

  ## Fill in missing plan type, org info, partial status, and eghp status (by contractid)
  service.area = service.area %>%
    group_by(contractid) %>%
    fill(plan_type, partial, eghp, org_type, org_name)
  

  ## Collapse to yearly data
  service.area = service.area %>%
    group_by(contractid, fips) %>%
    mutate(id_count=row_number())
  
  service.area = service.area %>%
    filter(id_count==1) %>%
    select(-c(id_count))

  
  assign(paste("service.area.",y,sep=""),service.area)  
}

contract.service.area=rbind(service.area.2010,
                            service.area.2011,service.area.2012,service.area.2013,service.area.2014,service.area.2015)
write_rds(contract.service.area,"data/output/contract_service_area.rds")
