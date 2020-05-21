library(rsconnect)

setwd("~/Michelle's Shower/Bridal_shower")

rsconnect::setAccountInfo(name='sarahleavitt',
                          token='9BE4A4277D3B565B2B1F029725D5248B',
                          secret='PIq4MKddLUL97RRB7AcKapW8EKY5NuJMsud2pSHN')

rsconnect::deployApp('.')
