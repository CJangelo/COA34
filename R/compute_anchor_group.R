#' Compute the Anchor Group
#'
#' @param dat pass the dataframe
#' @param anchor.variable numeric variable name that you want to use to create the anchor groups variable of the subject ID in the dataframe numeric; default is USUBJID
#' @return returns a dataframe with additional variable `anchor.group`
#' @export


compute_anchor_group <- function(dat,
                                 anchor.variable = 'PGIS_delta'
                  ){

  if (is.null(dat)) stop('Please specify dataframe in `compute_anchor_group()` ')
  if (is.null(anchor.variable)) stop('Please specify anchor.variable in `compute_anchor_group()` ')



    av <- dat[ , anchor.variable, drop = T]

    if (!is.numeric(av)) stop('Anchor variable is not numeric')

   # if (!number.of.anchor.groups %in% c(3, 5)) stop('Number of anchor groups must be either 3 or 5')



      ag <- ifelse(av >= 2, 'd2',
                                   ifelse(av == 1, 'd1',
                                          ifelse(av == 0, 'mm',
                                                 ifelse(av == -1, 'i1',
                                                        ifelse(av <= -2, 'i2', NA)))))

      agf <- factor(ag, levels = c('i2', 'i1', 'mm', 'd1', 'd2'),
                          #labels = c('Improved_2+', 'Improved_1', 'Maintained', 'Deteriorated_1', 'Deteriorated_2+'))
                        labels = c('Improved, 2 or more categories',
                                   'Improved, 1 category',
                                   'Maintained',
                                   'Deteriorated, 1 category',
                                   'Deteriorated, 2 or more categories'))


    # if (number.of.anchor.groups == 3) {
    #
    #     makeNA <- which(!(agf %in% c('Improved_1', 'Maintained', 'Deteriorated_1')))
    #     agf[makeNA] <- NA
    #     agf <- droplevels(agf)
    #
    # }


    dat$anchor.groups <- agf


        return(dat)

}
