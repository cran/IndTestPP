\name{TxBHZ}
\alias{TxBHZ}
\docType{data}
\title{Daily maximum temperature at Barcelona, Huesca and Zaragoza}

\description{
Daily maximum temperature series during the summer months (May, June, July, 
August and September) from  1951 to 2016 at three Spanish locations: Barcelona, Huesca and Zaragoza.}

\usage{data(TxBHZ)}


\details{Variables

year: Year, from 1951 to 2016.

month: Month, from 5 (May) to 9 (September).

day: Postion of the day in the month, from 1  up to 31.

dayyear: Postion of the day in the year, from 121 (1st of May) to 253 (30th of September).


TxB: Daily maximum temperature at Barcelona in Celsius degrees.

TxH: Daily maximum temperature at Huesca in Celsius degrees.

TxZ: Daily maximum temperature at Zaragoza in Celsius degrees.

Txm31B: Local maximum temperature signal in Celsius degrees. Moving average of TxB with a  window of the last past 31 days.

Txm31H: Local maximum temperature signal in Celsius degrees. Moving average of TxH with a  window of the last past 31 days.

Txm31Z: Local maximum temperature signal in Celsius degrees. Moving average of TxZ with a  window of the last past 31 days.

lambdaOZ: estimated intensities of the first indicator processes  of a CPSP fitted to model
the occurrence times  of the extreme events in the series of Zaragoza and Huesca, TxZ and TxH. The  first indicator process
includes the extreme events occurring only at TxZ.

lambdaOH: estimated intensities of the second indicator processes  of a CPSP fitted to model
the occurrence times  of the extreme events in the series of Zaragoza and Huesca, TxZ and TxH. The  second indicator process
includes the extreme events occurring only at TxH.

lambdaZH: estimated intensities of the third indicator processes  of a CPSP fitted to model
the occurrence times  of the extreme events in the series of Zaragoza and Huesca, TxZ and TxH. The  third indicator process
includes the simultaneous  extreme events occurring both at TxZ and TxH.

}


\examples{
data(TxBHZ)
}

