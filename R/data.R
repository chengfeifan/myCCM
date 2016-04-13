#' Process data of a simulating CSTR
#'
#' A dataset containing the material concentration, temperature and time of 99991.
#' \describe{
#'    \item{Ca}{Concentration of A, in mol/L}
#'    \item{Cb}{Concentration of B, in mol/L}
#'    \item{Cc}{Concentration of C, in mol/L}
#'    \item{Ci}{Input Concentration of A, 1 mol/L with white noise having N(0,0.1)}
#'    \item{Tf}{Reaction Temperature, 350K with white noise having N(0,3)}
#'    \item{time}{time, start from 1 second, sample interval is 0.1 second}
#' }
#' @details The model of CSTR is as followed.
#'
#' dCa(t)=F/V*(Ci(t)-Ca(t))-k1*Ca(t)*exp(-E1/(R*Tf))
#'
#' dCb(t)=k1*Ca(t)*exp(-E1/(R*Tf))-k2*Cb(t)*exp(-E2/(R*Tf))-F/V*Cb(t)
#'
#' Cc(t)=k2*Cb(t)*exp(-E2/(R*Tf))-F/V*Cc(t)
#'
#' @format A data frame with 99991 rows and 10 variables
#' @usage data(cstr)
#' @name cstr
#' @keywords data.frame
#' @source \url({https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&uact=8&ved=0ahUKEwi9zsnNurXLAhXC2qYKHadMAzsQFggtMAE&url=http%3A%2F%2Fwww.mdpi.com%2F2075-1702%2F2%2F4%2F255%2Fpdf&usg=AFQjCNF9zFnmneucNLd2r6rxi9jlb4dB-A&sig2=7_4_vPoY6_ni8KDjc8Pmew})
"cstr"

#'A list of eight Benchmark data sets
#'
#'A list of eight Benchmark data sets. Input variable is u, and output variable is y
#'
#'\describe{
#'    \item{BC}{BC,Base Configuration}
#'    \item{SD}{SD,Squaring device}
#'    \item{FL}{FL,Feedback loop}
#'    \item{CN}{CN,Colored noise}
#'    \item{SF}{SF,Step Function}
#'    \item{FT}{FT,Feed Through}
#'    \item{DT}{Dt,Dead Time}
#'    \item{SS}{SS,second-order system}
#'}
#'@usage data(BMData)
#'@format A list with eight elements
#'@name BMData
"BMData"
