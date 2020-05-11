#' Presidential Debate Transcripts Data File
#'
#' A tibble containing transcripts for a variety of presidential debates.
#' Presidential and Vice Presidential debates since 1960 are included, along
#' with primary debates since 2008.
#'
#' @format An object of class \code{tibble} with 17,664 rows and 6 columns.
#' \describe{
#' \item{speaker}{Name of Individual Speaking}
#' \item{text}{The text of the speaker's statement}
#' \item{type}{The election type. Possible values include Pres (Presidential), VP (Vice Presidential), Dem (Democratic Primary), and Rep (Republican Primary)}
#' \item{election_year}{The election year associated with the debate}
#' \item{date}{The date the debate took place}
#' \item{candidate}{Indicated whether or not the speaker was a candidate in the debate (as opposed to a moderator, announcer, etc.)}
#' }
#'
#' @docType data
#' @usage data(debate_transcripts)
#' @keywords datasets
#' @name debate_transcripts
#' @format A tibble.
#' @source Various sources, including \href{Rev.com}{https://www.rev.com/blog/transcript-category/debate-transcripts?view=all}, \href{debates.org}{https://www.debates.org/voter-education/debate-transcripts/}, and a variety of news sites.
'debate_transcripts'