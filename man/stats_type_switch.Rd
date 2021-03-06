% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_type_switch.R
\name{stats_type_switch}
\alias{stats_type_switch}
\title{Switch type of statistics.}
\usage{
stats_type_switch(type)
}
\arguments{
\item{type}{A character specifying the type of statistical approach:
\itemize{
\item \code{"parametric"}
\item \code{"nonparametric"}
\item \code{"robust"}
\item \code{"bayes"}
}

You can specify just the initial letter.}
}
\description{
Relevant mostly for \code{ggstatsplot} and \code{statsExpressions} packages, where
different statistical approaches are supported via this argument: parametric,
non-parametric, robust, and Bayesian. This switch function converts strings
entered by users to a common pattern for convenience.
}
\section{Examples}{
\if{html}{\out{<div class="sourceCode r">}}\preformatted{stats_type_switch("p")
}\if{html}{\out{</div>}}\preformatted{#> [1] "parametric"
}\if{html}{\out{<div class="sourceCode r">}}\preformatted{stats_type_switch("bf")
}\if{html}{\out{</div>}}\preformatted{#> [1] "bayes"
}
}

