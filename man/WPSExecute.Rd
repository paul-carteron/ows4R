% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WPSExecute.R
\docType{class}
\name{WPSExecute}
\alias{WPSExecute}
\title{WPSExecute}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling a WPS Execute request
}
\description{
WPSExecute

WPSExecute
}
\note{
Class used internally by \pkg{ows4R} to trigger a WPS Execute request
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{Execute}
\keyword{OGC}
\keyword{WPS}
\section{Super classes}{
\code{\link[ows4R:OGCAbstractObject]{ows4R::OGCAbstractObject}} -> \code{\link[ows4R:OWSHttpRequest]{ows4R::OWSHttpRequest}} -> \code{WPSExecute}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{Identifier}}{process identifier}

\item{\code{DataInputs}}{list of \link{WPSInput}}

\item{\code{ResponseForm}}{response form}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-WPSExecute-new}{\code{WPSExecute$new()}}
\item \href{#method-WPSExecute-getProcessDescription}{\code{WPSExecute$getProcessDescription()}}
\item \href{#method-WPSExecute-clone}{\code{WPSExecute$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="ERROR"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-ERROR'><code>ows4R::OGCAbstractObject$ERROR()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="INFO"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-INFO'><code>ows4R::OGCAbstractObject$INFO()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="WARN"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-WARN'><code>ows4R::OGCAbstractObject$WARN()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="encode"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-encode'><code>ows4R::OGCAbstractObject$encode()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="getClass"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-getClass'><code>ows4R::OGCAbstractObject$getClass()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="getClassName"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-getClassName'><code>ows4R::OGCAbstractObject$getClassName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="getNamespaceDefinition"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-getNamespaceDefinition'><code>ows4R::OGCAbstractObject$getNamespaceDefinition()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="isFieldInheritedFrom"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-isFieldInheritedFrom'><code>ows4R::OGCAbstractObject$isFieldInheritedFrom()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OGCAbstractObject" data-id="logger"><a href='../../ows4R/html/OGCAbstractObject.html#method-OGCAbstractObject-logger'><code>ows4R::OGCAbstractObject$logger()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="execute"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-execute'><code>ows4R::OWSHttpRequest$execute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getCapabilities"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getCapabilities'><code>ows4R::OWSHttpRequest$getCapabilities()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getException"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getException'><code>ows4R::OWSHttpRequest$getException()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getRequest"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getRequest'><code>ows4R::OWSHttpRequest$getRequest()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getRequestHeaders"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getRequestHeaders'><code>ows4R::OWSHttpRequest$getRequestHeaders()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getResponse"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getResponse'><code>ows4R::OWSHttpRequest$getResponse()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getResult"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getResult'><code>ows4R::OWSHttpRequest$getResult()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="getStatus"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-getStatus'><code>ows4R::OWSHttpRequest$getStatus()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ows4R" data-topic="OWSHttpRequest" data-id="setResult"><a href='../../ows4R/html/OWSHttpRequest.html#method-OWSHttpRequest-setResult'><code>ows4R::OWSHttpRequest$setResult()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WPSExecute-new"></a>}}
\if{latex}{\out{\hypertarget{method-WPSExecute-new}{}}}
\subsection{Method \code{new()}}{
Initializes a \link{WPSExecute} service request
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WPSExecute$new(
  capabilities,
  op,
  url,
  serviceVersion,
  identifier,
  dataInputs = list(),
  responseForm = NULL,
  storeExecuteResponse = FALSE,
  lineage = NULL,
  status = NULL,
  user = NULL,
  pwd = NULL,
  token = NULL,
  headers = c(),
  config = httr::config(),
  logger = NULL,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{capabilities}}{object of class \link{WPSCapabilities}}

\item{\code{op}}{object of class \link{OWSOperation}}

\item{\code{url}}{url}

\item{\code{serviceVersion}}{WPS service version}

\item{\code{identifier}}{process identifier}

\item{\code{dataInputs}}{a named list of data inputs, objects of class \link{WPSLiteralData}, \link{WPSComplexData} or \link{WPSBoundingBoxData}}

\item{\code{responseForm}}{response form, object of class \link{WPSResponseDocument}}

\item{\code{storeExecuteResponse}}{store execute response? object of class \code{logical}. \code{FALSE} by default}

\item{\code{lineage}}{lineage, object of class \code{logical}}

\item{\code{status}}{status, object of class \code{logical}}

\item{\code{user}}{user}

\item{\code{pwd}}{password}

\item{\code{token}}{token}

\item{\code{headers}}{headers}

\item{\code{config}}{config}

\item{\code{logger}}{logger}

\item{\code{...}}{any other parameter to pass to the request}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WPSExecute-getProcessDescription"></a>}}
\if{latex}{\out{\hypertarget{method-WPSExecute-getProcessDescription}{}}}
\subsection{Method \code{getProcessDescription()}}{
Get process description
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WPSExecute$getProcessDescription()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
an object of class \link{WPSProcessDescription}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-WPSExecute-clone"></a>}}
\if{latex}{\out{\hypertarget{method-WPSExecute-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{WPSExecute$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
