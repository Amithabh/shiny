#' Plot Output
#' 
#' Creates a reactive plot that is suitable for assigning to an \code{output} 
#' slot.
#' 
#' The corresponding HTML output tag should be \code{div} or \code{img} and have
#' the CSS class name \code{shiny-plot-output}.
#'
#' For output, it will try to use the following devices, in this order:
#' quartz (via \code{\link[grDevices]{png}}), then \code{\link[Cairo]{CairoPNG}},
#' and finally \code{\link[grDevices]{png}}. This is in order of quality of
#' output. Notably, plain \code{png} output on Linux and Windows may not
#' antialias some point shapes, resulting in poor quality output.
#' 
#' @param func A function that generates a plot.
#' @param width The width of the rendered plot, in pixels; or \code{'auto'} to 
#'   use the \code{offsetWidth} of the HTML element that is bound to this plot. 
#'   You can also pass in a function that returns the width in pixels or 
#'   \code{'auto'}; in the body of the function you may reference reactive 
#'   values and functions.
#' @param height The height of the rendered plot, in pixels; or \code{'auto'} to
#'   use the \code{offsetHeight} of the HTML element that is bound to this plot.
#'   You can also pass in a function that returns the width in pixels or 
#'   \code{'auto'}; in the body of the function you may reference reactive 
#'   values and functions.
#' @param ... Arguments to be passed through to \code{\link[grDevices]{png}}. 
#'   These can be used to set the width, height, background color, etc.
#'   
#' @export
ShinyReactiveEnvironment <- setRefClass(
  'ShinyReactiveEnvironment',
  contains=c('ReactiveEnvironment'),
  fields = c('shinyapp'),
  methods = list(
    reactivePlot = function(plotFun, width='auto', height='auto', ...) {
      plotArgs <- list(...)
      
      if (is.function(width))
        width <- reactive(width)
      if (is.function(height))
        height <- reactive(height)

      reactive(function() {
          png.file <- tempfile(fileext='.png')

          if (is.function(width))
            width <- width()
          if (is.function(height))
            height <- height()
          
          # Note that these are reactive calls. A change to the width and height
          # will inherently cause a reactive plot to redraw (unless width and 
          # height were explicitly specified).
          if (lenght(.reactiveName)>0){
            prefix <- '.shinyout_'
            if (width == 'auto')
              width <- shinyapp$reactive$input[[paste(prefix, .reactiveName, '_width', sep='')]];
            if (height == 'auto')
              height <- shinyapp$reactive$input[[paste(prefix, .reactiveName, '_height', sep='')]];
          }
          
          if (width <= 0 || height <= 0)
            return(NULL)
          

          # If quartz is available, use png() (which will default to quartz).
          # Otherwise, if the Cairo package is installed, use CairoPNG().
          # Finally, if neither quartz nor Cairo, use png().
          if (capabilities("aqua"))
            pngfun <- png
          else if (nchar(system.file(package = "Cairo")))
            pngfun <- Cairo::CairoPNG
          else
            pngfun <- png

          do.call(pngfun, c(plotArgs, filename=png.file, width=width, height=height))
          on.exit(unlink(png.file))
          tryCatch(
            plotFun(),
            finally=dev.off())
          
          bytes <- file.info(png.file)$size
          if (is.na(bytes))
            return(NULL)
          
          pngData <- readBin(png.file, 'raw', n=bytes)
          if (shinyapp$allowDataUriScheme) {
            b64 <- caTools::base64encode(pngData)
            return(paste("data:image/png;base64,", b64, sep=''))
          }
          else {
            imageUrl <- shinyapp$savePlot(name, pngData, 'image/png')
            return(imageUrl)
          }
        }
      )
    },
    reactiveTable = function(func, ...) {
      reactive(function() {
        classNames <- getOption('shiny.table.class', 'data table table-bordered table-condensed')
        data <- func()

        if (is.null(data) || is.na(data))
          return("")
        
        return(paste(
          capture.output(
            print(xtable::xtable(data, ...), 
                  type='html', 
                  html.table.attributes=paste('class="',
                                              htmlEscape(classNames, TRUE),
                                              '"',
                                              sep=''), ...)),
          collapse="\n"))
      })
    },
    reactivePrint = function(func) {
      reactive(function() {
        return(paste(capture.output(print(func())), collapse="\n"))
      })
    }, 
    reactiveText = function(func) {
      reactive(function() {
        return(paste(capture.output(cat(func())), collapse="\n"))
      })
    },
    reactiveUI = function(func) {
      reactive(function() {
        result <- func()
        if (is.null(result) || length(result) == 0)
          return(NULL)
        # Wrap result in tagList in case it is an ordinary list
        return(as.character(tagList(result)))
      })
    },
    downloadHandler = function(filename, content, contentType=NA) {
      # Not reactive at all. 
      reactive(function(){
          shinyapp$registerDownload(.reactiveName, filename, contentType, content)
          return(sprintf('session/%s/download/%s',
                         URLencode(shinyapp$token, TRUE),
                         URLencode(.reactiveName, TRUE)))
        }
      )
    },
    invalidateLater = function(millis) {
      ctx <- .rs$currentContext()
      shinyapp$timerCallbacks$schedule(millis, function() {
        ctx$invalidate()
        .rs$flush()
      })
      invisible(NULL)
    }
  )
)

