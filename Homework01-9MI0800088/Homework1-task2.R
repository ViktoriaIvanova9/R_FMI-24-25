library(methods)

setClass("Shape",
         slots = c(
           name = "character",
           side_lengths = "numeric"
         ),
         prototype = list(
           name = NA_character_,
           side_lengths = NA_real_
         )
)

setValidity("Shape", function(object) {
  if(object@name == "") {
    "@name must be a non-empty string"
  }
  else {
    TRUE
  }
  
  if(any(object@side_lengths <= 0)) {
    "@side_lengths must be positive numbers"
  }
  else {
    TRUE
  }
})

Shape <- function(name = "", side_lengths) {
  name <- as.character(name)
  new("Shape", name = name, side_lengths = side_lengths)
}


setMethod("show", "Shape", function(object) {
  cat(is(object)[[1]], "\n",
      " Name: ", object@name, "\n",
      " Lengths sizes: ", object@side_lengths, "\n",
      sep = " "
  )
})

setGeneric("name", function(object) standardGeneric("name"))
setGeneric("side_lengths", function(object) standardGeneric("side_lengths"))

setMethod("name", "Shape", function(object) {
  object@name
})

setMethod("side_lengths", "Shape", function(object) {
  object@side_lengths
})

setGeneric("name<-", function(object, value) standardGeneric("name<-"))
setGeneric("side_lengths<-", function(object, value) standardGeneric("side_lengths<-"))

setMethod("name<-", "Shape", function(object, value) {
  setValidity(value, object@name)
  object@name <- value
  object
})

setMethod("side_lengths<-", "Shape", function(object, value) {
  setValidity(object@name, value)
  object@side_lengths <- value
  object
})

setGeneric("is_rhombus", function(object) standardGeneric("is_rhombus"))

setMethod("is_rhombus", "Shape", function(object) {
  if (length(object@side_lengths) == 4 && length(unique(object@side_lengths)) == 1) {
    TRUE
  } else {
    FALSE
  }
})

triangle <- Shape(name = "Triangle", side_lengths = c(3, 4, 5))
rectangle <- Shape(name = "Rectangle", side_lengths = c(2, 4, 2, 4))
rhombus <- Shape(name = "Rhombus", side_lengths = c(4, 4, 4, 4))
#invalid_shape_negative_sides <- Shape(name = "Invalid", side_lengths = c(2, -3, 4))

show(triangle)
show(rectangle)
show(rhombus)

is_rhombus(triangle)
is_rhombus(rhombus)






