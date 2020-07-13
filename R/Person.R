#' R6 Class representing a person
#'
#' A person has a name and a hair color.
Person <- R6::R6Class("Person",
                      public = list(
                        #' @field name First or full name of the person.
                        name = NULL,

                        #' @field hair Hair color of the person.
                        hair = NULL,

                        #' @description
                        #' Create a new person object.
                        #' @param name Name.
                        #' @param hair Hair color.
                        #' @return A new `Person` object.
                        initialize = function(name = NA, hair = NA) {
                          self$name <- name
                          self$hair <- hair
                          self$greet()
                        },

                        #' @description
                        #' Change hair color.
                        #' @param val New hair color.
                        #' @examples
                        #' P <- Person("Ann", "black")
                        #' P$hair
                        #' P$set_hair("red")
                        #' P$hair
                        set_hair = function(val) {
                          self$hair <- val
                        },

                        #' @description
                        #' Say hi.
                        greet = function() {
                          cat(paste0("Hello, my name is ", self$name, ".\n"))
                        }
                      )
)
