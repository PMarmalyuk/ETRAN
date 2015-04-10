setClass("extFunction",
         representation(id = "integer", 
                        name = "character",
                        applyToClass = "character",
                        fun = "function",
                        settings = "list"
         ),
         prototype(
         )
)

setClass("extFunctions",
         representation(functionsList = "list"
         ),
         prototype(
         )
)