setClass("JavaScriptCode", contains = "character")
setClass("JavaScriptFunction", contains = "JavaScriptCode")

setMethod("show", "JavaScriptCode",
           function(object)
              cat(object, "\n"))

setGeneric("jsTranslate",
          function(code, substitute = list(), jsGlobals = character(), addSemiColon = NA, ...)
           standardGeneric("jsTranslate"))

setMethod("jsTranslate", "character",
          # read the code from a file.
function(code, substitute = list(), jsGlobals = character(), addSemiColon = NA, ...)
{
  jsTranslate(parse(code), substitute, jsGlobals, addSemiColon)
})

setMethod("jsTranslate", "ANY",
  #  work on the code.
function(code, substitute = list(), jsGlobals = character(), addSemiColon = NA, ...)
{
  substitute = sapply(substitute, toJSON)
  tmp = jsRewrite(code, substitute, jsGlobals)
  if(is.character(tmp) && !is(tmp, "JavaScriptCode"))
    tmp = new("JavaScriptCode", tmp)
  tmp
})


# inc = quote( x <- x + 1)
# jsRewrite(inc)
#   is -> typeof
# jsRewrite(quote(if(is(x, "numeric")) x))
#

# jsRewrite is the function (and its methods) that does all the work.

setGeneric("jsRewrite",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
             standardGeneric("jsRewrite"))

# we probably want to return objects that describe the language elements
# e.g. a for loop which has the three pieces with the Javascript code.
# This will allow us to do post-processing and, e.g., insert variable declarations
# i.e. var i; at the top of a block.

foo =
function(x, substitute = character(), jsGlobals = character())
{  
  if(is.name(x))
    as.character(x)
  else if(is(x, "numeric"))
    as.character(x - 1)
  else if(is.call(x))
    jsRewrite(x, substitute, jsGlobals)
}

setMethod("jsRewrite", "character",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
            sprintf('"%s"', e))

setMethod("jsRewrite", "numeric",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
            as.character(e))

setMethod("jsRewrite", "logical",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
            c("false", "true")[as.integer(e)])

JSConstants = c("NaN" = "Math.NaN", "Inf" = "Number.POSITIVE_INFINITY")
setMethod("jsRewrite", "name",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
          {
              ans = as.character(e)
              if(ans %in% names(substitute))
                substitute[ans]
              else if(ans %in% names(JSConstants))
                JSConstants[ans]
              else if(ans == "next")
                "continue"
              else
                ans
          })

setMethod("jsRewrite", "for",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
{
  var = as.character(e[[2]])
  block = jsRewrite(e[[4]])
  
  if(is.call(e[[3]])) {
        op = as.character(e[[3]][[1]])
        l = e[[3]]

        loop =
         if(op == ":" || op == "seq") {
           if(op == ":") {
              start = foo(l[[2]])
              end = foo(l[[3]])
              by = "1"
           } else if(op == "seq") {
              l = match.call(seq, l)
              if(!is.na(i <- pmatch("along", names(l)))) {
                 return(sprintf("for(%s in %s) %s",
                                 var, jsRewrite(l[[i]], substitute, jsGlobals),
                                       block))
              } else {
                if("length" %in% names(l))
                   end = foo(l[['length']])
                else if("to" %in% names(l))
                   end = foo(l[["to"]])

                if(!("from" %in% names(l)))
                   start = "0"
                else
                   start = foo(l[["from"]])
              
#              end = foo(l[[3]])                        
                 by = if("by" %in% names(l[[3]])) {
                         by = l[[3]][["by"]]
                         if(is.call(by))
                            by = jsRewrite(by, substitute, jsGlobals, FALSE)
                      } else
                         1L
              }
           }
           incr = if(by == 1) sprintf("%s++", var)
                  else sprintf("%s += %s", var, by)

           sprintf("for(%s = %s; %s < %s; %s) \n%s\n",
                      var, start, var, end, incr, block)
         } else
           sprintf("for(%s in %s) %s", var, jsRewrite(e[[3]], substitute, jsGlobals),
                                       block)
  } else  
     sprintf("for(%s in %s) %s", var, jsRewrite(e[[3]], substitute, jsGlobals), block)
                          
})


eq =
function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
{
  if(is.call(e[[3]]) && length(e[[3]][[1]]) == 1 && as.character(e[[3]][[1]]) == "function")
    return(rewriteFunction(e[[3]], as.character(e[[2]]), substitute, jsGlobals)) #XXX LHS may be more complex than a name.

    # recognize in-place updates, e.g. x = x + 1 
  if(is.call(e[[3]]) && as.character(e[[3]][[1]]) %in% c("+", "-", "*", "/") &&
       e[[2]] == e[[3]][[2]]) {
     op = as.character(e[[3]][[1]])
     if(e[[3]][[3]] == 1) {
         return(sprintf("%s%s%s", jsRewrite(e[[2]]), op, op))
     } else if(is(e[[3]][[3]], "numeric")) {
         return(sprintf("%s%s=%s", jsRewrite(e[[2]]), op, as.character(e[[3]][[3]])))
     } 
    }
  
  paste(jsRewrite(e[[2]], substitute, jsGlobals, FALSE), "=",
         jsRewrite(e[[3]], substitute, jsGlobals, FALSE))
}

setMethod("jsRewrite", "=", eq)
setMethod("jsRewrite", "<-", eq)
setMethod("jsRewrite", "<<-", eq)

setMethod("jsRewrite", "{",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, noBraces = FALSE, ...)
{
  tmp = sapply(e[seq(along = e)[-1]], jsRewrite, substitute, jsGlobals, TRUE)

  ans = paste("    ", tmp, ";", collapse = "\n", sep = "")
  if(!noBraces && length(e) > 1) 
     ans = paste("{", ans, "}", sep = "\n")
  ans
})

setMethod("jsRewrite", "(",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, noBraces = FALSE, ...)
{
  sprintf("(%s)", jsRewrite(e[[2]], substitute, jsGlobals, FALSE))
})


setMethod("jsRewrite", "call",
           function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
{
  atomic = length(e[[1]]) == 1
  op = as.character(e[[1]]) 

  if(atomic && op == "function") {
     return(rewriteFunction(e, "?", substitute, jsGlobals))
  }

   
  tmp = sapply(e, jsRewrite, substitute, jsGlobals, FALSE)
 
      # Deal with +, -, *, /, ^, etc. specially so as to preserve the order
  inlineOps = c('+', '-', '*', '/', '==', '!=', '&&', '||', ':')

  if(atomic && op %in% inlineOps)
     return(sprintf("%s %s %s", tmp[2], op, tmp[3]))
  
  if(atomic && op == "$")
     paste(tmp[2], tmp[3], sep = ".")
  else if(atomic && op == "[") {
    sprintf("%s[ %s ]", tmp[2], tmp[3]) #XXX handle x[1, 2, 3] as x[1][2][3]
  } else if(atomic && op == "switch") {
     rewriteSwitch(e, substitute, jsGlobals)
  } else if(atomic && op == "while") {
     rewriteWhile(e, substitute, jsGlobals)
  } else if(atomic && op == "^") {
     sprintf("Math.pow(%s, %s)", tmp[2], tmp[3])
  } else if(atomic && op == "is") {
     sprintf("typeof %s == \"%s\")", tmp[2], tmp[3])
  } else if(atomic && op == "length") {
     sprintf("%s.length", tmp[2])
  } else {
     sprintf("%s(%s)", tmp[1], paste(tmp[-1], collapse = ", "))
  }

})


setMethod("jsTranslate", "function",
           function(code, substitute = character(), jsGlobals = character(), addSemiColon = FALSE, ...) {
             rewriteFunction(code, "", substitute, jsGlobals)
           })
            

rewriteFunction =
function(e, name = "", substitute = character(), jsGlobals = character(), ...)
{
   isFun = is.function(e)

   if(isFun) {
     body = body(e)
     params = formals(e)
   } else {
     body = e[[3]]
     params = e[[2]]
   }

   
   block = jsRewrite(body, substitute, jsGlobals, noBraces = TRUE)
   jparams = paste(names(params), collapse = ", ")

     # Any default values for parameters go into the body of the code.
   i = sapply(params, function(x) !(is.name(x) && as.character(x) == ""))
   if(any(i))
     prefix = paste(sprintf("  if(%s == null) %s = %s;\n", 
                            names(params[i]),
                            names(params[i]),
                            sapply(params[i], jsRewrite, substitute, jsGlobals)),
                    collapse = "")
   else
     prefix = ""

   
   globals = setdiff(findLocals(body), names(params))
   varDefs = setdiff(globals, jsGlobals)
   varDefs = paste("   var", paste(varDefs, collapse = ", "), ";")

   new("JavaScriptFunction", sprintf("function %s(%s) {\n%s\n%s\n%s\n}\n", name, jparams, prefix, varDefs, block))
}


rewriteWhile =
function(e, substitute = character(), jsGlobals = character(), ...)
{

  block = jsRewrite(e[[3]], substitute, jsGlobals)
  cond = jsRewrite(e[[2]], substitute, jsGlobals, FALSE)
  sprintf("while(%s) %s", cond, block)
}

rewriteSwitch =
function(e, substitute = character(), jsGlobals = character(), ...)
{
  obj = jsRewrite(e[[2]])
  ids = names(e)[-c(1, 2)]
  cmds = sapply(e[-c(1,2)], jsRewrite, substitute, jsGlobals)
  i = grep("^[^{]", cmds)
  if(length(i))
    cmds[i] = paste("    ", cmds[i], ";")
  tmp = sprintf("%s if(%s == %s) \n%s",
                c("", rep("else", length(ids)-1)),
                rep(obj, length(ids)), ids, cmds)
  paste(tmp, "\n")
}

rewriteIf =
function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
{
# browser()
  cond = jsRewrite(e[[2]], substitute, jsGlobals, FALSE)
  block = jsRewrite(e[[3]], substitute, jsGlobals)
  
  if(length(e) == 4)
    els = paste("\nelse", jsRewrite(e[[4]], substitute, jsGlobals))
  else
    els = ""
  sprintf("if(%s)\n%s%s", cond, block, els)
}
setMethod("jsRewrite", "if", rewriteIf)


setMethod("jsRewrite", "expression",
function(e, substitute = character(), jsGlobals = character(), addSemiColon = NA, ...)
{
  sapply(e, jsRewrite, substitute, jsGlobals, TRUE)
})
