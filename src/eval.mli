open LccTypes

val alpha_convert: lambda_ast -> lambda_ast
val reduce: environment -> lambda_ast -> lambda_ast
val laze: environment -> lambda_ast -> lambda_ast
val eager: environment -> lambda_ast -> lambda_ast
val isalpha: lambda_ast -> lambda_ast -> bool
val convert: engl_ast -> string 
val readable: lambda_ast -> string 
