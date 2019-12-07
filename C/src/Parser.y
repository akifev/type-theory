{
module Parser where
import Lexer
import Grammar
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    open                                    { TSym '(' }
    close                                   { TSym ')' }
    lambda                                  { TSym '\\' }
    point                                   { TSym '.' }
    var                                     { TVar $$ }

%%

Expression:
    Applying lambda var point Expression    { App $1 (Abs $3 $5) }
    |   lambda var point Expression         { Abs $2 $4 }
    |   Applying                            { $1 }

Applying:
    Applying Atom                           { App $1 $2 }
    |   Atom                                { $1 }

Atom:
    open Expression close                   { $2 }
    |   var                                 { Var $1 }

{
parseError _ = error "Parse error"
}