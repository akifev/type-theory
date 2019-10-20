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
    Space lambda var point Expression       { Expression (Just $1) (Just (Lambda $3 $5)) }
    | lambda var point Expression           { Expression (Nothing) (Just (Lambda $2 $4)) }
    | Space                                 { Expression (Just $1) (Nothing) }

Space:
    Space Atom                              { Space (Just $1) $2 }
    |   Atom                                { Space (Nothing) $1 }

Atom:
    open Expression close                   { Atom $2 }
    |   var                                 { Var $1 }

{
parseError _ = error "Parse error"
}