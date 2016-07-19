module Test where

import Control.Applicative
import Control.Monad

import Mtl

import Prim
import Error
import Char
import Combinator

pTextBlock :: Parser String
pTextBlock = someTill anyChar eof

type Parser = StateT () (Parsec Dec String)
