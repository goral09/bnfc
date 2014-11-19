{-
    BNF Converter: ELisp main file
    Copyright (C) 2004  Author:  Rob Stewart, Markus Forberg,
                                 Peter Gammie, Aarne Ranta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module BNFC.Backend.ELisp (makeELisp) where


-- import BNFC.Utils
import BNFC.Options hiding (Backend)
import BNFC.CF hiding (Data)
import BNFC.Backend.Base

import qualified Data.AttoLisp as L
import Data.Char (toLower)
import Data.List (nub)
import qualified Data.Text as T (Text,pack)

makeELisp :: SharedOptions -> CF -> Backend
makeELisp opts cf = do
  let cfg = unCFG cf
      (_exts,xs) = cfg
      rules = map rhsRule xs :: [[Either Cat String]]
      langName = lang opts
      lisps = deriveLanguageMode rules langName
      content = unlines $
             [";; define several class of keywords"] ++
              map show lisps ++
              ["",";; create the regex string for each class of keywords"] ++
              map show (regexes langName) ++
              ["",";; clear memory"] ++
              map show (clearMemory langName) ++
              ["",";; create the list for font-lock.",
               ";; each class of keyword is given a particular face"] ++
              map show (fontLock langName) ++
              ["",";; define the mode"] ++
              map show (defineDerivedMode langName) ++
              ["",";; povide mode to features list"] ++
              map show (provide langName)

  mkfile (lang opts ++ ".el") content

data FontKeyword = Type String | Keyword String | Function String deriving (Eq)

deriveLanguageMode :: [[Either Cat String]] -> String -> [L.Lisp]
deriveLanguageMode rules lang = [keywordsLisp,typesLisp,functionsLisp]
    where isKeyword = \case (Keyword _)  -> True; _ -> False
          isType    = \case (Type _)     -> True; _ -> False
          isFunc    = \case (Function _) -> True; _ -> False
          keywords = extractAllKeywords
          keywordsLisp  = mkKeywordsLisp  (filter isKeyword keywords)
          typesLisp     = mkTypesLisp     (filter isType keywords)
          functionsLisp = mkFunctionsLisp (filter isFunc keywords)
                          
          mkKeywordsLisp :: [FontKeyword] -> L.Lisp
          mkKeywordsLisp ys = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "keywords"),
                                       L.List [L.Symbol "quote", L.List (concatMap mkKeyword ys)]]
              where mkKeyword  = \case (Keyword s) -> [L.String (T.pack s)] ; _ -> []

          mkTypesLisp :: [FontKeyword] -> L.Lisp
          mkTypesLisp ys = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "types"),
                                    L.List [L.Symbol "quote", L.List (concatMap mkType ys)]]
              where mkType  = \case (Type s) -> [L.String (T.pack s)] ; _ -> []

          mkFunctionsLisp :: [FontKeyword] -> L.Lisp
          mkFunctionsLisp ys = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "functions"),
                                        L.List [L.Symbol "quote", L.List (concatMap mkFun ys)]]
              where mkFun  = \case (Function s) -> [L.String (T.pack s)] ; _ -> []

          extractAllKeywords :: [FontKeyword]
          extractAllKeywords = nub $ concatMap extractKeywords rules
              where 
                extractKeywords :: [Either Cat String] -> [FontKeyword]
                extractKeywords []  = []
                extractKeywords [x] = case x of Left _ -> []; Right s -> [Type s]

                extractKeywords (x:xs) =
                    let languageKeywords = concatMap extractKeyword xs
                        extractKeyword = \case Left _ -> []; Right s -> [Keyword s]
                    in case x of Left _ -> languageKeywords; Right s -> Function s : languageKeywords

regexes :: String -> [L.Lisp]
regexes lang = [keywords_regexp,types_regexp,functions_regexp]
    where
      keywords_regexp  = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "keywords-regexp"),
                                  L.List [L.Symbol "regexp-opt", L.Symbol (mkSymbol lang "keywords"),
                                           L.List [L.Symbol "quote", L.Symbol "words"]]]
      types_regexp     = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "types-regexp"),
                                  L.List [L.Symbol "regexp-opt", L.Symbol (mkSymbol lang "types"),
                                           L.List [L.Symbol "quote", L.Symbol "words"]]]
      functions_regexp = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "functions-regexp"),
                                  L.List [L.Symbol "regexp-opt", L.Symbol (mkSymbol lang "functions"),
                                           L.List [L.Symbol "quote", L.Symbol "words"]]]

clearMemory :: String -> [L.Lisp]
clearMemory lang = [keywords_clearmem,types_clearmem,functions_clearmem]
    where
      keywords_clearmem  = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "keywords"),L.Symbol "nil"]
      types_clearmem     = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "types"),L.Symbol "nil"]
      functions_clearmem = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "functions"),L.Symbol "nil"]

fontLock :: String -> [L.Lisp]
fontLock lang = [font_lock]
    where
      font_lock = L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "font-lock-keywords"),
                           L.List [L.Symbol "list",
                                    L.List [L.Symbol "cons", L.Symbol (mkSymbol lang "keywords-regexp"),
                                             L.List [L.Symbol "quote", L.Symbol "font-lock-keyword-face"]
                                           ],
                                    L.List [L.Symbol "cons", L.Symbol (mkSymbol lang "types-regexp"),
                                             L.List [L.Symbol "quote", L.Symbol "font-lock-type-face"]
                                           ],
                                    L.List [L.Symbol "cons", L.Symbol (mkSymbol lang "functions-regexp"),
                                             L.List [L.Symbol "quote", L.Symbol "font-lock-function-name-face"]
                                           ]
                                  ]
                         ]

defineDerivedMode :: String -> [L.Lisp]
defineDerivedMode lang = [defineMode]
    where
      defineMode = L.List [L.Symbol "define-derived-mode", L.Symbol (mkSymbol lang "mode"), L.Symbol "prog-mode",
                            L.List [L.Symbol "setq", L.Symbol "font-lock-defaults",
                                     L.List [L.Symbol "quote", L.List [L.Symbol (mkSymbol lang "font-lock-keywords")]]
                                   ],
                            L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "keywords-regexp"), L.Symbol "nil"],
                            L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "types-regexp"), L.Symbol "nil"],
                            L.List [L.Symbol "setq", L.Symbol (mkSymbol lang "functions-regexp"), L.Symbol "nil"]
                          ]

provide :: String -> [L.Lisp]
provide lang = [provideMode]
    where
      provideMode = L.List [L.Symbol "provide", L.List [L.Symbol "quote", L.Symbol (mkSymbol lang "mode")]]

mkSymbol :: String -> String -> T.Text
mkSymbol lang symbolSuffix = T.pack $ map toLower lang ++ "-" ++ symbolSuffix
