module Main where

import qualified Chapter14.Simple as S
import qualified Chapter14.Expr as E
import qualified Chapter14.Expr2 as E2
import qualified Chapter14.ExprMonad as E3
import qualified Chapter14.Presents as P
import qualified Chapter14.Description as D
import qualified Chapter14.Origami as O

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Pretty

main :: IO ()
main = do 
  print $ executeExpr (E.Root_Root $ E.Expr_FVal 4.0) productsExample
  print $ executeExpr exprExample productsExample
  print $ executeExpr2 expr2Example productsExample
  print $ E3.executeExpr E3.exprExample productsExample
  print $ executePresents offerExample
  putStrLn $ renderHtml $ describeOffer offerExample2
  
executeExprSimple :: S.Expr -> String -> Int
executeExprSimple e s =
  let syn = S.wrap_Expr (S.sem_Expr e) (S.Inh_Expr s)
   in S.result_Syn_Expr syn

executeExpr :: Ord a => E.Root a -> [(a,Float)] -> (Maybe Int, Maybe Float, Maybe Bool)
executeExpr e products = 
  let syn = E.wrap_Root (E.sem_Root e) (E.Inh_Root products)
   in (E.intValue_Syn_Root syn, E.fltValue_Syn_Root syn, E.boolValue_Syn_Root syn)

executeExpr2 :: Ord a => E2.Expr a -> [(a,Float)] -> (Maybe Int, Maybe Float, Maybe Bool)
executeExpr2 e products = 
  let syn = E2.wrap_Expr (E2.sem_Expr e) (E2.Inh_Expr products)
   in (E2.intValue_Syn_Expr syn, E2.fltValue_Syn_Expr syn, E2.boolValue_Syn_Expr syn)

productsExample :: [(Char,Float)]
productsExample = [('a',15.0), ('b',400.0)]

exprExample :: E.Root Char
exprExample = E.Root_Root $
  E.Expr_And (E.Expr_AmountOf 'a' `E.Expr_LessThan` E.Expr_IVal 2)
             (E.Expr_FVal 300.0 `E.Expr_LessThan` E.Expr_TotalPrice)

expr2Example :: E2.Expr Char
expr2Example = 
  E2.Expr_And (E2.Expr_AmountOf 'a' `E2.Expr_LessThan` E2.Expr_IVal 2)
              (E2.Expr_FVal 300.0 `E2.Expr_LessThan` E2.Expr_TotalPrice)

executePresents :: Eq a => P.Offer a -> [a]
executePresents o = P.presents_Syn_Offer $ P.wrap_Offer (P.sem_Offer o) P.Inh_Offer

offerExample :: P.Offer Char
offerExample =
  P.Offer_Both (P.Offer_Present 'a')
               (P.Offer_BetterOf (P.Offer_Present 'b')
                                 (P.Offer_AbsoluteDiscount 30.0))

describeOffer :: (Eq a, Show a) => D.Offer a -> H.Html
describeOffer o = D.html_Syn_HtmlRoot $ D.wrap_HtmlRoot (D.sem_HtmlRoot $ D.HtmlRoot_HtmlRoot o) D.Inh_HtmlRoot

offerExample2 :: D.Offer Char
offerExample2 =
  D.Offer_Both (D.Offer_Present 'a')
               (D.Offer_BetterOf (D.Offer_Present 'b')
                                 (D.Offer_AbsoluteDiscount 30.0))
