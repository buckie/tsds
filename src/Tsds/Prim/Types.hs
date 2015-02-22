
module Tsds.Prim.Types where

import           BasePrelude hiding (toList)
import           GHC.TypeLits

import           Data.Proxy
import qualified Data.IntSet as S
import           Data.Default

import qualified Text.PrettyPrint.Boxes as B

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS

import           Tsds.Prim.Unbox ()

data Rec :: (u -> *) -> [u] -> * where
  RNil :: Rec f '[]
  (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

infixr :&


data ColStoreExist :: * where
    CStore :: (VG.Vector v b, Typeable b,Show b)=> v b -> ColStoreExist

data Store = UBox | Box | Stored

data ColStore :: (Symbol,Store,*) -> * where
    CStored ::(KnownSymbol nm, VG.Vector VS.Vector a) => prox nm ->  VS.Vector a -> ColStore  '(nm,Stored,a)
    CUnbox :: (KnownSymbol nm, VG.Vector VU.Vector a) => prox nm -> VU.Vector a -> ColStore '(nm,UBox,a)
    CBox :: (KnownSymbol nm, VG.Vector V.Vector a) => prox nm -> V.Vector a -> ColStore '(nm,Box,a)

type Int32_Col = VU.Vector (Maybe Int32)
type Int64_Col = VU.Vector (Maybe Int64)


filterRows ::  forall t . (Typeable t)=> (t -> Bool ) -> ColStoreExist -> Either String S.IntSet
filterRows pred  (CStore v) |  0 == VG.length v = Left "empty Column"
                            | Nothing  <- myPred $ v VG.! 0  = Left "Error, MisMatched types, just like python"
                            | otherwise =  Right $
                              VG.ifoldl' (\ set idx val ->
                                          maybe (error "wtf")
                                                (\b  -> if b then S.insert idx set else set)
                                          (myPred  (v VG.! idx)) )
                                        S.empty  v

              where
                myPred :: (Typeable a) => a -> Maybe Bool
                myPred x= fmap pred $ cast x

foo :: Rec ColStore [ '("Your Mou",Box,String), '("Age",UBox,Int16)]
foo =  (CBox Proxy undefined) :&  (CUnbox Proxy undefined) :& RNil

sample_table :: [(String, ColStoreExist)]
sample_table = [ ("foo", v)
               , ("bar", w)
               , ("baz", x)
               , ("bing", y)
               ]
  where
    v = CStore (VU.fromList [Just 1, Just 2, Just 3, Nothing] :: Int32_Col)
    w = CStore (VU.fromList [Just 1, Just 2, Nothing, Just 4] :: Int32_Col)
    x = CStore (VU.fromList [Just 1, Just 2, Nothing, Just 4] :: Int32_Col)
    y = CStore (VU.fromList [Nothing, Just 2, Just 3, Just 4] :: Int32_Col)

sample_col :: (String, ColStoreExist)
sample_col = ("foo", CStore (VU.fromList [Just 1, Just 2, Just 3, Nothing] :: Int32_Col))

-- type ColName = String
-- type Table = [(ColName, Column)]

-- printCell :: Show a => Maybe a -> String
-- printCell (Just c) = show c
-- printCell Nothing = "<null>"

-- printCol :: (ColName, Column) -> B.Box
-- printCol (name, (WrapCol vals)) = B.punctuateV B.center1 (B.text $ replicate (2+ B.cols col) '-') (header : [col])
--   where
--     header = B.text name
--     col = B.vcat B.center1 $ B.text `fmap` lst vals
--       where
--         lst v = fmap printCell $ VU.toList v

-- printTable :: Table -> IO ()
-- printTable table = B.printBox $ B.hsep 3 B.center1 $ printCol `fmap` table


