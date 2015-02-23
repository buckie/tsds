module Tsds.Prim.Types where

import           BasePrelude hiding (toList)

import           Data.Proxy ()

import qualified Text.PrettyPrint.Boxes as B

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG

import           Tsds.Prim.Unbox ()

-- import           GHC.TypeLits
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Storable as VS
-- data Rec :: (u -> *) -> [u] -> * where
--   RNil :: Rec f '[]
--   (:&) :: !(f r) -> !(Rec f rs) -> Rec f (r ': rs)

-- infixr :&

-- foo :: Rec ColStore [ '("Your Mou",Box,String), '("Age",UBox,Int16)]
-- foo =  (CBox Proxy undefined) :&  (CUnbox Proxy undefined) :& RNil
-- data Store = UBox | Box | Store

-- data ColStore :: (Symbol, Store, *) -> * where
--   CStored :: (KnownSymbol nm, VG.Vector VS.Vector a) => prox nm -> VS.Vector a -> ColStore '(nm, Stored, a)
--   CUnbox  :: (KnownSymbol nm, VG.Vector VU.Vector a) => prox nm -> VU.Vector a -> ColStore '(nm, UBox,   a)
--   CBox    :: (KnownSymbol nm, VG.Vector V.Vector  a) => prox nm -> V.Vector  a -> ColStore '(nm, Box,    a)


data ColStoreExist :: * where
  CStore :: (VG.Vector v b, Typeable b, Show b) => v b -> ColStoreExist

type Int32_Col = VU.Vector (Maybe Int32)
type Int64_Col = VU.Vector (Maybe Int64)
type Date_Col  = VU.Vector (Maybe Int32)
type DTime_Col = VU.Vector (Maybe Int64)

-- class (Show a) => Nullable a where
--   nullShow :: Maybe a -> String

-- instance Nullable (Maybe Int32) where
--   nullShow (Just c) = show c
--   nullShow Nothing = "<null>"

-- instance Nullable (Maybe Int64) where
--   nullShow (Just c) = show c
--   nullShow Nothing = "<null>"


type ColName = String
type Table = [(ColName, ColStoreExist)]

printCol :: Typeable a => (a -> String) -> (String, ColStoreExist) -> B.Box
printCol printMask (name, (CStore vals)) = B.punctuateV B.center1 (B.text $ replicate (2+ B.cols col) '-') (header : [col])
  where
    header = B.text name
    col = B.vcat B.center1 $ B.text `fmap` vals'
      where
        lst = VG.toList vals
        mask' :: Typeable b => b -> Maybe String
        mask' = castWrap printMask
        show' :: Typeable a => a -> String
        show' x = case mask' x of
                       Just y -> y
                       Nothing -> error $ "Mismatched Types! Wanted: \"" ++ show (typeOf x) ++ "\" Got: \"" -- ++ show (typeOf mask') ++ "\""
        vals' = show' `fmap` lst

printTable :: Typeable a => (a -> String) -> [(String, ColStoreExist)] -> IO ()
printTable printMask table = B.printBox $ B.hsep 3 B.center1 $ (printCol printMask) `fmap` table

castWrap :: (Typeable a1, Typeable a) => (a -> b) -> a1 -> Maybe b
castWrap fn x = fn `fmap` cast x

noMask :: (Typeable a, Show a) => a -> String
noMask = show

-- Main λ colToString nullMask sample_col
-- ["wtf! Wanted: Maybe Int32 Got: Maybe Int32 -> Maybe [Char]",
--  "wtf! Wanted: Maybe Int32 Got: Maybe Int32 -> Maybe [Char]",
--  "wtf! Wanted: Maybe Int32 Got: Maybe Int32 -> Maybe [Char]",
--  "wtf! Wanted: Maybe Int32 Got: Maybe Int32 -> Maybe [Char]"]

colToString :: Typeable a => (a -> String) -> ColStoreExist -> [String]
colToString printMask (CStore vals) = fmap (show' fn') (VG.toList vals)
  where
    show' :: Typeable a => (a -> Maybe [Char]) -> a -> [Char]
    show' fn x = maybe ("wtf! Wanted: " ++ show (typeOf x) ++ " Got: " ++ show (typeOf fn)) id $ fn x
    fn' :: Typeable b => b -> Maybe String
    fn' x = printMask `fmap` (cast x)

nullMask :: (Typeable a, Show a) => Maybe a -> String
nullMask (Just x) = show x
nullMask Nothing = "<null>"

sample_col :: ColStoreExist
sample_col = CStore (VU.fromList [Just 1, Just 2, Just 3, Nothing] :: Int32_Col)

sample_print :: [String]
sample_print = colToString nullMask sample_col


