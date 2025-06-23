module Strassen (
    SqMatrix(..), 
    Matrix(..), 
    matrixAdd, 
    matrixSub, 
    matrixMul, 
    matrixMul', 
    matrixMul'', 
    strassen) where

import Data.List (transpose)

data SqMatrix a
  = Scalar a
  | Block  (SqMatrix a)  -- top-left
           (SqMatrix a)  -- top-right
           (SqMatrix a)  -- bottom-left
           (SqMatrix a)  -- bottom-right
   deriving (Eq, Show)

matrixAdd :: Num a => SqMatrix a -> SqMatrix a -> SqMatrix a
matrixAdd (Block a b c d) (Block e f g h) =
  Block (matrixAdd a e)
        (matrixAdd b f)
        (matrixAdd c g)
        (matrixAdd d h)
matrixAdd (Scalar x) (Scalar y) = Scalar (x + y)
matrixAdd _ _ = error "matrixAdd: shape mismatch"

matrixSub :: Num a => SqMatrix a -> SqMatrix a -> SqMatrix a
matrixSub (Block a b c d) (Block e f g h) =
  Block (matrixSub a e)
        (matrixSub b f)
        (matrixSub c g)
        (matrixSub d h)
matrixSub (Scalar x) (Scalar y) = Scalar (x - y)
matrixSub _ _ = error "matrixSub: shape mismatch"

matrixMul :: Num a => SqMatrix a -> SqMatrix a -> SqMatrix a
matrixMul (Block a b c d) (Block e f g h) =
  Block
    (matrixAdd (matrixMul a e) (matrixMul b g))  -- top-left:  a*e + b*g
    (matrixAdd (matrixMul a f) (matrixMul b h))  -- top-right: a*f + b*h
    (matrixAdd (matrixMul c e) (matrixMul d g))  -- bot-left:  c*e + d*g
    (matrixAdd (matrixMul c f) (matrixMul d h))  -- bot-right: c*f + d*h
matrixMul (Scalar x) (Scalar y) = Scalar (x * y)
matrixMul _ _ = error "matrixMul: shape mismatch"

-- multiply-and-add: computes x1*y1 + x2*y2 in one traversal
mulAdd
  :: Num a
  => SqMatrix a -> SqMatrix a  -- first product: x1 * y1
  -> SqMatrix a -> SqMatrix a  -- second product: x2 * y2
  -> SqMatrix a
mulAdd (Scalar x1) (Scalar y1) (Scalar x2) (Scalar y2) = Scalar (x1 * y1 + x2 * y2)
mulAdd
  (Block a11 a12 a21 a22)
  (Block b11 b12 b21 b22)
  (Block c11 c12 c21 c22)
  (Block d11 d12 d21 d22) =
    Block
      (mulAdd a11 b11 c11 d11)  -- top-left
      (mulAdd a12 b12 c12 d12)  -- top-right
      (mulAdd a21 b21 c21 d21)  -- bot-left
      (mulAdd a22 b22 c22 d22)  -- bot-right
mulAdd _ _ _ _ = error "mulAdd: shape mismatch"

-- now matrix multiplication in one traversal per pair of inputs
matrixMul' :: Num a => SqMatrix a -> SqMatrix a -> SqMatrix a
matrixMul' (Block a b c d) (Block e f g h) =
  Block
    (mulAdd a e b g)
    (mulAdd a f b h)
    (mulAdd c e d g)
    (mulAdd c f d h)
matrixMul' (Scalar x) (Scalar y) = Scalar (x * y)
matrixMul' _ _ = error "matrixMul': shape mismatch"

-- define matrix as lists of rows
newtype Matrix a = Matrix { unMatrix :: [[a]] }
  deriving (Eq, Show)

-- now using higher order functions
matrixMul'' :: Num a => Matrix a -> Matrix a -> Matrix a
matrixMul'' (Matrix a) (Matrix b) =
  let bt = transpose b
  in Matrix [ [ sum (zipWith (*) row col) | col <- bt ] | row <- a ]


-- Strassen’s algorithm
strassen :: Num a => SqMatrix a -> SqMatrix a -> SqMatrix a
strassen (Block a b c d) (Block e f g h) =
  let
    -- intermediate sums/differences
    s1  = matrixSub f h
    s2  = matrixAdd a b
    s3  = matrixAdd c d
    s4  = matrixSub g e
    s5  = matrixAdd a d
    s6  = matrixAdd e h
    s7  = matrixSub b d
    s8  = matrixAdd g h
    s9  = matrixSub a c
    s10 = matrixAdd e f
    -- seven Strassen products
    p1 = matrixMul a  s1
    p2 = matrixMul s2 h
    p3 = matrixMul s3 e
    p4 = matrixMul d  s4
    p5 = matrixMul s5 s6
    p6 = matrixMul s7 s8
    p7 = matrixMul s9 s10
    -- assemble
    c11 = matrixAdd (matrixSub (matrixAdd p5 p4) p2) p6
    c12 = matrixAdd p1 p2
    c21 = matrixAdd p3 p4
    c22 = matrixSub (matrixSub (matrixAdd p5 p1) p3) p7
  in
    Block c11 c12 c21 c22
strassen (Scalar x) (Scalar y) = Scalar (x * y)
strassen _ _ = error "matrixMul: shape mismatch"