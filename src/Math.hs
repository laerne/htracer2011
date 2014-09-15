module Math
    where
import Shortnames
import Data.Maybe
-- * Mathematics
-- ** Vectors

-- | Represents a 3-dimentionnal vector in the raytracer scene.
newtype Num a => SpaceVect a = SpaceVect (a, a, a)
    deriving (Show,Eq)
type Coord a = SpaceVect a -> a

-- | Accessors to the x component of a vector.
x :: Num a => Coord a
x (SpaceVect(e,_,_)) = e
-- | Accessors to the y component of a vector.
y :: Num a => Coord a
y (SpaceVect(_,e,_)) = e
-- | Accessors to the z component of a vector.
z :: Num a => Coord a
z (SpaceVect(_,_,e)) = e

-- | The euclidian norm of a vector. It is a bit slow to use, because it use
-- 'sqrt' in it.
norm :: (Floating a) => SpaceVect a -> a
norm (SpaceVect(x,y,z)) = sqrt $ x^2 + y^2 + z^2


-- | The dot product of two vectors. For two normed vectors, it is equal to the
-- cosine of the angle
(@|@) :: Num a => SpaceVect a -> SpaceVect a -> a
SpaceVect(x1,y1,z1) @|@ SpaceVect(x2,y2,z2) = x1*x2 + y1*y2 + z1*z2


-- | This function outputs the argument, but with norm 1. In mathematical terms,
-- for a given vector v, it outputs @normed v = v/norm(v)@.
normed :: (Floating a) => SpaceVect a -> SpaceVect a
normed v@(SpaceVect(x,y,z)) = SpaceVect (x',y',z')
    where normV = norm $ v
          x' = x / normV
          y' = y / normV
          z' = z / normV

-- | The cross product of two vectors, i.e. a vector orthogonal to the two
-- arguments, and with its norm equals to the product of the norm of the two
-- arguments times the sine of the angle between these two vectors.
(@*@) :: Num a => SpaceVect a -> SpaceVect a -> SpaceVect a
SpaceVect(x1,y1,z1) @*@ SpaceVect(x2,y2,z2) = SpaceVect (y1*z2 - z1*y2,  z1*x2 - x1*z2,  x1*y2 - y1*x2)

-- | The sum of two vectors.
(@+@) :: Num a => SpaceVect a -> SpaceVect a -> SpaceVect a
SpaceVect(x1,y1,z1) @+@ SpaceVect(x2,y2,z2) = SpaceVect (x1+x2, y1+y2, z1+z2)

-- | The product of a vector by a scalar.
(.*@) :: Num a => a -> SpaceVect a -> SpaceVect a
s .*@ SpaceVect(x,y,z) = SpaceVect (s*x, s*y, s*z)

-- | The opposite vector, i.e. the vector with each component of its coordinates
-- negated.
-- This is equivalent to @(s.*\@)@, but it's faster than actually use a product
-- by a scalar.
negateVect :: Num a => SpaceVect a -> SpaceVect a
negateVect (SpaceVect(x,y,z)) = SpaceVect(negate x, negate y, negate z)

-- | The difference of two vectors.
(@-@) :: Num a => SpaceVect a -> SpaceVect a -> SpaceVect a
v1 @-@ v2 = v1 @+@ (negateVect v2)

-- | This is the null vector, the neutral for the sum of two vectors.
null3x1 :: Num a => SpaceVect a
null3x1 = SpaceVect(0,0,0)


-- ** Planar vectors
-- | Represents a 2-dimentionnal vector in any plane. (Usually on the texture map).
newtype Num a => PlaneVect a = PlaneVect (a, a)
    deriving (Show,Eq)
-- | Accessors to the x component of a vector.
planar_x (PlaneVect(e,_)) = e
-- | Accessors to the y component of a vector.
planar_y (PlaneVect(_,e)) = e


-- | The sum of two planar vectors.
(@+@~) :: Num a => PlaneVect a -> PlaneVect a -> PlaneVect a
PlaneVect(x1,y1) @+@~ PlaneVect(x2,y2) = PlaneVect (x1+x2, y1+y2)

-- | The product of a planar vector by a scalar.
(.*@~) :: Num a => a -> PlaneVect a -> PlaneVect a
s .*@~ PlaneVect(x,y) = PlaneVect (s*x, s*y)

-- | The opposite planar vector, i.e. the planar vector with each component of
-- its coordinates negated.
negateVect_ :: Num a => PlaneVect a -> PlaneVect a
negateVect_ (PlaneVect(x,y)) = PlaneVect(negate x, negate y)

-- | The difference vector of two planar vectors.
(@-@~) :: Num a => PlaneVect a -> PlaneVect a -> PlaneVect a
v1 @-@~ v2 = v1 @+@~ (negateVect_ v2)


-- | The ratio of a planar vector, also called angular coefficient, is simply
-- its abciss divided by its coordinate.
planeVect_ratio (PlaneVect(a,b)) = a/b






-- ** Matrices

-- | A 3 times 3 matrix. It is represented as a 9-tuple to allow faster
-- computations than generic matrix representations.
newtype Num a => Matrix3x3 a = Mat3 (a,a,a,a,a,a,a,a,a)
    deriving (Show,Eq)

-- | Product of two matrices.
(@@@*@@@) :: Num a => Matrix3x3 a -> Matrix3x3 a -> Matrix3x3 a
(Mat3(a11,a12,a13,a21,a22,a23,a31,a32,a33)) @@@*@@@ (Mat3(b11,b12,b13,b21,b22,b23,b31,b32,b33)) =
    Mat3 ( a11*b11 + a12*b21 + a13*b31 , a11*b12 + a12*b22 + a13*b32 , a11*b13 + a12*b23 + a13*b33
        , a21*b11 + a22*b21 + a23*b31 , a21*b12 + a22*b22 + a23*b32 , a21*b13 + a22*b23 + a23*b33
        , a31*b11 + a32*b21 + a33*b31 , a31*b12 + a32*b22 + a33*b32 , a31*b13 + a32*b23 + a33*b33
        )

-- | Sum of two matrices.
(@@@+@@@) :: Num a => Matrix3x3 a -> Matrix3x3 a -> Matrix3x3 a
(Mat3(a11,a12,a13,a21,a22,a23,a31,a32,a33)) @@@+@@@ (Mat3(b11,b12,b13,b21,b22,b23,b31,b32,b33)) =
    Mat3 (a11+b11,a12+b12,a13+b13,a21+b21,a22+b22,a23+b23,a31+b31,a32+b32,a33+b33)

-- | Product of a matrix by a scalar.
(.*@@@) :: Num a => a -> Matrix3x3 a -> Matrix3x3 a
s .*@@@ (Mat3(a11,a12,a13,a21,a22,a23,a31,a32,a33)) =
    Mat3(s*a11,s*a12,s*a13,s*a21,s*a22,s*a23,s*a31,s*a32,s*a33)

-- | Difference of two matrices.
mat1 @@@-@@@ mat2 = mat1 @@@+@@@ (-1 .*@@@ mat2)

-- | The determinant a matrix.
det :: Num a => Matrix3x3 a -> a
det (Mat3(a11,a12,a13,a21,a22,a23,a31,a32,a33)) =
    (a11*a22*a33) +(a12*a23*a31) +(a13*a21*a32) -(a31*a22*a13) -(a32*a23*a11) -(a33*a21*a12)

-- | If it exists, give 'Just' the inverse matrix of the give one. Else return
-- 'Nothing'.
inv :: Fractional a => Matrix3x3 a -> Maybe (Matrix3x3 a)
inv mat =
    case abs delta of
        0 -> Nothing
        _ -> Just $ (1/delta) .*@@@
             (Mat3 (coff mat (0,0)
                   ,coff mat (0,1)
                   ,coff mat (0,2)
                   ,coff mat (1,0)
                   ,coff mat (1,1)
                   ,coff mat (1,2)
                   ,coff mat (2,0)
                   ,coff mat (2,1)
                   ,coff mat (2,2)
                   )
            )
    where
        delta = det mat

-- | Computes the cofactor at the given position of the given matrix.
coff :: Fractional a => Matrix3x3 a -> (Integer, Integer) -> a
coff (Mat3 (a11,a12,a13,a21,a22,a23,a31,a32,a33)) (i,j)= ( (i_$ sign (i,j)) * coff' (j,i) )
    where
        sign  (i,j) = ((i+j) `mod` 2)*(-2)+1
        coff' (0,0) = a22*a33-a23*a32
        coff' (0,1) = a21*a33-a23*a31
        coff' (0,2) = a21*a32-a22*a31
        coff' (1,0) = a12*a33-a32*a13
        coff' (1,1) = a11*a33-a31*a13
        coff' (1,2) = a11*a32-a31*a12
        coff' (2,0) = a12*a23-a22*a13
        coff' (2,1) = a11*a23-a21*a13
        coff' (2,2) = a11*a22-a21*a12

-- | Solves the 3x3 of the following linear system :
-- 
-- * a*x + b*y + c*z = j
-- 
-- * d*x + e*y + f*z = k
-- 
-- * g*x + h*y + i*z = j
-- 
-- The system is given in its matrix form, i.e. as
-- @(Mat3(a,b,c,d,e,f,g,h,i)) (SpaceVect(j,k,l))@
cramer :: (Fractional a) =>  Matrix3x3 a -> SpaceVect a -> SpaceVect a
cramer (Mat3(a,b,c,d,e,f,g,h,i)) (SpaceVect(j,k,l)) = SpaceVect(sol_x, sol_y, sol_z)
    where sol_x = (j*(ei_hf)+k*(_bi_hc)+l*(bf_ec)) / m
          sol_y = (c*(dl_gk)+f*(_al_gj)+i*(ak_dj)) / m
          sol_z = negate $ (b*(dl_gk)+e*(_al_gj)+h*(ak_dj)) / m

          m = a*(ei_hf)+d*(_bi_hc)+g*(bf_ec)

          ei_hf = e*i-h*f
          _bi_hc= h*c-b*i
          bf_ec = b*f-e*c
          dl_gk = d*l-g*k
          _al_gj= g*j-a*l
          ak_dj = a*k-d*j


-- | Transpose the given matrix.
transpose :: Num a => Matrix3x3 a -> Matrix3x3 a
transpose (Mat3 (a11,a12,a13,a21,a22,a23,a31,a32,a33)) =
   Mat3 (a11,a21,a31,a12,a22,a32,a13,a23,a33)


-- | Product of a matrix by a vector, computed as the matrix product where the
-- vector is a 3 times 1 matrix.
(@@@*@) :: Num a => Matrix3x3 a -> SpaceVect a -> SpaceVect a
(Mat3(a11,a12,a13,a21,a22,a23,a31,a32,a33)) @@@*@ (SpaceVect(x1,x2,x3)) =
    SpaceVect ( a11*x1 + a12*x2 + a13*x3
              , a21*x1 + a22*x2 + a23*x3
              , a31*x1 + a32*x2 + a33*x3
              )
-- | The identity matrix, i.e the matrix with 1's on its diagonal and 0's
-- elsewhere.
id3x3 :: Num a => Matrix3x3 a
id3x3 = Mat3(1,0,0,0,1,0,0,0,1)

-- | The null matrix, with every of its elements set to 0.
null3x3 :: Num a => Matrix3x3 a
null3x3 = Mat3(0,0,0,0,0,0,0,0,0)


-- ** Transformations

-- | A affine trasformation, represented by the product of a matrix, and the sum
-- of another vector, as it is done in geometry in the general case.
-- 
-- Usually in computer graphics, quaternions are used to represents every Affine
-- transformation in the plane, in order to improve graphic hardware
-- accelerations.
-- Since we do not use graphic hardware, we do not need to have quaternions.
-- Actually, representing transformation like this should be cheaper in
-- computation.
data RealFloat a => Trans a = Affine (Matrix3x3 a) (SpaceVect a)
    deriving (Show,Eq)
-- | compose two transformations. The first applied is the right (second) one
-- given.
-- Hence, we have the following assertion :
-- @
-- transf_evaluate (compose T1 T0) v = transf_evaluate T1 (transf_evaluate T0 v)
-- @
compose :: RealFloat a => Trans a -> Trans a -> Trans a
compose (Affine m1 v1) (Affine  m2 v2)=
    Affine (m2 @@@*@@@ m1) ((m2 @@@*@ v2)@+@ v1)

-- | Give the inverse transformation. In case the transformation is
-- non-inversible, return Nothig
transf_invert :: RealFloat a => Trans a -> Maybe (Trans a)
transf_invert (Affine m v) = case m_inv of
    Nothing -> Nothing
    Just m' -> Just $ Affine m' (negateVect $ m' @@@*@ v)
    where m_inv = inv m

-- | Apply a transformation to a vector.
transf_evaluate :: RealFloat a => Trans a -> SpaceVect a -> SpaceVect a
transf_evaluate (Affine m b) v = m @@@*@ v @+@ b

-- | Apply a transformation to a vector, but keeps its origin to 0. In other
-- words, the translation component is no applied.
transf_evaluate_linear :: RealFloat a => Trans a -> SpaceVect a -> SpaceVect a
transf_evaluate_linear (Affine m b) v = m @@@*@ v

-- | Return the transformation that converts back a normal vector from the image
-- of the given transformation to its preimage.
transf_transpose :: RealFloat a => Trans a -> Trans a
transf_transpose (Affine m v) = Affine mt null3x1
    where mt = transpose m

-- | The identity transformation. It does do not change any vectors when
-- evaluated.
affine_id :: (RealFloat a) => Trans a
affine_id = Affine id3x3 null3x1

-- | Convert the vector t into a transformation that represent a translation
-- along t.
affine_translate :: (RealFloat a) => SpaceVect a -> Trans a
affine_translate t = Affine id3x3 t

-- | Convert the scalar s into a transformation that scales the space with
-- factor s.
affine_scale :: (RealFloat a) => a -> Trans a
affine_scale s = Affine (Mat3(s,0,0,0,s,0,0,0,s)) null3x1

-- | Convert vector s to a scale, with different factor for each basis
-- vector. The first vector is scaled by the first componet of s, and so on.
affine_non_uniform_scale :: RealFloat a => SpaceVect a -> Trans a
affine_non_uniform_scale (SpaceVect(s1,s2,s3))= Affine (Mat3(s1,0,0,0,s2,0,0,0,s3)) null3x1

-- | Create a transformation that is a rotation with the axis and angle given.
affine_rotate ::(RealFloat a) => SpaceVect a -> a -> Trans a
affine_rotate axis angle =
    Affine
        (Mat3
            ( cosa  + oneMinusCosa * ux^2
            , uxy_a - uz_sina
            , uxz_a + uy_sina
            , uxy_a + uz_sina
            , cosa  + oneMinusCosa * uy^2
            , uyz_a - ux_sina
            , uxz_a - uy_sina
            , uyz_a + ux_sina
            , cosa  + oneMinusCosa * uz^2
            )
        )
        null3x1
    where cosa = cos angle
          oneMinusCosa = 1-cosa
          sina = sin angle
          ux = x axis
          uy = y axis
          uz = z axis
          ux_sina = ux*sina
          uy_sina = uy*sina
          uz_sina = uz*sina
          uxy_a = ux*uy*oneMinusCosa
          uxz_a = ux*uz*oneMinusCosa
          uyz_a = uy*uz*oneMinusCosa


















-- ** Various mathematic utilities
-- Positive infinite Real value
infinity :: RealFloat a => a
infinity = 1/0
-- Negative infinite Real value
minus_infinity :: RealFloat a => a
minus_infinity = negate infinity
-- Not A Number, something that can't be computed, such as 0/0 or 0^0
nan :: RealFloat a => a
nan = 0/0




-- | This function ouputs the mean value of the list.
mean :: Fractional a => [a] -> a
mean list = (sum list)/(i_ $ toInteger $ length list)

-- | This function select an interval [a,b], and a value t. It ensure that t is
-- in [a,b]. If it isn't, then t is truncated to a or b depending on whether
-- t<a or t>b.
minmax :: Ord a =>
    a -- ^ the minimum value that t can be.
    -> a -- ^ the maximal value that t can be.
    -> a -- ^ t, the value to possibly truncate.
    -> a -- ^ output value.
minmax a b t= max a $ min t b

-- | Convert radians to degrees.
toDegree :: RealFloat a => a -> a
toDegree = (*180).(/pi)
-- | Convert degrees to radians.
toRadian :: RealFloat a => a -> a
toRadian = (*pi).(/180)

-- | moduli function, but for Real numbers. Note that this function do a
-- recursive call, and hence have a performanec of O(n/x).
-- Hence, do not use this function with very small x and large n.
x `modR` n | (x==0) || (n==0) = 0
           | (x>0)  && (n>0)  =   modR'   x    n  0
           | (x<0)  && (n>0)  =   modR''  x    n  0
           | (x<0)  && (n<0)  = - modR' (-x) (-n) 0
           | (x>0)  && (n<0)  = - modR''(-x) (-n) 0
modR' x n i  = if n*(i_ i1) <= x then modR' x n i1 else x-(n*(i_ i))
    where i1 = i+1
modR'' x n i = if n*(i_ i1) >= x then modR'' x n i1 else x-(n*(i_ i1))
    where i1 = i-1



-- | outputs the square norm of a vector. It should be faster to compute than
-- its actual 'norm', since no 'sqrt' is used.
normSquared :: (Num a) => SpaceVect a -> a
normSquared v = v @|@ v
compareDist v1 v2 = compare (normSquared v1) (normSquared v2)

-- | cross product of function on the cross product of their domains
-- @
-- (f1,f2) $$ (x1,x2) = (f1 x1, f2 x2)
-- @
(f1,f2) $$ (x1,x2) = (f1 x1, f2 x2)

-- | If the list is empty, return Nothing,
-- else return Just the maximum for the comparison function m. Note m should
-- return a boolean.
f_extremum _ [] = Nothing
f_extremum m (h:t)= Just $ f_extremum' m t h
f_extremum' m (h:t) e = f_extremum' m t (m e h)
f_extremum' m [] e = e

min_fst t1@(a1,b1) t2@(a2,b2) | a1 <= a2 = t1
                              | otherwise=t2 --WARNING missing checks


-- Solve the simple second-degree equation
square_eq_roots a b c = ( ((-b) + (sqrt $ delta)) / (2*a)
                      , ((-b) - (sqrt $ delta)) / (2*a)
                      )
    where delta = square_eq_determinant a b c
square_eq_determinant a b c = b^2 - 4*a*c


True  `exclusive_or` True  = False
True  `exclusive_or` False = True
False `exclusive_or` True  = True
False `exclusive_or` False = False

