module Util
    where
import Shortnames
import Math

import Data.Maybe
import Debug.Trace

-- for Hashes
import Data.Int  (Int64(..))
import Data.List (mapAccumL, mapAccumR)
import Data.Bits (rotateL, rotateR, bitSize, shiftL, shiftR)
import Data.Char (ord)
import Numeric   (floatToDigits)



-- * Colors

-- | The Color class is a generic way to represent colors throught channels.
-- Actually @ColorSpace@ would have been a better name, but it's longer to type.
-- An actual color is therefore a tuple of intensities in each channels, each
-- channel corresponding to a certain color in rgb space.
class Color c where
    -- | Convert a color into a rgb triple whose value are between 0 and 255.
    toRGB :: c -> (Integer,Integer,Integer)
    -- | Convert a color into a rgba triple whose value are between 0 and 255.
    toRGBA:: c -> (Integer,Integer,Integer,Integer)
    -- | Product of two colors. The product of two colors is the usual product
    -- per channel, where values are regarded as between 0 and 1.
    (#*#) :: c -> c -> c
    -- | Sum of two colors. The sum may overlap color values out of the maximal
    -- displayable channel value.
    (#+#) :: c -> c -> c
    -- | Product of a color and a scalar.
    (.*#) :: (RealFloat a) => a -> c -> c
    -- | Light color to be used by default when none is given.
    def_lightColor :: c
    -- | Background color to be used by default when none is given.
    def_background :: c
    -- | Material color to be used by default when none is given.
    def_matColor :: c
    -- | The black color. It should be given, since it is the neutral element
    -- for color sums.
    black :: c
    -- | The white color. It should be given, since it is the neutral element
    -- for color products.
    white :: c

to8bits = round . (*255.0) . truncate01
truncate01 :: OrdRealFloat a => a -> a
truncate01 = minmax 0 1
truncate0255 = minmax 0 255

-- | Standard red green blue space. This a direct mapping from and to usual rgb
-- space, with 'Float' channels.
newtype RGBColor = RGB (Float,Float,Float)
    deriving (Show,Eq)
instance Color RGBColor where
    toRGB (RGB(r,g,b)) = ( to8bits r, to8bits g, to8bits b)
    toRGBA(RGB(r,g,b)) = ( to8bits r, to8bits g, to8bits b, 0)
    RGB(r1,g1,b1) #*# RGB(r2,g2,b2) = RGB(r1*r2,g1*g2,b1*b2)
    RGB(r1,g1,b1) #+# RGB(r2,g2,b2) = RGB(r1+r2,g1+g2,b1+b2)
    s .*# RGB(r,g,b) = RGB(s'*r, s'*g, s'*b)
        where s'= _f s
    def_lightColor = RGB(1,1,1)
    def_background = RGB(0,0,0)
    def_matColor = RGB(0.5,0.6,0.5)
    black = RGB(0,0,0)
    white = RGB(1,1,1)

-- | Standard red-green-blue space. This a direct mapping from and to usual rgb
-- space, with 'Integer' channels, with values between 0 and 255.
newtype RGBColor_Integer = RGBi (Integer,Integer,Integer)
    deriving (Show,Eq)
instance Color RGBColor_Integer where
    toRGB (RGBi(r,g,b)) = (truncate0255 r, truncate0255 g, truncate0255 b)
    toRGBA(RGBi(r,g,b)) = (truncate0255 r, truncate0255 g, truncate0255 b,0)
    RGBi(r1,g1,b1) #*# RGBi(r2,g2,b2) = RGBi(r1`prod255`r2, g1`prod255`g2, b1`prod255`b2)
    RGBi(r1,g1,b1) #+# RGBi(r2,g2,b2) = RGBi(r1+r2,g1+g2,b1+b2)
    s .*# RGBi(r,g,b) = RGBi(s'`prod255` r, s'`prod255` g, s'`prod255` b)
        where s' :: Integer
              s' = round (s*255)
    def_lightColor = RGBi(255,255,255)
    def_background = RGBi(0,0,0)
    def_matColor = RGBi(128,128,152)
    black = RGBi(0,0,0)
    white = RGBi(255,255,255)

-- | used for 8 bits integer color computation. Do the product, then divise
-- by 255.
prod255 :: (Integral a) => a -> a -> a
prod255 a b = (a*b)`div`255


-- | Simple grayscale color space. If you intend to compute a grayscale image,
-- this color space will allow you to earn computation time by avoiding to
-- compute three times the same value. BW stands for \'black and white\'.
newtype BWColor = BW Float
    deriving (Show,Eq)
instance Color BWColor where
    toRGB (BW l)      = (li,li,li)
        where li = to8bits l
    toRGBA(BW l)      = (li,li,li,0)
        where li = to8bits l
    (BW l1) #*# (BW l2) = BW (l1 * l2)
    (BW l1) #+# (BW l2) = BW (l1 + l2)
    s       .*# (BW l ) = BW (s' * l )
        where s'= _f s
    def_lightColor = BW 1
    def_background = BW 0
    def_matColor = BW 0.5
    black = BW 0
    white = BW 1

-- | Two channel space color that simulate render of objects in clay.
-- The first clay color is pale yellow, and the second is deep brown.
--newtype ClayColorTwo = Clay (Float,Float)
--    deriving (Show,Eq)
--instance Color ClayColorTwo where

--newtype CMYColor = CMY (Float,Float,Float)
--    deriving (Show,Eq)
--instance Color CMYColor where

color_mean :: Color c => [c] -> c
color_mean list = (1/ (ii $ length list)) .*# (foldr (#+#) black list)




-- * Image buffer


-- | Instance of an Image. Pixel are arranged in a 1-dimentionnal list, with a
-- pair a index. You can view this newtype like this :
--
-- @Raster2D c = Raster2D ((width,height),[pixel])@
--
-- Obviously the list should be @width*height@ elements long.
-- See 'Pixel' for much details on pixels data structures
newtype (Color c) =>
    Raster2D c = Raster2D ((Integer,Integer),[Pixel c])
    deriving (Eq,Show)
-- | A pixel is a tuple of the pixel position on the image, and the color
-- associated with it.
data (Color c) => Pixel c = Pixel (Integer,Integer) c
    deriving (Eq,Show)

-- | extract the width of the image, to avoid to write boring useless pattern
-- matchings in function who delegates actual image manipulation to a
-- subfunction, such as 'display'.
imgWidth :: Color c => Raster2D c -> Integer
imgWidth  ( Raster2D((w,_), _) )  =  w
-- | extract the height of the image, to avoid to write boring useless pattern
-- matchings in function who delegates actual image manipulation to a
-- subfunction, such as 'display'.
imgHeight:: Color c => Raster2D c -> Integer
imgHeight ( Raster2D((_,h), _) )  =  h






















-- * Scene data structures
-- ** Camera
data OrdRealFloat a => Camera a = Camera
    { cam_position  :: SpaceVect a
    , cam_u         :: SpaceVect a -- normalized direction vector
    , cam_v         :: SpaceVect a -- up vector, projected and normalized like in Gram-Schmidt
    , cam_w         :: SpaceVect a -- the remaining vector
    , cam_focus_dist:: a
    , cam_screenRect:: (PlaneVect a, PlaneVect a) -- origin and dimension in local coordinates in the <v,w> plane
    , cam_n         :: (Integer,Integer)
    , cam_antialiasing :: Integer
    , cam_lens      :: PlaneVect a
    , cam_dof       :: Bool
    , cam_name      :: String -- for debugging purpouses
    }
-- ** Lights
data (OrdRealFloat a, Color c) => Light a c = Point
    { light_position  :: SpaceVect a
    , light_color     :: c
    , light_castShadow:: Bool
    , light_name      :: String
    }
    | Directional
    { light_direction :: SpaceVect a
    , light_color     :: c
    , light_castShadow:: Bool
    , light_name      :: String
    }
    | Spot
    { light_position  :: SpaceVect a
    , light_direction :: SpaceVect a
    , light_cosAngle  :: a
    , light_color     :: c
    , light_castShadow:: Bool
    , light_name      :: String
    }
    | Area
    { light_position  :: SpaceVect a
    , light_u         :: SpaceVect a
    , light_v         :: SpaceVect a
    , light_direction :: SpaceVect a
    , light_dims      :: (PlaneVect a, PlaneVect a) -- down_left x up_right
    , light_density   :: Integer
    , light_color     :: c
    , light_castShadow:: Bool
    , light_name      :: String
    }
    | Ambient
    { light_color     :: c
    , light_castShadow:: Bool
    , light_name      :: String
    }
-- ** Materials
data (OrdRealFloat a, Color c) => Material a c = Combined
    { mat_combineFunction :: SpaceVect a -> a
    , mat_1               :: Material a c
    , mat_2               :: Material a c
    , mat_name            :: String
    }
    | Diffuse
    { mat_color           :: c
    , mat_name            :: String
    }
    | Unlit
    { mat_color           :: c
    , mat_name            :: String
    }
    | Phong
    { mat_color           :: c
    , mat_shininess       :: Integer
    , mat_name            :: String
    }
-- ** Geometries
data OrdRealFloat a => Geometry a = TriangleMesh
    -- { geo_vertices :: [Vertex a]
    { geo_faces    :: [Face a]
    , geo_name     :: String
    }
    | Sphere
    { geo_radius   :: a
    , geo_name     :: String
    }
    | Shape_Plane
    { geo_plane    :: Plane a
    , geo_name     :: String
    }
    | Shape_Box
    { geo_box      :: Box a
    , geo_name     :: String
    }


data OrdRealFloat a => Vertex a = Vertex
    { v      :: SpaceVect a -- vertex position
    , vn     :: SpaceVect a -- normal vector
    }
    deriving (Show,Eq)
transf_evaluate_vertex transf
    (Vertex { v=p , vn=n }) =
    Vertex
        { v = transf_evaluate        transf p
        , vn= transf_evaluate_linear transf p
        }

data OrdRealFloat a => Face a = Triangle
    { f_v1   :: Vertex a
    , f_v2   :: Vertex a
    , f_v3   :: Vertex a
    }
    deriving (Show,Eq)
transf_evaluate_face transf
    (Triangle{f_v1=v1,f_v2=v2,f_v3=v3}) =
     Triangle
        { f_v1=transf_evaluate_vertex transf v1
        , f_v2=transf_evaluate_vertex transf v2
        , f_v3=transf_evaluate_vertex transf v3
        }
-- ** Scene
data (OrdRealFloat a, Color c) => Scene a c = Scene
    { sc_camera     :: Camera a
    , sc_lights     :: [Light a c]
    , sc_background :: c
    , sc_shapes     :: [Shape a c]
    }
    | KDScene
    { sc_camera     :: Camera a
    , sc_lights     :: [Light a c]
    , sc_background :: c
    , sc_kdtree     :: KDTree a c
    }


--data (OrdRealFloat a, Color c) => SceneNode a c = Node (Trans a) [SceneNode a c]
--    | Leaf (Shape a c)

-- | A shape is a set of a 'Geometry', a 'Material', and a 'Trans'formation on the
-- geometry (given inverted).
data (OrdRealFloat a, Color c) => Shape a c = Shape
    { sh_geometry :: Geometry a  -- ^ Geometry of this object.
    , sh_material :: Material a c-- ^ Material of this object.
    , sh_invTransf:: Trans a     -- ^ The inverse of the transformation the objet had in the end.
    , sh_transf   :: Trans a     -- ^ The transformation the objet had in the end.
    }



























-- * KDTrees
-- ** Boxes
newtype Num a => Box a = Box (SpaceVect a, SpaceVect a)
    deriving (Show, Eq)

-- | Boxes represents a box aligned along the x, y or z axis.
-- The first vector of a box contains minimal bounds along x, y and z axis.
-- The second one contains the maximal values.
newBox :: (Num a, Ord a) => (SpaceVect a, SpaceVect a) -> Box a
newBox (b1,b2) = Box (v1, v2)
    where
        v1 = SpaceVect (min (x b1) (x b2), min (y b1) (y b2), min (z b1) (z b2))
        v2 = SpaceVect (max (x b1) (x b2), max (y b1) (y b2), max (z b1) (z b2))


isInInterval t (x,y)= (t >= x)&&(t <= y)

isInBoxFor :: (Num a, Ord a) => Coord a-> SpaceVect a -> Box a -> Bool
isInBoxFor coord p (Box(b1,b2)) = (coord p) `isInInterval` ((coord b1), (coord b2))

isInBoxRectangle (coord1,coord2) p box =
    (isInBoxFor coord1 p box) && (isInBoxFor coord2 p box)
isInBoxForXY = isInBoxRectangle (x,y)
isInBoxForXZ = isInBoxRectangle (x,z)
isInBoxForYZ = isInBoxRectangle (y,z)

isInBox :: (Num a, Ord a) => SpaceVect a -> Box a -> Bool
isInBox p box =  (isInBoxFor x p box)
              && (isInBoxFor y p box)
              && (isInBoxFor z p box)

-- ERRONEOUS
--intersectsBox :: (Num a, Ord a) => Box a -> Box a -> Bool
--intersectsBox box1 box2
--    = let box1_vertices = boxVertices box1
--      in intersectsBox' box1_vertices box2
--intersectsBox' [] _ = False
--intersectsBox' (h:t) box2 = if h `isInBox` box2 then True else intersectsBox' t box2

isOnPositiveSideOf :: (Ord a, Num a) => SpaceVect a -> Plane a -> Bool
isOnPositiveSideOf p (Plane orientation cst) = (getter orientation) p >= cst
boxIsOnPositiveSideOf (Box(b1,b2)) plane = (b1`isOnPositiveSideOf`plane) || (b2`isOnPositiveSideOf`plane)
boxIsOnNegativeSideOf (Box(b1,b2)) plane = (not $ b1`isOnPositiveSideOf`plane) || (not $ b2`isOnPositiveSideOf`plane)



containingBox (Box(b1,b2)) (Box(b'1,b'2)) = Box(v1, v2)
    where
        v1 = SpaceVect (min (x b1) (x b'1), min (y b1) (y b'1), min (z b1) (z b'1))
        v2 = SpaceVect (max (x b2) (x b'2), max (y b2) (y b'2), max (z b2) (z b'2))

boxSurface :: (Num a) => Box a -> a
boxSurface (Box(v1, v2)) = 2 * (dx*dy + dx*dz + dy*dz)
    where dx = (x v2) - (x v1)
          dy = (y v2) - (y v1)
          dz = (z v2) - (z v1)


boundaries :: (OrdRealFloat a, Color c) => Shape a c -> Box a
boundaries shape@(Shape
    { sh_geometry = geometry
    , sh_invTransf= invTransf
    , sh_transf   = transf
    }) =
    case geometry of
        TriangleMesh {geo_faces=faces} -> let
                tri_bounds = map (boundaries_tri_transf transf) faces
                tri_bounds_fst = map (\(Box(b1,_))->b1) tri_bounds
                tri_bounds_snd = map (\(Box(_,b2))->b2) tri_bounds
            in
                Box
                ( SpaceVect
                    ( fromJust $ f_extremum min $ map x tri_bounds_fst
                    , fromJust $ f_extremum min $ map y tri_bounds_fst
                    , fromJust $ f_extremum min $ map z tri_bounds_fst
                    )
                , SpaceVect
                    ( fromJust $ f_extremum max $ map x tri_bounds_snd
                    , fromJust $ f_extremum max $ map y tri_bounds_snd
                    , fromJust $ f_extremum max $ map z tri_bounds_snd
                    )
                )
        Sphere {geo_radius=r} -> let --ERROR !!! consider scales
                untransf_box = Box( SpaceVect(-r,-r,-r), SpaceVect(r,r,r))
                box = Shape_Box 
                                { geo_box=untransf_box
                                , geo_name="!:Boundaries->Sphere:"
                                }--TODO enhance performance in case of rotations or shears
            in boundaries shape{sh_geometry=box}
        Shape_Plane { geo_plane=plane } -> 
            Box ( minus_infinityVect
                , infinityVect
                )
            where infinityVect = SpaceVect(infinity,infinity,infinity)
                  minus_infinityVect = negateVect infinityVect
        Shape_Box { geo_box = box } ->
            Box ( SpaceVect(min_x,min_y,min_z)
                , SpaceVect(max_x,max_y,max_z)
                )
            where vertices = map (transf `transf_evaluate`) $ boxVertices box
                  vx's     = map x vertices
                  vy's     = map y vertices
                  vz's     = map z vertices
                  min_x    = minimum vx's
                  max_x    = maximum vx's
                  min_y    = minimum vy's
                  max_y    = maximum vy's
                  min_z    = minimum vz's
                  max_z    = maximum vz's


boxVertices :: Num a => Box a -> [SpaceVect a]
boxVertices (Box(SpaceVect(b1x,b1y,b1z),SpaceVect(b2x,b2y,b2z)))
            = SpaceVect(b1x,b1y,b1z)
            : SpaceVect(b1x,b1y,b2z)
            : SpaceVect(b1x,b2y,b1z)
            : SpaceVect(b1x,b2y,b2z)
            : SpaceVect(b2x,b1y,b1z)
            : SpaceVect(b2x,b1y,b2z)
            : SpaceVect(b2x,b2y,b1z)
            : SpaceVect(b2x,b2y,b2z)
            : []

boundaries_tri :: OrdRealFloat a => Face a -> Box a
boundaries_tri (Triangle { f_v1=v1, f_v2=v2, f_v3=v3 }) =
    Box
    ( SpaceVect
        ( minCoord3 x (v v1) (v v2) (v v3)
        , minCoord3 y (v v1) (v v2) (v v3)
        , minCoord3 z (v v1) (v v2) (v v3)
        )
    , SpaceVect
        ( maxCoord3 x (v v1) (v v2) (v v3)
        , maxCoord3 y (v v1) (v v2) (v v3)
        , maxCoord3 z (v v1) (v v2) (v v3)
        )
    )

boundaries_tri_transf :: OrdRealFloat a => Trans a -> Face a -> Box a
boundaries_tri_transf transf = boundaries_tri . (transf_evaluate_face transf)


minCoord :: (Ord a, Num a) => Coord a -> SpaceVect a -> SpaceVect a -> a
minCoord coord v1 v2 = min (coord v1) (coord v2)
maxCoord :: (Ord a, Num a) => Coord a -> SpaceVect a -> SpaceVect a -> a
maxCoord coord v1 v2 = max (coord v1) (coord v2)
minCoord3 :: (Ord a, Num a) => Coord a -> SpaceVect a -> SpaceVect a -> SpaceVect a -> a
minCoord3 coord v1 v2 v3 = min (coord v1) (minCoord coord v2 v3)
maxCoord3 :: (Ord a, Num a) => Coord a -> SpaceVect a -> SpaceVect a -> SpaceVect a -> a
maxCoord3 coord v1 v2 v3 = max (coord v1) (maxCoord coord v2 v3)


-- ** Planes
data PlaneOrientation = XY|XZ|YZ
    deriving (Show, Eq, Read)
getter :: (Num a) => PlaneOrientation -> Coord a
getter XY = z
getter XZ = y
getter YZ = x
getter_contr :: (Num a) => PlaneOrientation -> (Coord a, Coord a)
getter_contr XY = (x, y)
getter_contr XZ = (x, z)
getter_contr YZ = (y, z)


data Num a => Plane a = Plane PlaneOrientation a
    deriving (Show, Eq)

xVect :: (Num a) => SpaceVect a
xVect = SpaceVect(1,0,0)
yVect :: (Num a) => SpaceVect a
yVect = SpaceVect(0,1,0)
zVect :: (Num a) => SpaceVect a
zVect = SpaceVect(0,0,1)
baseVects XY = (xVect, yVect)
baseVects XZ = (xVect, zVect)
baseVects YZ = (yVect, zVect)
orthoVect XY = zVect
orthoVect XZ = yVect
orthoVect YZ = xVect
buildVectorAlong :: Num a => Plane a -> (a, a) -> SpaceVect a
buildVectorAlong (Plane XY cst) (x1,x2) = SpaceVect(x1, x2, cst)
buildVectorAlong (Plane XZ cst) (x1,x2) = SpaceVect(x1, cst, x2)
buildVectorAlong (Plane YZ cst) (x1,x2) = SpaceVect(cst, x1, x2)
planeVect (Plane XY cst) = SpaceVect(0,0,cst)
planeVect (Plane XZ cst) = SpaceVect(0,cst,0)
planeVect (Plane YZ cst) = SpaceVect(cst,0,0)




-- ** KDTree
data (Num a, Color c) => KDTree a c -- | A inner branch of the KDTree, stores the splitting plane
                                    = KDBranch (Plane a)
                                               (KDTree  a c)
                                               (KDTree  a c)
                                    -- | A final node of the KDTree, stores the size of the concerned box, plus the shapes belonging to it
                                    | KDLeaf   (Box a)
                                               [Shape a c]
                                    -- | The root of the KDTree, stores the larger box, every ray outside this box will eventually fails.
                                    | KDRoot   (Box a)
                                               (KDTree a c)

splitPositiveBox plane (Box(SpaceVect(x1,y1,z1), v2)) =
    case plane of Plane XY cst -> Box(SpaceVect(x1,y1,cst), v2)
                  Plane XZ cst -> Box(SpaceVect(x1,cst,z1), v2)
                  Plane YZ cst -> Box(SpaceVect(cst,y1,z1), v2)
splitNegativeBox plane (Box(v1, SpaceVect(x2,y2,z2))) =
    case plane of Plane XY cst -> Box(v1, SpaceVect(x2,y2,cst))
                  Plane XZ cst -> Box(v1, SpaceVect(x2,cst,z2))
                  Plane YZ cst -> Box(v1, SpaceVect(cst,y2,z2))
splitBox  plane box =
    case plane of Plane XY cst -> (Box(SpaceVect(x1,y1,cst), v2) ,  Box(v1, SpaceVect(x2,y2,cst)) )
                  Plane XZ cst -> (Box(SpaceVect(x1,cst,z1), v2) ,  Box(v1, SpaceVect(x2,cst,z2)) )
                  Plane YZ cst -> (Box(SpaceVect(cst,y1,z1), v2) ,  Box(v1, SpaceVect(cst,y2,z2)) )
        where
            Box(v1, v2) = box
            SpaceVect(x1,y1,z1) = v1
            SpaceVect(x2,y2,z2) = v2



-- ** KDTree construction

buildTree :: (OrdRealFloat a, Color c) => [Shape a c] -> KDTree a c
buildTree shapes =
    KDRoot outerBox $ buildTree' global_cost (shapes, shapes_boxes) (outerBox, outerSurface) 
    where shapes_boxes = map boundaries shapes
          outerBox = foldl containingBox headBox shapes_boxes
          headBox = head shapes_boxes
          outerSurface = boxSurface outerBox
          global_cost = cost_traversal + n_shapes * cost_intersect
          n_shapes = ii $ length shapes

cost_traversal :: OrdRealFloat a => a
cost_traversal = 1
cost_intersect :: OrdRealFloat a => a
cost_intersect = 1
buildTree' :: (OrdRealFloat a, Color c) =>
    a -- ^ cost from the parent
    -> ([Shape a c],[Box a]) -- ^ list of objects, and of their boundaries
    -> (Box a, a)            -- ^ Current limiting box, and it boundaries
    -> KDTree a c
    --TODO : do not re-compute surface of shapes at each step, send it as temp data
buildTree' current_cost (shapes, shapes_boxes) (currentBox,surface) =
    let
        cost_data plane =
            ( cost, plane
            , (cost_traversal+cost_posit, box_posit, shapes_posit, shape_boxes_posit, surface_posit)
            , (cost_traversal+cost_negat, box_negat, shapes_negat, shape_boxes_negat, surface_negat)
            )
            where
                cost = cost_traversal + p_posit*cost_posit + p_negat*cost_negat
                (box_posit, box_negat) = plane `splitBox` currentBox

                --TODO do not copy the code
                --POSITIVE
                p_posit = surface_posit / surface
                cost_posit = n_posit * cost_intersect
                n_posit = i_orf $ length shapes_posit

                surface_posit = boxSurface box_posit
                --shapeSurface_posit = sum $ map boxSurface shape_boxes_posit

                --TODO use a better surface approximation
                (shapes_posit, shape_boxes_posit) = unzip
                    $ filter ((`boxIsOnPositiveSideOf`plane).snd) --ERROR
                    $ zip_shapes_shapes_boxes

                --NEGATIVE
                p_negat = surface_negat / surface
                cost_negat = n_negat * cost_intersect
                n_negat = i_orf $ length shapes_negat

                surface_negat = boxSurface box_negat
                --shapeSurface_negat = sum $ map boxSurface shape_boxes_negat

                (shapes_negat, shape_boxes_negat) = unzip
                    $ filter ((`boxIsOnNegativeSideOf` plane).snd)
                    $ zip_shapes_shapes_boxes
        zip_shapes_shapes_boxes = zip shapes shapes_boxes





        costs = map cost_data planes

        ( best_cost, best_plane
            ,   (best_cost_posit, best_box_posit, best_shapes_posit, best_shape_boxes_posit, best_surface_posit)
            ,   (best_cost_negat, best_box_negat, best_shapes_negat, best_shape_boxes_negat, best_surface_negat)
            )
            = fromJust $ f_extremum get_minCost costs
    in
        case shapes of
            [] -> KDLeaf currentBox []
            _  ->
                if best_cost < current_cost then
                    KDBranch best_plane
                        (buildTree' best_cost_posit (best_shapes_posit,best_shape_boxes_posit) (best_box_posit, best_surface_posit) )
                        (buildTree' best_cost_negat (best_shapes_negat,best_shape_boxes_negat) (best_box_negat, best_surface_negat) )
                else
                    KDLeaf currentBox shapes

        where
            -- On every box shape_box, we get a plane like
            -- Plane orientation (x $ fst shape_box)
            -- TODO remove double planes
            planes = (planes' XY)++(planes' XZ)++(planes' YZ)
            planes' orientation= map ((Plane orientation).(getter orientation))
                                    $ flattenBoxList
                                    $ shapes_boxes

flattenBoxList [] = []
flattenBoxList (Box(v1,v2):t) = v1:v2:(flattenBoxList t)


get_minCost cost1@(c1,_,_,_) cost2@(c2,_,_,_) = if c1 <= c2 then cost1 else cost2

nextOrientation XY = YZ
nextOrientation YZ = XZ
nextOrientation XZ = XY

sceneToKDScene :: (OrdRealFloat a, Color c) => Scene a c -> Scene a c
sceneToKDScene Scene
        { sc_camera     = camera
        , sc_lights     = lights
        , sc_background = background
        , sc_shapes     = shapes
        }
--    = KDScene
--        { sc_camera     = camera
--        , sc_lights     = lights
--        , sc_background = background
--        , sc_kdtree     = buildTree shapes
--        }
    = let tmp = buildTree shapes
      in KDScene
            { sc_camera     = camera
            , sc_lights     = lights
            , sc_background = background
            , sc_kdtree     = tmp
            }

    
    
    
-- * Debug Utilities
kdprint :: (Show a, Num a, Color c) => KDTree a c -> String
kdprint = kdprint' 0
kdprint' :: (Show a, Num a, Color c) => Integer -> KDTree a c -> String
kdprint' i (KDRoot box child) = (indent i) ++ "KDRoot(\n"++(kdprint' (i+1) child)++")\n"
kdprint' i (KDBranch _ c1 c2) = (indent i) ++ "KDBranch(\n" ++(kdprint' (i+1) c1)++",\n"++(kdprint' (i+1) c2)++")\n"
kdprint' i (KDLeaf box sh)     = (indent i) ++ "KDLeaf(\n"++(show box)++"--"++(show $ length sh)++" shapes)\n"
indent 0 = ""
indent i = "  " ++ (indent (i-1))


-- * hash functions
type Hash = Int64
class Hashable h where
    hash :: h -> Hash

hash_length :: Int
hash_length = (bitSize seed_def)
half_hash_length :: Int
half_hash_length = hash_length `div` 2
maxHash :: Hash
maxHash = 9223372036854775807 
    
seed_def :: Hash
seed_def =  945581987819956
seed_a   :: Hash
seed_a   = 5285236650360209
seed_b   :: Hash
seed_b   = 1105126240622966
seed_p   :: Hash
seed_p   = 6011000990139424 --prime number, see http://msdn.microsoft.com/en-us/library/ee621251.aspx
seed_e   :: Hash
seed_e   = 5476976892674397

h1 %% h2 = (h1*seed_e + h2)

hashToInterval :: (RealFloat a) => Hash -> (a,a) -> a -- I use the MAD method
hashToInterval (h) (t0,t1) = t0+  (ii ((seed_a*h + seed_b) `mod` seed_p)/(ii seed_p))*t
    where t = (t1-t0)
hashToPlaneInterval :: (RealFloat a) => Hash -> (PlaneVect a, PlaneVect a) -> PlaneVect a 
hashToPlaneInterval (h) (PlaneVect(au,av),PlaneVect(bu,bv)) =
    PlaneVect (hashToInterval h (au,bu), hashToInterval (rotateR h 7) (av,bv))

instance Hashable Int64 where
    hash = id
instance Hashable Integer where
    hash = hash_integer
hash_integer i = if i /= 0 then (i_ i)+(hash_integer $ shiftR i hash_length) else 0

instance Hashable Float where
    hash x = hash h
        where (mantiss,exponent) = (map ii, ii) $$ (floatToDigits 2 x)
              (acc,h) = mapAccumR (\acc t -> (acc*seed_e, t+acc)) exponent mantiss
instance Hashable Double where
    hash x = hash h
        where (mantiss,exponent) = (map ii, ii) $$ (floatToDigits 2 x)
              (acc,h) = mapAccumR (\acc t -> (acc*seed_e, t+acc)) exponent mantiss
instance Hashable Char where
    hash c = ii $ ord c

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hash (x,y) = (hash x) %% (hash y)
instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hash (x,y,z) = ((hash x) %% (hash y)) %% (hash z) -- for tuples, we fold left
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (a,b,c,d) where
    hash (w,x,y,z) = (((hash w) %% (hash x)) %% (hash y) %% (hash z))
instance (Hashable a) => Hashable [a] where
    hash list = foldr (%%) seed_def $ map hash list


instance (Hashable a, Num a) => Hashable (SpaceVect a) where
    hash (SpaceVect(a1,a2,a3)) = hash (a1,a2,a3)
instance (Hashable a, Num a) => Hashable (PlaneVect a) where
    hash (PlaneVect(a1,a2)) = hash (a1,a2)


class (Hashable a, Ord a, RealFloat a) => OrdRealFloat a
instance OrdRealFloat Float
instance OrdRealFloat Double

