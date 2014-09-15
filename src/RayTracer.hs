module RayTracer
    where
import Shortnames
import Util
import Math
import Data.Maybe (catMaybes)
import Debug.Trace
import List (sortBy)

-- | The maximum error allowed when computing intersections.
epsilon :: (OrdRealFloat a) => a
epsilon = 5e-4

full_range :: (OrdRealFloat a) => Range a
full_range = (epsilon, infinity)







-- * Initial ray casting

traceFullScene :: (OrdRealFloat a, Color c) => Scene a c -> Raster2D c
traceFullScene scene =
    let n@(nx,ny) = cam_n $ sc_camera scene
        list = [(i,j)| i<-[0..(nx-1)], j<-[0..(ny-1)]]
    in Raster2D(n, traceScene scene list)

-- | Trace the full image of the given scene.
traceScene :: (OrdRealFloat a, Color c) => Scene a c -> [(Integer,Integer)] -> [Pixel c]
traceScene scene idxList = map (tracepixel scene) idxList

traceFullBox :: (OrdRealFloat a, Color c) => Scene a c -> Raster2D c
traceFullBox scene = 
    let n@(nx,ny) = cam_n $ sc_camera scene
        list = [(i,j)| i<-[0..(nx-1)], j<-[0..(ny-1)]]
    in Raster2D(n, traceBox scene list)

traceBox :: (OrdRealFloat a, Color c) => Scene a c -> [(Integer,Integer)] -> [Pixel c]
traceBox scene idxList = map (traceBox_pixel scene) idxList








-- | Trace a the pixel from the give scene at the given position in the pixel
-- grid.
tracepixel :: (OrdRealFloat a, Color c) => Scene a c -> (Integer, Integer) -> Pixel c
tracepixel scene pixelIdx  = Pixel pixelIdx $ color_mean values
    where rays   = pixelRays (sc_camera scene) pixelIdx
          values = map (traceRay scene) rays

traceRay scene ray = colorAtPoint scene $ closestIntersection full_range ray scene


traceBox_pixel :: (OrdRealFloat a, Color c) => Scene a c -> (Integer, Integer) -> Pixel c
traceBox_pixel scene pixelIdx  = Pixel pixelIdx $ color_mean colors
    where KDRoot box _ = sc_kdtree scene
          rays = pixelRays (sc_camera scene) pixelIdx
          colors = map (colorOfHit . (`hitsBoxAt` box)) rays
          colorOfHit hit =
              case hit of Nothing -> black
                          Just _  -> white

-- | Computes the ray cast from the camera for the given pixel position in the
-- screen.
pixelRays :: OrdRealFloat a => Camera a -> (Integer, Integer) -> [Ray a]
pixelRays camera pixelIdx = 
    let points = if (cam_antialiasing camera) <= 0
        then
            [pixelPosition camera pixelIdx]
        else
            hash_pixelPosition camera pixelIdx
    in map (cameraRay camera) points
        

-- | Give the ray direction from the camera through the given point
--cameraRay :: OrdRealFloat a => Camera a -> SpaceVect a -> Ray a
--cameraRay camera p = (p, normed $ p @-@ (cam_position camera))
cameraRay :: OrdRealFloat a => Camera a -> SpaceVect a -> Ray a
cameraRay camera p = (pos, normed $ p @-@ pos)
    where
        cam_pos = cam_position camera
        PlaneVect(lensU,lensV) = cam_lens camera
        halfLens = PlaneVect(lensU/2, lensV/2)
        hashToUse = hash (cam_pos,halfLens,p)
        PlaneVect(selectedU,selectedV) = hashToPlaneInterval hashToUse (negateVect_ halfLens, halfLens)
        pos = if cam_dof camera
                then
                    cam_pos @+@ (selectedU .*@ cam_u camera) @+@ (selectedV .*@ cam_v camera)
                else
                    cam_pos

-- | Give the position of the given pixel position on the camera screen.
pixelPosition :: OrdRealFloat a
    => Camera a             -- ^ The camera.
    -> (Integer,Integer)
        -- ^ Pixel position, in terms of its number in the image, where (0,0) is the down left of the image.
    -> SpaceVect a          -- ^ Position of the pixel on the space rendered.
pixelPosition camera = glob_pos
    where
        (screenOrigin, screenDim) = cam_screenRect camera
        step_u = (planar_x screenDim) / (i_ $ fst n)
        step_v = (planar_y screenDim) / (i_ $ snd n)
        n = cam_n camera
        local_pos_u = (+(step_u/2)) . (+(planar_x screenOrigin)) . (*step_u) . i_
        local_pos_v = (+(step_v/2)) . (+(planar_y screenOrigin)) . (*step_v) . i_
        glob_pos_u lu = (lu.*@(cam_u camera))
        glob_pos_v lv = (lv.*@(cam_v camera))
        cam_pos = cam_position camera
        focus_dist = actual_cam_focus_dist camera
        glob_pos = (   ( (negate focus_dist).*@(cam_w camera) ) @+@ cam_pos @+@   )
            . (uncurry (@+@)) . ((glob_pos_u.local_pos_u, glob_pos_v.local_pos_v) $$)

hash_pixelPosition :: OrdRealFloat a
    => Camera a             -- ^ The camera.
    -> (Integer,Integer)
        -- ^ Pixel position, in terms of its number in the image, where (0,0) is the down left of the image.
    -> [SpaceVect a]          -- ^ Position of the pixel on the space rendered.
hash_pixelPosition camera idx = let
        (screenOrigin, screenDim) = cam_screenRect camera
        (nx,ny) = cam_n camera
        
        step_u = (planar_x screenDim) / (i_ nx)
        step_v = (planar_y screenDim) / (i_ ny)
        f (curr_nx,curr_ny) = screenOrigin @+@~ PlaneVect((i_ curr_nx)*step_u,(i_ curr_ny)*step_v)
        localSubArea = (f idx, f $ ((+1),(+1)) $$ idx )
        
        antialiasingLevel = cam_antialiasing camera
        hashes = map hash [(i, actual_cam_focus_dist camera, cam_position camera, cam_screenRect camera)|i<-[1..antialiasingLevel]]
        localSelectedPoints = map (`hashToPlaneInterval` localSubArea) hashes
        
        cam_dir = negateVect $ cam_w camera
        localToGlobal (PlaneVect(c1,c2)) = 
            (cam_position camera) @+@ (actual_cam_focus_dist camera .*@ cam_dir) @+@(c1.*@cam_u camera) @+@ (c2.*@cam_v camera)
        selectedPoints = map localToGlobal localSelectedPoints
    in
        selectedPoints

actual_cam_focus_dist camera = if d <= 0 then 1 else d
    where d = cam_focus_dist camera











-- * Color computation

-- | Computes the lighting and color in a given scene at a given position.
colorAtPoint :: (OrdRealFloat a, Color c) =>
    Scene a c -- ^ The scene where to render.
    -> Maybe (RayHit a c)
        -- ^ The hit, i.e, a position in space, a material, a normal vector, and
        -- the incident vector of ray casting. If no hit is given, then, the
        -- background color is output.
    -> c -- ^ outputed color.
colorAtPoint scene Nothing = sc_background scene
colorAtPoint scene (Just hit@Hit
    { hit_position = pos
    , hit_incoming = dir
    , hit_normal   = normal
    , hit_material = material
    }) =
    let lights = sc_lights scene
        colorFromLight fct light =
            let thatMuchShadow = pos `isThatMuchShadowedFrom` light
            in
                thatMuchShadow .*# (fct light)
        isThatMuchShadowedFrom = shadowFactor scene
    in
    case material of
    Combined
        { mat_combineFunction = f
        , mat_1               = m1
        , mat_2               = m2
        }
        -> (s .*# (colorAtPoint scene hit1)) #+# ((1-s) .*# (colorAtPoint scene hit2))
            where s = f pos
                  hit1 = Just hit{hit_material=m1}
                  hit2 = Just hit{hit_material=m2}
    Diffuse
        { mat_color = m_color
        , mat_name  = _
        }
        -> foldr (#+#) black $ (map (colorFromLight fun) lights)
            where fun light = 
                      (lightAngle light pos normal) .*# (l_color #*# m_color)
                      where
                          l_color = light_color light
    Unlit
        { mat_color = color } -> color
    Phong
        { mat_color     = m_color
        , mat_shininess = m_shininess
        }
        -> foldr (#+#) black $ (map (colorFromLight fun) lights)
            where fun light = 
                      ( l_angle.*# (l_color #*# m_color)) #+# ((l_causticAngle^^m_shininess).*#l_color)
                      where
                          l_color = light_color light
                          l_angle = lightAngle light pos normal
                          l_causticAngle = lightCausticAngle light pos normal dir


shadowFactor :: (OrdRealFloat a, Color c) => Scene a c -> SpaceVect a -> Light a c -> a
shadowFactor scene p light = 
    let
        shadowed = 0
        lit      = 1
        actualShadowCheck range ray = 
          if not $ light_castShadow light then
              lit
          else
              case (closestIntersection range ray scene) of Nothing -> lit
                                                            Just _  -> shadowed
    in
        case light of
            Point
                { light_position  = l_pos
                }
                ->  actualShadowCheck range ray
                where distance= norm toLight
                      ray = (p,toLight)
                      toLight = l_pos @-@ p
                      range = (epsilon, distance)
            Directional
                { light_direction = l_dir
                }
                -> actualShadowCheck range ray
                where ray = (p,toLight)
                      toLight = negateVect l_dir
                      range= (epsilon,infinity)
            Spot
                { light_position   = l_pos
                , light_direction  = l_dir
                , light_cosAngle   = l_cosAngle
                }
                -> if (dirToLight @|@ negateVect l_dir) <= l_cosAngle
                    then  shadowed
                    else  actualShadowCheck range ray
                where distance= norm toLight
                      ray = (p,dirToLight)
                      toLight = l_pos @-@ p
                      dirToLight = normed toLight
                      range = (epsilon, distance)
            Area
                { light_position  = l_pos
                , light_u         = l_u
                , light_v         = l_v
                , light_direction = l_dir
                , light_density   = density
                , light_dims      = (uv_pos, uv_size)
                }
                -> mean shadowFactors
                where
                    nx = round $ (i_ density) * (planar_x uv_size)
                    ny = round $ (i_ density) * (planar_y uv_size)
                    n's = [(a,b)|a<-[0..nx-1],b<-[0..ny-1]]
                    
                    PlaneVect(sizeU,sizeV) = uv_size
                    step_u = (sizeU) / (i_ nx)
                    step_v = (sizeV) / (i_ ny)

                    f (curr_nx,curr_ny) = uv_pos @+@~ PlaneVect((i_ curr_nx)*step_u,(i_ curr_ny)*step_v)
                    localSubArea curr_n = (f curr_n, f $ ((+1),(+1)) $$ curr_n )
                    localSubAreas = map localSubArea n's
                    hashes = map ((%%(hash (l_pos,p,density,l_dir))).hash) n's
                    localSelectedPoints = map (uncurry hashToPlaneInterval) $ zip hashes localSubAreas
                    --localSelectedPoints = map (uncurry (@+@~)) $ zip localSubAreas localPosFromHash's
                    
                    localToGlobal (PlaneVect(c1,c2)) = l_pos @+@ (c1.*@l_u) @+@(c2.*@l_v)
                    selectedPoints = map localToGlobal localSelectedPoints

                    toLight's = map (@-@p) selectedPoints
                    distance's = map norm toLight's

                    range's = zip (repeat epsilon) distance's
                    ray's   = zip (repeat p) toLight's

                    shadowFactors = map (uncurry actualShadowCheck) $ zip range's ray's
            Ambient {} -> lit

-- | Compute the cosine of the angle at a given position and normal, between that normal, and
-- the light (i.e. the light position minus the point position) vector to that position.
lightDirection :: (OrdRealFloat a, Color c)
    => Light a c    -- ^ The light
    -> SpaceVect a  -- ^ The position
    -> SpaceVect a  -- ^ The cosine of the angle between the normal and the light vector.
lightDirection light pos = case light of
    Point
        { light_position  = l_pos }
        -> normed (l_pos @-@ pos)
    Directional
        { light_direction = l_dir }
        -> negateVect l_dir --l_dir should already be normed
    Spot
        { light_position  = l_pos }
        -> normed (l_pos @-@ pos)
    Area
        { light_direction = l_dir }
        -> negateVect l_dir --l_dir should already be normed
    --Ambient is unneeded since there is no call to this function in that case

lightAngle :: (OrdRealFloat a, Color c)
    => Light a c    -- ^ The light
    -> SpaceVect a  -- ^ The position
    -> SpaceVect a  -- ^ The normal vector
    -> a            -- ^ The cosine of the angle between the normal and the light vector.
lightAngle Ambient{} _ _ = 1
lightAngle light pos normal = truncate01 $ (l_dir@|@normal)
    where l_dir = lightDirection light pos


lightCausticAngle light pos normal dir = truncate01 $ (e @|@ r)
    where l = lightDirection light pos
          e = negateVect dir
          r = ((2*l_angle) .*@ normal) @-@ l
          l_angle = lightAngle light pos normal -- TODO reuse data used for the diffuse part ?



-- * ray intersection computation


-- | Return the closest hit of the shape with every object.
-- If there is no intersection, 'Nothing' is returned.
closestIntersection :: (OrdRealFloat a, Color c) => Range a -> Ray a -> Scene a c -> Maybe (RayHit a c)
closestIntersection range ray scene@(Scene{}) =
    let intersections = everyIntersections range ray (sc_shapes scene)
    in  case intersections of
            [] -> Nothing
            list -> Just (snd $ closestHit intersections)
closestIntersection range ray scene@(KDScene{}) =
    let boxList = getEveryBoxIntersections (sc_kdtree scene) ray
        kdClosestIntersection [] = Nothing
        kdClosestIntersection ((box,shapesInBox):tail)=
            let intersections = everyIntersections range ray shapesInBox
                innerIntersections = filter ((`isInBox`box) . hit_position . snd) intersections
            in case innerIntersections of
                    []   -> kdClosestIntersection tail
                    list -> Just (snd $ closestHit list)
    in
        kdClosestIntersection boxList


-- | Return the closest hit of the shape with every object. There must be at
-- least one hit in the list of hits
closestHit [] = error "call to closest hit with []."
closestHit (head:list) = closestHit' head list
closestHit' max_pair@(max_t, max_hit) (pair:list) = if t<max_t
    then closestHit' pair list else closestHit' max_pair list
    where t=fst pair
closestHit' max_pair [] = max_pair





-- | Return every hit of the shape with every object.
everyIntersections :: (OrdRealFloat a, Color c) => Range a -> Ray a -> [Shape a c] -> [(a, RayHit a c)]
everyIntersections range ray shapes =
    concat $ map (shapeIntersections range ray) shapes

-- | Return every hit of the shape with the object.
shapeIntersections :: (OrdRealFloat a, Color c) => Range a -> Ray a -> Shape a c -> [(a, RayHit a c)]
shapeIntersections range ray@(r_orig, r_dir) shape =
    let inters = geometryIntersections range (transf_ray transf_i ray) (sh_geometry shape)
        createHit (t,p,normal) = Hit
            { hit_position = transf_evaluate transf p
            , hit_incoming = r_dir
            , hit_normal   = normed 
                $ transf_evaluate (transf_transpose transf_i) normal -- TODO use fastest computations
            , hit_material = sh_material shape
            }
        createHitPair pair@(t,_,_) = (t, createHit pair)

        transf_i = (sh_invTransf shape)
        transf   = (sh_transf    shape)
    in
        map createHitPair inters


-- | Return a list of intersection from the ray with the geometry.
geometryIntersections :: (OrdRealFloat a)
    => Range a
    -> Ray a                -- ^ The cast 'Ray'
    -> Geometry a           -- ^ The geometry to test
    -> [(a, SpaceVect a, SpaceVect a)]
        -- ^ A list of intersections. An intersection is a tuple of distance
        -- from the camera, the position of the intersection  and a the normal
        -- vector at the intersection.
geometryIntersections range ray TriangleMesh
        { geo_faces = faces
        } = let ts = map (triangleIntersection range ray) faces
            in catMaybes ts
geometryIntersections range (o, d) Sphere
    { geo_radius = r
    } = let r2=r*r
            (x1,x2) = square_eq_roots a b c
            createData t = (t, pt, normed pt )
                where pt = p t
            a = normSquared d
            b = 2*(d@|@o)
            c = (normSquared o) - r2
            p t = o @+@ (t .*@ d) 
        in map createData $ filter (`isInRange` range) [x1,x2]
geometryIntersections range ray Shape_Plane
    { geo_plane = plane@(Plane ori cst)
    } = let (t,p) = ray `intersectionWithPlane` plane
        in if t `isInRange` range then [(t,p, orthoVect ori)] else []
geometryIntersections range ray Shape_Box
    { geo_box = box@(Box(b1,b2))
    } =
        prepend_intersection   inter_bx1 YZ
        $ prepend_intersection inter_by1 XZ
        $ prepend_intersection inter_bz1 XY
        $ prepend_intersection inter_bx2 YZ
        $ prepend_intersection inter_by2 XZ
        $ prepend_intersection inter_bz2 XY
        $ []
        where 
            prepend_intersection ((t,p),n) orientation list =
                if isInBoxRectangle (getter_contr orientation) p box
                        && t `isInRange` range
                    then (t,p,n):list
                    else list --TODO nicer code, not a copied and paste one
            inter_bx1 = (intersectionWithPlane ray $ Plane YZ $ x b1, negateVect xVect)
            inter_by1 = (intersectionWithPlane ray $ Plane XZ $ y b1, negateVect yVect)
            inter_bz1 = (intersectionWithPlane ray $ Plane XY $ z b1, negateVect zVect)
            inter_bx2 = (intersectionWithPlane ray $ Plane YZ $ x b2,            xVect)
            inter_by2 = (intersectionWithPlane ray $ Plane XZ $ y b2,            yVect)
            inter_bz2 = (intersectionWithPlane ray $ Plane XY $ z b2,            zVect)

-- | Return wether a intersection occurs, and if so, return distance from camera
-- and normal vector.
triangleIntersection  :: (OrdRealFloat a) => Range a
    -> Ray a  -- ^ The cast ray
    -> Face a -- ^ The triangle
    -> Maybe (a, SpaceVect a, SpaceVect a)
        -- ^ If no intersection occurs, 'Nothing' is returned.
        -- Else, return 'Just' a tuple with the distance from the origin point
        -- ray, and the interpolated normal vector at the intersection.
triangleIntersection range (ray_orig, ray_dir) triangle =
    let
        a @( SpaceVect(ax,ay,az)    ) = v $ f_v1 triangle
        b @( SpaceVect(bx,by,bz)    ) = v $ f_v2 triangle
        c @( SpaceVect(cx,cy,cz)    ) = v $ f_v3 triangle
        b'@( SpaceVect(bx',by',bz') ) = a @-@ b
        c'@( SpaceVect(cx',cy',cz') ) = a @-@ c
        SpaceVect(dx,dy,dz) = ray_dir
        SpaceVect(ex,ey,ez) = ray_orig
        cramer_matrix = Mat3( bx', cx', dx
                            , by', cy', dy
                            , bz', cz', dz
                            )
        cramer_vect   = SpaceVect(ax-ex,ay-ey,az-ez)
        SpaceVect(beta,gamma,t) = cramer cramer_matrix cramer_vect
    in
        if ((not $ t `isInRange` range)||(gamma<0)||(gamma>1)||(beta<0)||(beta>(1-gamma)))
            then Nothing
            else let normal1 = vn $ f_v1 triangle
                     normal2 = vn $ f_v2 triangle
                     normal3 = vn $ f_v3 triangle
                     normal = normed $ ((1-beta-gamma).*@normal1) @+@ (beta.*@normal2) @+@ (gamma.*@normal3)
            --else let normal1 = flipVectorIfAngleIsLowerThanHalfPi (vn $ f_v1 triangle) ray_dir
            --         normal2 = flipVectorIfAngleIsLowerThanHalfPi (vn $ f_v2 triangle) ray_dir
            --         normal3 = flipVectorIfAngleIsLowerThanHalfPi (vn $ f_v3 triangle) ray_dir
            --         normal = normed $ ((1-beta-gamma).*@normal1) @+@ (beta.*@normal2) @+@ (gamma.*@normal3)
                 in
                     Just ( t
                          , a @+@ (beta .*@ (b@-@a)) @+@ (gamma .*@ (c@-@a)) 
                          , flipVectorIfAngleIsLowerThanHalfPi normal ray_dir
                          ) --flip vector if point backwards.


flipVectorIfAngleIsLowerThanHalfPi v1 v2 = if v1@|@v2 >= 0 then negateVect v1 else v1

-- * usefull computation data

-- | This datastructure contains data needed to compute the color of a point in
-- space.
data (OrdRealFloat a, Color c) => RayHit a c = Hit
    { hit_position :: SpaceVect a   -- ^ The position of the point in the scene.
    , hit_incoming :: SpaceVect a   -- ^ The direction of the casting ray
    , hit_normal   :: SpaceVect a   -- ^ The normal of the surface where the point is.
    , hit_material :: Material  a c -- ^ The material of the object at the intersection.
    }



-- Let's say Ray = (origin, direction)
type Ray a = (SpaceVect a, SpaceVect a)

transf_ray :: (OrdRealFloat a) => Trans a -> Ray a -> Ray a
transf_ray t (orig,dir) = (transf_evaluate t orig, transf_evaluate_linear t dir)


type Range a = (a,a)

isInRange :: (Num a, Ord a) => a -> Range a -> Bool
isInRange = isInInterval

-- * Traversal of 'KDTree's.
-- ** A few intersection utilities

intersectionWithPlane :: Fractional a => Ray a -> Plane a -> (a, SpaceVect a)
intersectionWithPlane (ray_orig, ray_dir) plane@(Plane orientation cst) =
    let
          SpaceVect(ax,ay,az) = planeVect plane
          (SpaceVect(bx,by,bz), SpaceVect(cx,cy,cz)) = baseVects orientation
          SpaceVect(dx,dy,dz) = ray_dir
          SpaceVect(ox,oy,oz) = ray_orig
          cramer_matrix = Mat3( bx, cx, -dx
                              , by, cy, -dy
                              , bz, cz, -dz
                              )
          cramer_vect   = SpaceVect(ox-ax,oy-ay,oz-az)
          SpaceVect(beta,gamma,t) = cramer cramer_matrix cramer_vect
    in
          (t, buildVectorAlong plane (beta, gamma))






epsilonBox (Box(SpaceVect(ax,ay,az),SpaceVect(bx,by,bz))) =
    Box( SpaceVect ( ax-epsilon , ay-epsilon , az-epsilon )
       , SpaceVect ( bx+epsilon , by+epsilon , bz+epsilon )
       )

hitsBoxAt :: OrdRealFloat a => Ray a -> Box a -> Maybe (a, SpaceVect a)
hitsBoxAt ray box@(Box(b1,b2)) =
    f_extremum min_fst
        $ prepend_intersection inter_bx1 YZ
        $ prepend_intersection inter_by1 XZ
        $ prepend_intersection inter_bz1 XY
        $ prepend_intersection inter_bx2 YZ
        $ prepend_intersection inter_by2 XZ
        $ prepend_intersection inter_bz2 XY
        $ []
    where 
        ebox = epsilonBox box
        prepend_intersection tp@(t,p) orientation list =
            if isInBoxRectangle (getter_contr orientation) p box
                    && t `isInRange` full_range
                then tp:list
                else list
        inter_bx1 = intersectionWithPlane ray $ Plane YZ $ x b1
        inter_by1 = intersectionWithPlane ray $ Plane XZ $ y b1
        inter_bz1 = intersectionWithPlane ray $ Plane XY $ z b1
        inter_bx2 = intersectionWithPlane ray $ Plane YZ $ x b2
        inter_by2 = intersectionWithPlane ray $ Plane XZ $ y b2
        inter_bz2 = intersectionWithPlane ray $ Plane XY $ z b2


hitsBox :: OrdRealFloat a => Ray a -> Box a -> Bool
hitsBox ray = (/=Nothing).(hitsBoxAt ray)










-- ** Traversal of kdtrees

currentNode :: (OrdRealFloat a, Color c) => KDTree a c -> SpaceVect a -> Maybe (KDTree a c)
currentNode (KDBranch plane positiveChild negativeChild) p =
    if p `isOnPositiveSideOf` plane
    then currentNode positiveChild p
    else currentNode negativeChild p
currentNode (KDRoot box child) p =
    if p `isInBox` box
    then currentNode child p
    else Nothing
currentNode leaf@(KDLeaf _ _)  p = Just leaf


isInRectangle (Plane ori _) = isInBoxRectangle (getter_contr ori)


getEveryBoxIntersections :: (OrdRealFloat a, Color c) => KDTree a c -> Ray a -> [(Box a, [Shape a c])]
getEveryBoxIntersections kdtree@(KDRoot box _) ray =
    (map snd)$(sortBy compare_fst)$(getEveryBoxIntersections' kdtree ray box)
compare_fst :: Ord a => (a,b) -> (a,b) -> Ordering
compare_fst (a1,b1) (a2,b2) = compare a1 a2

getEveryBoxIntersections' :: (OrdRealFloat a, Color c) 
    => KDTree a c 
    -> Ray a
    -> Box a
    -> [(a, (Box a, [Shape a c]))]
getEveryBoxIntersections' kdtree ray@(orig,dir) currentBox =
    let
        hit = ray `hitsBoxAt` currentBox
        (proceed,t) = case hit of
            Nothing -> (orig `isInBox` currentBox, 0)
            Just(t',_) -> (True, t')
    in
        if proceed then
            case kdtree of
                KDRoot box child -> getEveryBoxIntersections' child ray box
                KDBranch plane positiveChild negativeChild ->
                    (getEveryBoxIntersections' positiveChild ray $ splitPositiveBox plane currentBox)
                    ++ (getEveryBoxIntersections' negativeChild ray $ splitNegativeBox plane currentBox)
                KDLeaf box shapes -> [(t,(box,shapes))]
        else
            []

