module Parser
    where


import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle

import Shortnames
import Util
import Math

import qualified Data.Map as Map
import qualified Data.HashTable as HashTable
import qualified Data.Ix as Ix
import IO
import qualified Data.Maybe as Maybe
import Debug.Trace


parse string = runX $ xunpickleDocument xpXML_World [withTrace 0, withRemoveWS yes, withPreserveComment no] string


-- These data structures are very similar to those in the *.sdl file. They do
-- not interpret anything, just store the sdl file in them.

-- | This is the top node of the xml tree. It corresponds to the @<Sdl>@
-- element. It simply contains references to sub elements, no else data is
-- stored. Click on subelements type to know what they represent.
data XML_World = XML_World
    { xml_world_cameras   :: XML_Cameras
    , xml_world_lights    :: XML_Lights
    , xml_world_geometries:: XML_Geometries
    , xml_world_materials :: XML_Materials
    , xml_world_scene     :: XML_Scene
    }
    deriving (Show,Eq)

-- | Shorcut for a list of camera. We could use arrays or balanced trees to
-- implement it, but lists are just fine. Moving from list in another data
-- structure will break the use of xpList in the 'xpickle' instance of 'XML_World'.
-- This is the only function to rename. Possibly, we could rename it
-- xpContainer, and define it to match either lists, arrays or balanced trees
-- ('FiniteMap') depending on our choice of implementation.
type XML_Cameras = [XML_Camera]
-- | A camera of the scene

-- | The camera node. Every argument are mendatory. Since no data is actually
-- processed in the XML data structure, every argument is a String. See 'xxxx'
-- and 'Util.Camera' for the actual processed named field.
data XML_Camera = XML_Camera
    { xml_cam_position  :: String
        -- ^ String representation of 'cam_position',the plane where the screen
        -- is positionned.
    , xml_cam_direction :: String
        -- ^ String representation of 'cam_u' (but possibly not normalized).
    , xml_cam_up        :: String
        -- ^ String representation of the up vector, which indicates where is
        -- the up of the camera. When projected on the screen plane and
        -- normalized (this in fact the Gramm-Schmidt algorithm), then it is the
        -- 'cam_v' vector.
    , xml_cam_fovy      :: String
        -- ^ String representing the \"field of view\", i.e. the angle formed by
        -- the two edges going from the eye (see 'cam_position') and the the left
        -- and right extremities of the screen.
    , xml_cam_antialiasing :: Maybe String
    , xml_cam_focus     :: Maybe String
    , xml_cam_lens      :: Maybe String
    , xml_cam_dof       :: Maybe String
    , xml_cam_name      :: String
        -- ^ a unique name to the camera, used for choosing the used camera for
        -- the render in the xml file.
    }
    deriving (Show,Eq)

-- | Shortcut to implement a list of lights. See 'XML_Cameras'
type XML_Lights = [XML_Light]
-- | This data structures corresponds to the several light source available in
-- the xml. @XML_Point@ correspond to the basic point light source, and
-- @XML_Directional@ is the global illumination with respect to direction,
-- sometimes called the \"sun\" light source.
data XML_Light = XML_Point
    { xml_light_position  :: String
        -- ^ String representation of the position of the point light.
    , xml_light_intensity :: Maybe String
        -- ^ The intensity of the light. The argument is facultative. If not
        -- set, then 'xml_light_intensity' is set to Nothing. Beware, @Nothing@
        -- and the empty @String@ have different meanings. @Just \"\"@ simply means
        -- that the argument is specified in the xml file, but with an empty
        -- value.  Nothing means the argument is not present.
    , xml_light_color     :: Maybe String
        -- ^ String representation of a color. If not set, then its color should
        -- be 'def_light'.
    , xml_light_castShadow:: Maybe String
    , xml_light_name      :: String
        -- ^ A unique name to the light, used to list the active lights.
    }
    | XML_Directional
    { xml_light_direction :: String

        -- ^ String representation of the direction of the sun light.
    , xml_light_intensity :: Maybe String
        -- ^ The intensity of the light. The argument is facultative. If not
        -- set, then 'xml_light_intensity' is set to Nothing. Beware, @Nothing@
        -- and the empty @String@ have different meanings. @Just \"\"@ simply means
        -- that the argument is specified in the xml file, but with an empty
        -- value.  Nothing means the argument is not present.
    , xml_light_color     :: Maybe String
        -- ^ String representation of a color. If not set, then its color should
        -- be 'def_light'.
    , xml_light_castShadow:: Maybe String
    , xml_light_name      :: String
        -- ^ A unique name to the light, used to list the active lights.
    }
    | XML_Spot
    { xml_light_position  :: String
    , xml_light_direction :: String
    , xml_light_angle     :: String
    , xml_light_intensity :: Maybe String
    , xml_light_color     :: Maybe String
    , xml_light_castShadow:: Maybe String
    , xml_light_name      :: String
    }
    | XML_Area
    { xml_light_position  :: String
    , xml_light_direction :: String
    , xml_light_up        :: String
    , xml_light_dimensions:: String
    , xml_light_density   :: Maybe String
    , xml_light_intensity :: Maybe String
    , xml_light_color     :: Maybe String
    , xml_light_castShadow:: Maybe String
    , xml_light_name      :: String
    }
    | XML_Ambient
    { xml_light_intensity :: Maybe String
    , xml_light_color     :: Maybe String
    , xml_light_castShadow:: Maybe String
    , xml_light_name      :: String
    }
    deriving (Show,Eq)


-- | Shortcut to implement a list of geometries. See 'XML_Cameras'
type XML_Geometries = [XML_Geometry]
-- | Geometry correspond of the shape of objects. It could not be named
-- 'XML_Shape', since \'shape\' is already used name for the actual scene
-- objects.
--
-- Mesh are represented in a indexed mode, where verticed are listed separatly,
-- and triangles are listed in triple of indices of the corresponding vertex.
data XML_Geometry = XML_TriangleIdx
    { xml_geom_coordinates   :: String
        -- ^ The list of vertex, in a @String@ representation.
    , xml_geom_normals       :: Maybe String
        -- ^ String representation of the list of normals of each vertex. These
        -- normal are independant of the vertex list. rather, a triangle will
        -- defined its vertex by indices and for each vertex, will defined its
        -- normal by another -- index. For instance,
        --
        -- @
        -- xml_geom_coordinates="0 3.5 1, 0 3.5 -1, 0 2.5 1"
        -- xml_geom_normals="1 0 0, 0 0 1, 0 1 0"
        -- xml_geom_coordinateInd="0, 1, 2"
        -- xml_geom_normalInd="0, 2, 1"
        -- @
        --
        -- defines a triangle with vertex at position (0,3.5,-1) with normal
        -- (0,1,0)
    , xml_geom_coordinateInd :: String
        -- ^ String representation of the list of triangles. Every triple of
        -- indices correspond to a triangle, list one after another. It is /not/
        -- a strip or a fan triangle representation. For instance to have two
        -- triangles, you need to write six indices, to have three, you need to
        -- write nine, ...
    , xml_geom_normalInd     :: Maybe String
        -- ^ String representation of the list of triangles. The @n@th index
        -- written correspond to the normal of the triangle containing the
        -- @n@thi vertex index in 'xml_geom_coordinateInd'. See
        -- 'xml_geom_normals'.
    , xml_geom_name          :: String
        -- ^ A unique name to the geometry, so that the shapes can link them in
        -- the xml file.
    }
    | XML_TriangleFile
    { xml_geom_file         :: String
        -- ^ A filename of an @*.obj@ file. See obj file format specification on
        -- your own, but they are very similar to those of 'XML_TriangleIdx'.
    , xml_geom_name         :: String
        -- ^ A unique name to the geometry, so that the shapes can link them in
        -- the xml file.
    }
    | XML_Sphere
    { xml_geom_radius       :: String
    , xml_geom_name         :: String
    }
    | XML_Plane
    { xml_geom_ori          :: Maybe String
    , xml_geom_cst          :: Maybe String
    , xml_geom_name         :: String
    }
    | XML_Box
    { xml_geom_vect1        :: String
    , xml_geom_vect2        :: String
    , xml_geom_name         :: String
    }
    deriving (Show,Eq)

-- | Shortcut to implement a list of materials. See 'XML_Cameras'.
-- Since the whole section is facultative, the list is contained in a 'Maybe'
-- type. 'Nothing' is equivalent to an empty list, but is not set for simplicity
-- reasons. Actually 'Nothing' means the whole @<Material>@ section is missing
-- whereas @Just []@ meas the section is present but empty.
type XML_Materials = Maybe [XML_Material]

-- | Strings representation of severals material.
data XML_Material = XML_LinearCombined
    { xml_mat_linearFactor    :: String
        -- ^ This should be a String representation of the proportion of merging
        -- the first material. The second proportion is specified in the xml
        -- file, but is ignored, since it is @1-weight1@
    , xml_mat_1               :: String
        -- ^ The name of the first material to merge with.
    , xml_mat_2               :: String
        -- ^ The name of the second material to merge.
    , xml_mat_name            :: String
        -- ^ A unique name to the geometry, so that the shapes can link them in
        -- the xml file.
    }
    | XML_Diffuse
    { xml_mat_color           :: String
        -- ^ String representation of the color of the material. It is mendatory
        -- since the default material is set to a Diffule material with some
        -- default color. If you want not to specify the color, do not specify
        -- the material.
    , xml_mat_name            :: String
        -- ^ A unique name to the geometry, so that the shapes can link them in
        -- the xml file.
    }
    | XML_Unlit
    { xml_mat_maybe_color     :: Maybe String
    , xml_mat_name            :: String
    }
    | XML_Phong
    { xml_mat_color           :: String
    , xml_mat_shininess       :: String
    , xml_mat_name            :: String
    }
    | XML_CheckerCombined
    { xml_mat_dimensions      :: Maybe String
    , xml_mat_1               :: String
        -- ^ The name of the first material to merge with.
    , xml_mat_2               :: String
        -- ^ The name of the second material to merge.
    , xml_mat_name            :: String
        -- ^ A unique name to the geometry, so that the shapes can link them in
        -- the xml file.
    }
    deriving (Show,Eq)


-- | The actual data of the scene.
data XML_Scene = XML_Scene
    { xml_sc_camera     :: String
        -- ^ The name of the active camera. See 'XML_Camera'.
    , xml_sc_lights     :: String
        -- ^ A list of names of the active lights from the light list. See
        -- 'XML_Light'
    , xml_sc_background :: Maybe String
        -- ^ String representation of the color to be returned when a ray do not
        -- intersect with a geometry. In other words, it is the color of the
        -- sky. Argument is facultative, by default, it is set to 'def_background'.
    , xml_sc_nodes      :: [XML_SceneNode]
        -- ^ This is the scene graph. There is no root element, since the root
        -- element must no transform any object in space. See 'XML_SceneNode'
    }
    deriving (Show,Eq)

-- | Scene graph Tree, where inner nodes are either one of the basic
-- transformations, and its leafs are the actual shapes.
data XML_SceneNode = XML_Translate
    { xml_aff_transVect  :: String
        -- ^ String representation of the translation vector.
    , xml_aff_children   :: [XML_SceneNode]
        -- ^ List of children, which are commited to this affine transformation
        -- as well.
    }
    |XML_Scale
    { xml_aff_scaleFactor:: String
        -- ^ String representation of the scale factor. Scaling in done with the
        -- same factor along every axis, hence is represented by only one
        -- factor.
    , xml_aff_children   :: [XML_SceneNode]
        -- ^ List of children, which are commited to this affine transformation
        -- as well.
    }
    |XML_Rotate
    { xml_aff_rotAxis    :: String
        -- ^ String representation of the axis of the rotation.
    , xml_aff_rotAngle   :: String
        -- ^ String representation of the angle of the rotation.
    , xml_aff_children   :: [XML_SceneNode]
        -- ^ List of children, which are commited to this affine transformation
        -- as well.
    }
    | XML_Leaf XML_Shape
        -- ^ Store data of a leaf in another structure, since it might be a bit
        -- complex.
    deriving (Show,Eq)


-- A Shape is simply a link to a geometry and a material. Since it belongs in
-- the scene graph, its affine transformations are not stored in it. @Shape@ is
-- not a well choosen name, but since the original project was to be written in
-- java, \"Object\" would have been awful.
data XML_Shape = XML_Shape
    { xml_sh_geometry :: String
        -- ^ Name of the geometry of the object. See 'XML_Geometry'.
    , xml_sh_material :: Maybe String
        -- ^ Name of the material of the object. See 'XML_Material'.
    }
    deriving (Show,Eq)


-- instances
instance XmlPickler XML_World where
    xpickle = xpXML_World
instance XmlPickler XML_Camera where
    xpickle = xpXML_Camera
instance XmlPickler XML_Light where
    xpickle = xpXML_Light
instance XmlPickler XML_Geometry where
    xpickle = xpXML_Geometry
instance XmlPickler XML_Material where
    xpickle = xpXML_Material
instance XmlPickler XML_Scene where
    xpickle = xpXML_Scene
instance XmlPickler XML_Shape where
    xpickle = xpXML_Shape
instance XmlPickler XML_SceneNode where
    xpickle = xpXML_SceneNode

-- picklers to build the xml tree


-- | Pickler for 'XML_World'. Convert a @<Sdl>@ node in the XML_World data format.
-- See URL for details.
xpXML_World = xpElem "Sdl"
    $ xpWrap
    ( uncurry5 XML_World
    , \t -> (xml_world_cameras t, xml_world_lights t, xml_world_geometries t, xml_world_materials t, xml_world_scene t)
    )
    $ xp5Tuple
               (xpElem "Cameras"   xpickle)
               (xpElem "Lights"    xpickle)
               (xpElem "Geometry"  xpickle)
               (xpOption $ xpElem "Materials" xpickle)
               (xpickle) -- "Scene" is a data already instance of "XmlPickler"

-- | Pickler for 'XML_Camera'. Convert a @<Camera/>@ node in the XML_Camera data format.
-- See URL for details.
xpXML_Camera :: PU XML_Camera
xpXML_Camera = xpElem "Camera"
    $ xpWrap
    ( uncurry9 XML_Camera
    , \t -> (xml_cam_position t
            , xml_cam_direction t
            , xml_cam_up t
            , xml_cam_fovy t
            , xml_cam_antialiasing t
            , xml_cam_focus t
            , xml_cam_lens t
            , xml_cam_dof t
            , xml_cam_name t
            )
    )
    $ xp9Tuple(xpAttr        "position"  xpText)
              (xpAttr        "direction" xpText)
              (xpAttr        "up"        xpText)
              (xpAttr        "fovy"      xpText)
              (xpAttrImplied "osa"       xpText)
              (xpAttrImplied "focus"     xpText)
              (xpAttrImplied "lens"      xpText)
              (xpAttrImplied "dof"       xpText)
              (xpAttr        "name"      xpText)

-- | Pickler for 'XML_Light'. Convert a @<PointLight/>@ or a
-- @<DirectionalLight/>@ node in the XML_Light data format.
-- See URL for details.
xpXML_Light :: PU XML_Light
xpXML_Light = xpAlt indexer xpLight
    where indexer (XML_Point       _ _ _ _ _        ) = 0
          indexer (XML_Directional _ _ _ _ _        ) = 1
          indexer (XML_Spot        _ _ _ _ _ _ _    ) = 2
          indexer (XML_Area        _ _ _ _ _ _ _ _ _) = 3
          indexer (XML_Ambient     _ _ _ _          ) = 4
          xpLight =
              [ xpElem "PointLight"
                $ xpWrap (uncurry5 XML_Point
                , \t -> ( xml_light_position t
                        , xml_light_intensity t
                        , xml_light_color t
                        , xml_light_castShadow t
                        , xml_light_name t))
                $ xp5Tuple(xpAttr        "position"  xpText)
                          (xpAttrImplied "intensity" xpText)
                          (xpAttrImplied "color"     xpText)
                          (xpAttrImplied "cast_shadow" xpText)
                          (xpAttr        "name"      xpText)
              , xpElem "DirectionalLight"
                $ xpWrap (uncurry5 XML_Directional
                , \t -> ( xml_light_direction t
                        , xml_light_intensity t
                        , xml_light_color t
                        , xml_light_castShadow t
                        , xml_light_name t))
                $ xp5Tuple(xpAttr        "direction" xpText)
                          (xpAttrImplied "intensity" xpText)
                          (xpAttrImplied "color"     xpText)
                          (xpAttrImplied "cast_shadow" xpText)
                          (xpAttr        "name"      xpText)
              , xpElem "SpotLight"
                $ xpWrap (uncurry7 XML_Spot
                , \t -> ( xml_light_position t
                        , xml_light_direction t
                        , xml_light_angle t
                        , xml_light_intensity t
                        , xml_light_color t
                        , xml_light_castShadow t
                        , xml_light_name t))
                $ xp7Tuple(xpAttr        "position"  xpText)
                          (xpAttr        "direction" xpText)
                          (xpAttr        "angle"     xpText)
                          (xpAttrImplied "intensity" xpText)
                          (xpAttrImplied "color"     xpText)
                          (xpAttrImplied "cast_shadow" xpText)
                          (xpAttr        "name"      xpText)
              , xpElem "AreaLight"
                $ xpWrap (uncurry9 XML_Area
                , \t -> ( xml_light_position t
                        , xml_light_direction t
                        , xml_light_up t
                        , xml_light_dimensions t
                        , xml_light_density t
                        , xml_light_intensity t
                        , xml_light_color t
                        , xml_light_castShadow t
                        , xml_light_name t))
                $ xp9Tuple(xpAttr        "position"  xpText)
                          (xpAttr        "direction" xpText)
                          (xpAttr        "up"        xpText)
                          (xpAttr        "dimensions" xpText)
                          (xpAttrImplied "density"   xpText)
                          (xpAttrImplied "intensity" xpText)
                          (xpAttrImplied "color"     xpText)
                          (xpAttrImplied "cast_shadow" xpText)
                          (xpAttr        "name"      xpText)
              , xpElem "AmbientLight"
                $ xpWrap (uncurry4 XML_Ambient
                , \t -> ( xml_light_intensity t
                        , xml_light_color t
                        , xml_light_castShadow t
                        , xml_light_name t))
                $ xp4Tuple(xpAttrImplied "intensity" xpText)
                          (xpAttrImplied "color"     xpText)
                          (xpAttrImplied "cast_shadow" xpText)
                          (xpAttr        "name"      xpText)
              ]

-- | Pickler for 'XML_Material'. Convert a @<Material/>@ node in the XML_Material data format.
-- See URL for details.
xpXML_Material :: PU XML_Material
xpXML_Material = xpAlt indexer xpMat
    where indexer (XML_LinearCombined  _ _ _ _) = 0
          indexer (XML_Diffuse         _ _    ) = 1
          indexer (XML_Unlit           _ _    ) = 2
          indexer (XML_Phong           _ _ _  ) = 3
          indexer (XML_CheckerCombined _ _ _ _) = 4
          xpMat =
              [ xpElem "LinearCombinedMaterial"
                $ xpWrap (uncurry4 XML_LinearCombined
                , \t -> (xml_mat_linearFactor t, xml_mat_1 t, xml_mat_2 t, xml_mat_name t))
                $ xp4Tuple(xpAttr "weight1"   xpText)
                          (xpAttr "material1" xpText)
                          (xpAttr "material2" xpText)
                          (xpAttr "name"      xpText)
              , xpElem "DiffuseMaterial"
                $ xpWrap (uncurry XML_Diffuse
                , \t -> (xml_mat_color t, xml_mat_name t))
                $ xpPair  (xpAttr "color" xpText)
                          (xpAttr "name"  xpText)
              , xpElem "UnlitMaterial"
                $ xpWrap (uncurry XML_Unlit
                , \t -> (xml_mat_maybe_color t, xml_mat_name t))
                $ xpPair  (xpAttrImplied "color" xpText)
                          (xpAttr        "name"  xpText)
              , xpElem "PhongMaterial"
                $ xpWrap (uncurry3 XML_Phong
                , \t -> (xml_mat_color t, xml_mat_shininess t, xml_mat_name t))
                $ xpTriple(xpAttr "color"     xpText)
                          (xpAttr "shininess" xpText)
                          (xpAttr "name"      xpText)
              , xpElem "CheckerCombinedMaterial"
                $ xpWrap (uncurry4 XML_CheckerCombined
                , \t -> (xml_mat_dimensions t, xml_mat_1 t, xml_mat_2 t, xml_mat_name t))
                $ xp4Tuple(xpAttrImplied "dimensions" xpText)
                          (xpAttr        "material1"  xpText)
                          (xpAttr        "material2"  xpText)
                          (xpAttr        "name"       xpText)
              ]


-- | Pickler for 'XML_Geometry'. Convert a @<FileGeometry/>@ or a
-- @<IndexedTriangleSet/>@ node in the XML_Geometry data format.
-- See URL for details.
xpXML_Geometry :: PU XML_Geometry
xpXML_Geometry = xpAlt indexer xpGeom
    where indexer (XML_TriangleIdx  _ _ _ _ _) = 0
          indexer (XML_TriangleFile _ _      ) = 1
          indexer (XML_Sphere       _ _      ) = 2
          indexer (XML_Plane        _ _ _    ) = 3
          indexer (XML_Box          _ _ _    ) = 4
          xpGeom =
              [ xpElem "IndexedTriangleSet"
                $ xpWrap (uncurry5 XML_TriangleIdx
                , \t -> (xml_geom_coordinates t, xml_geom_normals t, xml_geom_coordinateInd t, xml_geom_normalInd t, xml_geom_name t))
                $ xp5Tuple(xpAttr        "coordinates"       xpText)
                          (xpAttrImplied "normals"           xpText)
                          (xpAttr        "coordinateIndices" xpText)
                          (xpAttrImplied "normalIndices"     xpText)
                          (xpAttr        "name"              xpText)
              , xpElem "FileGeometry"
                $ xpWrap (uncurry XML_TriangleFile
                , \t -> (xml_geom_file t, xml_geom_name t))
                $ xpPair  (xpAttr "filename" xpText)
                          (xpAttr "name"     xpText)
              , xpElem "Sphere"
                $ xpWrap (uncurry XML_Sphere
                , \t -> (xml_geom_radius t, xml_geom_name t))
                $ xpPair  (xpAttr "radius"   xpText)
                          (xpAttr "name"     xpText)
              , xpElem "Plane"
                $ xpWrap (uncurry3 XML_Plane
                , \t ->  (xml_geom_ori t, xml_geom_cst t, xml_geom_name t))
                $ xpTriple (xpAttrImplied "orientation" xpText)
                           (xpAttrImplied "cst"         xpText)
                           (xpAttr        "name"        xpText)
              , xpElem "Box"
                $ xpWrap (uncurry3 XML_Box
                , \t ->  (xml_geom_vect1 t, xml_geom_vect2 t, xml_geom_name t))
                $ xpTriple (xpAttr "from_vector" xpText)
                           (xpAttr "to_vector"   xpText)
                           (xpAttr "name"        xpText)
              ]

-- | Pickler for 'XML_Scene'. Convert the @<Scene/>@ node in the XML_Scene data format.
-- See URL for details.
xpXML_Scene :: PU XML_Scene
xpXML_Scene = xpElem "Scene"
    $ xpWrap
    ( uncurry4 XML_Scene
    , \t -> (xml_sc_camera t, xml_sc_lights t, xml_sc_background t, xml_sc_nodes t)
    )
    $ xp4Tuple(xpAttr        "camera"     xpText)
              (xpAttr        "lights"     xpText)
              (xpAttrImplied "background" xpText)
              (xpList $ xpickle)

-- | Pickler for 'XML_Shape'. Convert the @<Shape/>@ node in the XML_Shape data format.
-- See URL for details.
xpXML_Shape :: PU XML_Shape
xpXML_Shape = xpElem "Shape"
    $ xpWrap
    ( uncurry XML_Shape
    , \shape -> (xml_sh_geometry shape, xml_sh_material shape)
    )
    $ xpPair (xpAttr        "geometry" xpText)
             (xpAttrImplied "material" xpText)



-- | Pickler for 'XML_SceneNode'. Convert a @<Translate/>@, @<Rotate/>@ or a
-- @<Scale/>@ node in the XML_SceneNode data format.
-- See URL for details.
xpXML_SceneNode :: PU XML_SceneNode
xpXML_SceneNode = xpAlt indexer xpAff
    where indexer (XML_Translate _ _) = 0
          indexer (XML_Scale     _ _) = 1
          indexer (XML_Rotate  _ _ _) = 2
          indexer (XML_Leaf        _) = 3
          xpAff =
              [ xpElem "Translate"
                $ xpWrap ( uncurry XML_Translate , \t -> (xml_aff_transVect t, xml_aff_children t))
                $ xpPair (xpAttr "vector" xpText)
                         (xpList $ xpickle)
              , xpElem "Scale"
                $ xpWrap ( uncurry XML_Scale  , \t -> (xml_aff_scaleFactor t, xml_aff_children t))
                $ xpPair (xpAttr "scale" xpText)
                         (xpList $ xpickle)
              , xpElem "Rotate"
                $ xpWrap ( uncurry3 XML_Rotate , \t -> (xml_aff_rotAxis t, xml_aff_rotAngle t, xml_aff_children t) )
                $ xpTriple (xpAttr "axis"  xpText)
                           (xpAttr "angle" xpText)
                           (xpList $ xpickle)
              , xpWrap ( XML_Leaf, \(XML_Leaf leaf) -> leaf)
                $ xpickle
              ]





















-- | transform a string of three numeric values in a 'SpaceVect'.
-- The numeric values are separated by spaces. Trailing or suffix spaces are
-- allowed.
readSpaceVect :: String -> SpaceVect Float
readSpaceVect string = SpaceVect (value 0, value 1, value 2)
    where wls = words string
          value i = read (wls!!i)

readPlaneVect :: String -> PlaneVect Float
readPlaneVect string = PlaneVect (value 0, value 1)
    where wls = words string
          value i = read (wls!!i)

    
    
-- | transform a string of three numeric value in a 'RGBColor'
-- The numeric values are separated by spaces. Trailing or suffix spaces are
-- allowed.
readRGB :: String -> RGBColor
readRGB string = RGB (value 0, value 1, value 2)
    where wls = words string
          value i = read (wls!!i)

-- | Similar to 'Data.Maybe.fromMaybe'.
--If a is @Nothing@, the default value is given.
--Else, if a is @Just stuff@, the @stuff@ is given.
readIfDefined :: a -> (String -> a) -> Maybe String -> a
readIfDefined default_value _ Nothing = default_value
readIfDefined _ readFun (Just word)   = readFun word



-- | Convert a 'XML_Camera' in an actual 'Camera'.
parseCamera :: (Integer, Integer) -> XML_Camera -> Camera Float
parseCamera (n@(nx, ny)) XML_Camera 
                        {xml_cam_position = position_word
                        ,xml_cam_direction   = direction_word
                        ,xml_cam_up          = up_word
                        ,xml_cam_fovy        = fovy_word
                        ,xml_cam_antialiasing= m_antialiasing_word
                        ,xml_cam_focus       = m_focus_word
                        ,xml_cam_lens        = m_lens_word
                        ,xml_cam_dof         = m_dof_word
                        ,xml_cam_name        = name
                        }
    = Camera
      { cam_position = position
      , cam_u = u
      , cam_v = v
      , cam_w = w
      , cam_focus_dist = focus_dist
      , cam_screenRect = (PlaneVect(negate sx, negate sy),PlaneVect(2*sx, 2*sy))
      , cam_n = n
      , cam_antialiasing = readIfDefined 0 read m_antialiasing_word
      , cam_lens = readIfDefined (PlaneVect(sx/2,sy/2)) readPlaneVect m_lens_word
      , cam_dof  = readIfDefined False read m_dof_word
      , cam_name = name
      }
    where up       = readSpaceVect up_word
          position = readSpaceVect position_word
          direction= readSpaceVect direction_word
          theta    = toRadian $ (read fovy_word)/2

          w = normed $ negateVect direction
          v = normed $ up @-@ ((up@|@w).*@ w) --Gramm-Schmidt method. since w is normed, there is no need to divide by normSquared w
          u = v @*@ w --No needs to normalize, since w and v are already normed

          -- Amount of pixels in the window. That value should definitivly
          -- be passed as an argument to that function and to the program.
          focus_dist = readIfDefined 1 read m_focus_word

          sx = focus_dist * (tan theta)
          sy = sx * (i_ ny) / (i_ nx)

-- | Convert a 'XML_Light' in an actual 'Light'.
parseLight :: XML_Light -> Light Float RGBColor
parseLight
    XML_Point { xml_light_position = position_word
              , xml_light_intensity = intensity_word
              , xml_light_color = color_word
              , xml_light_castShadow = castShadow_word
              , xml_light_name = name
              }
    =
    Point {light_position = readSpaceVect position_word
          ,light_color = intensity .*# ( readIfDefined def_lightColor readRGB color_word )
          ,light_castShadow = readIfDefined True read castShadow_word
          ,light_name = name
          }
              where intensity = readIfDefined 1 read intensity_word
parseLight
    XML_Directional { xml_light_direction = direction_word
                    , xml_light_intensity = intensity_word
                    , xml_light_color = color_word
                    , xml_light_castShadow = castShadow_word
                    , xml_light_name = name
                    }
    =
    Directional {light_direction = normed $ readSpaceVect direction_word
                ,light_color = intensity .*# ( readIfDefined def_lightColor readRGB color_word )
                ,light_castShadow = readIfDefined True read castShadow_word
                ,light_name = name
                }
                    where intensity = readIfDefined 1 read intensity_word
parseLight XML_Spot
    { xml_light_position   = p_word
    , xml_light_direction  = d_word
    , xml_light_angle      = a_word
    , xml_light_intensity  = m_i_word
    , xml_light_color      = m_c_word
    , xml_light_castShadow = m_sh_word
    , xml_light_name       = name
    }
    = Spot
        { light_position   = readSpaceVect p_word
        , light_direction  = normed $ readSpaceVect d_word
        , light_cosAngle   = cos $ toRadian $ (/2) $read a_word
        , light_color      = intensity .*# ( readIfDefined def_lightColor readRGB m_c_word)
        , light_castShadow = readIfDefined True read m_sh_word
        , light_name       = name
        }
            where intensity = readIfDefined 1 read m_i_word

parseLight XML_Area
    { xml_light_position   = p_word
    , xml_light_direction  = d_word
    , xml_light_up         = up_word
    , xml_light_dimensions = dim_word
    , xml_light_density    = dens_word
    , xml_light_intensity  = m_i_word
    , xml_light_color      = m_c_word
    , xml_light_castShadow = m_sh_word
    , xml_light_name       = name
    }
    = Area
        { light_position   = p
        , light_u          = u
        , light_v          = v
        , light_direction  = d
        , light_dims       = (down_left, dims)
        , light_density    = readIfDefined 4 read dens_word
        , light_color      = intensity .*# ( readIfDefined def_lightColor readRGB m_c_word)
        , light_castShadow = readIfDefined True read m_sh_word
        , light_name       = name
        }
            where intensity = readIfDefined 1 read m_i_word
                  p = readSpaceVect p_word
                  d = normed $ readSpaceVect d_word
                  up= readSpaceVect up_word
                  w = negateVect d
                  v = normed $ up @-@ ((up@|@w).*@ w) --Gramm-Schmidt method. since w is normed, there is no need to divide by normSquared w
                  u = v @*@ w --No needs to normalize, since w and v are already normed
                  dims = readPlaneVect dim_word
                  up_right = 0.5 .*@~ dims
                  down_left = negateVect_ up_right

parseLight XML_Ambient
    { xml_light_intensity  = m_i_word
    , xml_light_color      = m_c_word
    , xml_light_castShadow = m_sh_word
    , xml_light_name       = name
    }
    = Ambient
        { light_color      = intensity .*# ( readIfDefined def_lightColor readRGB m_c_word)
        , light_castShadow = readIfDefined True read m_sh_word
        , light_name       = name
        }
            where intensity = readIfDefined 1 read m_i_word




-- | Convert a 'XML_Geometry' in an actual 'Geometry'. The output @Geometry@ is
-- in the 'IO' Monad, since geometries might come from @*.obj@ files, and thus
-- require some external world dependencies.
parseGeometry :: XML_Geometry -> IO (Geometry Float)
parseGeometry xmlgeom = case xmlgeom of
    XML_TriangleIdx { xml_geom_coordinates = coord_word
    , xml_geom_normals      = m_norm_word
    , xml_geom_coordinateInd= coordidx_word
    , xml_geom_normalInd    = m_normidx_word
    , xml_geom_name         = name
    }
        ->  return TriangleMesh
                { geo_faces    = faces
                , geo_name     = name
                }
            where
                -- "ng" stands for "normal given"
                --
                -- TODO augment complexity performance
                -- poorly efficient code, <IndexedTriangleSet> object
                -- shouldn't be too large
                norm_word = Maybe.fromMaybe "" m_norm_word
                normidx_word = Maybe.fromMaybe "" m_normidx_word
                ng_coord = Map.fromList $ zip [0..] $ map readSpaceVect $ filter (/="") $ splitComa coord_word
                --ng_coord = Map.fromList $ zip [0..] $ map readSpaceVect $ splitComa coord_word
                ng_norm  = Map.fromList $ zip [0..] $ map readSpaceVect $ filter (/="") $ splitComa norm_word
                ng_coordidx  = map read $ splitComa coordidx_word
                ng_normidx   = (map (Just . read) $ filter (/="") $ splitComa normidx_word) ++ nothings
                
                faces = buildFaces (ng_coord,ng_norm,ng_coordidx,ng_normidx)



    XML_TriangleFile {xml_geom_file = filename, xml_geom_name = name }
        ->  do lists <- readOBJ filename
               return TriangleMesh
                   { geo_faces    = buildFaces lists
                   , geo_name     = name
                   }
    XML_Sphere {xml_geom_radius = radius_word, xml_geom_name = name }
        -> return Sphere
                { geo_radius = radius
                , geo_name   = name
                }
           where radius = read radius_word
    XML_Plane {xml_geom_ori = m_ori_word, xml_geom_cst = m_cst_word, xml_geom_name = name }
        -> return Shape_Plane
                { geo_plane = Plane ori cst
                , geo_name  = name
                }
            where ori = case m_ori_word of Nothing       -> XZ
                                           Just ori_word -> read ori_word
                  cst = case m_cst_word of Nothing       -> 0
                                           Just cst_word -> read cst_word
    XML_Box {xml_geom_vect1 = v1_word, xml_geom_vect2 = v2_word, xml_geom_name = name }
        -> return Shape_Box
                { geo_box   = newBox (v1,v2)
                , geo_name  = name
                }
            where v1 = readSpaceVect v1_word
                  v2 = readSpaceVect v2_word

nothings = Nothing:nothings

-- Generate the normal of a triangle
generateNormal :: Floating a => SpaceVect a -> SpaceVect a -> SpaceVect a -> SpaceVect a
generateNormal v1 v2 v3 = normed $ (v3@-@v1) @*@ (v2@-@v3)

-- | Create the list of Faces and vertices from a map of coordinates vectors, a
-- map of normal vectors, a list of index coordinates and a list of normal
-- index.
buildFaces :: ( Map.Map Integer (SpaceVect Float)
              , Map.Map Integer (SpaceVect Float)
              , [Integer], [Maybe Integer])
              -- ^ A 4-tuple of a list of coordinates, a list of normal vectors,
              -- a list of indexes which, onced grouped by three, form a
              -- triangle, and a list for the normal vector indices.
                  -> [Face Float]
buildFaces 
    (coordMap,normMap,coordidx,normidx)
    = let
        genVertex (i, Nothing)= ((coordMap Map.! i),(Nothing))                -- search is in O(log i)+O(log j)
        genVertex (i, Just j) = ((coordMap Map.! i),(Just $ normMap Map.! j)) -- search is in O(log i)+O(log j)
        posNormList= map genVertex $ zip coordidx normidx -- zip is i O(n) and very slow on allocations
    in
        map generateFaceWithNeededNormals $ chunks 3 posNormList --O(n*log n)

generateFaceWithNeededNormals chunk@[(v1,n1),(v2,n2),(v3,n3)] = 
    if n1 == Nothing || n2 == Nothing || n3 == Nothing
    then
        let gnorm = generateNormal v1 v2 v3
            v1'= Vertex v1 gnorm
            v2'= Vertex v2 gnorm
            v3'= Vertex v3 gnorm
        in Triangle v1' v2' v3'
    else
        faceFromList $ map (uncurry Vertex) $ map (Maybe.fromJust . sequenceSnd) chunk
generateFaceWithNeededNormals chunk = error (show chunk)


-- | read an @*.obj@ file, and output its contents as the four lists needed by
-- 'buildFace'.
readOBJ :: FilePath
    -> IO ( Map.Map Integer (SpaceVect Float)
          , Map.Map Integer (SpaceVect Float)
          , [Integer], [Maybe Integer])
readOBJ filepath = do
    putStrLn $ ">> Reading \"" ++ filepath ++ "\" ..."
    bracket (openFile filepath ReadMode) hClose (doReadOBJ (1,1))




-- | The loop of 'read', in global scope, in order not to redefine it every
-- call.
doReadOBJ :: (Integer,Integer) -> Handle
    -> IO ( Map.Map Integer (SpaceVect Float)
          , Map.Map Integer (SpaceVect Float)
          , [Integer], [Maybe Integer])
doReadOBJ idx@(i,j) handle = do
    b <- hIsEOF handle
    if b then return (Map.empty, Map.empty, [],[])
         else do
            lineWords <- (hGetLine handle)
            actualWords <- return $ words lineWords
            if length actualWords == 0 then doReadOBJ idx handle -- skip empty lines
                else let lineName:lineValues = actualWords in
                        case lineName of
                            "v" -> do
                                (v_m, vn_m, vidx_m, vnidx_m) <- doReadOBJ (i+1, j) handle
                                --Debug.Trace.trace ("v:"++ (show (v_m, vn_m, vidx_m, vnidx_m )) ++ show v_h) $
                                return (Map.insert i v_h v_m, vn_m, vidx_m, vnidx_m)
                                    where v_h = (readSpaceVect_word lineValues)
                            "vt"-> do
                                skip <- doReadOBJ (i,j) handle -- skip "vt" lines
                                --Debug.Trace.trace ("vt:"++ show lineWords) $
                                return skip
                            "vn"-> do
                                (v_m, vn_m, vidx_m, vnidx_m) <- doReadOBJ (i, j+1) handle
                                --Debug.Trace.trace ("vn:"++ (show (v_m, vn_m, vidx_m, vnidx_m )) ++ show vn_h) $
                                return (v_m, Map.insert j vn_h vn_m, vidx_m, vnidx_m )
                                    where vn_h = (readSpaceVect_word lineValues)
                            "f" -> do
                                (v_m, vn_m, vidx_m, vnidx_m) <- doReadOBJ idx handle
                                --Debug.Trace.trace ("f:"++ (show (v_m, vn_m, vidx_m, vnidx_m )) ++ show (v_m, vn_m, (getvi 0):(getvi 1):(getvi 2):vidx_m, (getvni 0):(getvni 1):(getvni 2):vnidx_m) ) $
                                return (v_m, vn_m, (getvi 0):(getvi 1):(getvi 2):vidx_m, (getvni 0):(getvni 1):(getvni 2):vnidx_m)
                                    where subwords = map (splitString (=='/')) lineValues
                                          getvi  n = read $ (subwords !! n) !! 0
                                          getvni n = getvni' $ subwords !! n
                                          getvni' words= if length words < 3
                                                         then Nothing -- if normals are unspecified
                                                         else
                                                        Just $ read $ words !! 2
                            -- by default, skip unknown lines

                            _   -> do putStrLn (">>>: warning : ignoring "++show lineWords)
                                      doReadOBJ idx handle


-- | Convert three String represent a number in a 'SpaceVect'. The strings must be
-- given as a list of three elements.
readSpaceVect_word :: [String] -> SpaceVect Float
readSpaceVect_word wls =  SpaceVect (value 0, value 1, value 2)
    where value i = read (wls!!i)



-- | Util function for parseGeometry. The first one is not in a \'where\'
-- clause, in order not to be redefined for every geometry.
faceFromList [v1,v2,v3] = Triangle v1 v2 v3

-- | Split a string into substrings defined by some separator charaters.
-- Long sequence of separators gives empty string in result.
--
-- >>> splitString (==\'/\') "hello//world/"
-- > ["hello","","world",""]
--
splitString :: 
    (Char -> Bool) -- ^ the function to evaluate wheter a character is a separator.
    -> String      -- ^ the string to split.
    -> [String]
splitString p t = loop t
    where loop s | Prelude.null st = [w]
                 | otherwise = w : loop (tail st )
              where (w, st) = break p s
-- | As simple as :
-- 
-- @
-- splitString (==',')
-- @
-- 
-- See 'splitString' for more details.
splitComa :: String -> [String]
splitComa = splitString (==',')

-- | Split a list in chunks of same length n. The last chunk have a length between
-- 1 and n.
chunks :: Integer -> [a] -> [[a]]
chunks n s = loop s
    where loop l = if length l <= i_ n
                   then [l]
                   else let (chunk,queue) = splitAt (i_ n) l
                       in chunk:(loop queue)










-- | This code may duplicate data. Beware also of infinite loops, generated by
-- loop in the combined material dependency graph.
parseMaterial :: Map.Map String XML_Material -> XML_Material -> Material Float RGBColor
parseMaterial matls xmlmat = case xmlmat of
    XML_LinearCombined { xml_mat_linearFactor = comb_value_word
                       , xml_mat_1 = mat1
                       , xml_mat_2 = mat2
                       , xml_mat_name = name}
        -> Combined { mat_combineFunction = \_ -> (read comb_value_word)
                    , mat_1 = parseMaterial matls $ matls Map.! mat1
                    , mat_2 = parseMaterial matls $ matls Map.! mat2
                    , mat_name = name
                    };
    XML_Diffuse { xml_mat_color = color_word
                , xml_mat_name = name
                }
        -> Diffuse { mat_color = readRGB color_word
                   , mat_name  = name
                   }
    XML_Unlit { xml_mat_maybe_color = m_color_word
              , xml_mat_name        = name
              }
        -> Unlit { mat_color = readIfDefined def_matColor readRGB m_color_word
                 , mat_name  = name
                 }
    XML_Phong { xml_mat_color     = color_word
              , xml_mat_shininess = shininess_word
              , xml_mat_name      = name
              }
        -> Phong { mat_color = readRGB color_word
                 , mat_shininess = read shininess_word
                 , mat_name  = name
                 }
    XML_CheckerCombined { xml_mat_dimensions = m_dims_word
                        , xml_mat_1 = mat1
                        , xml_mat_2 = mat2
                        , xml_mat_name = name}
        -> Combined { mat_combineFunction = checker
                    , mat_1 = parseMaterial matls $ matls Map.! mat1
                    , mat_2 = parseMaterial matls $ matls Map.! mat2
                    , mat_name = name
                    }
            where SpaceVect(a,b,c) = readIfDefined (SpaceVect(1,1,1)) readSpaceVect m_dims_word
                  checker (SpaceVect(u,v,w)) =  i_ $ ((a `modParity` u) + (b `modParity` v) + (c `modParity` w)) `mod` 2
                    where modParity a' u' = if (u' `modR` (2*a'))<= a' then 0 else 1

-- | Default material for objects who do not specifies it. It's a Diffuse
-- material with color def_matColor.
default_mat :: (OrdRealFloat a, Color c) => Material a c
default_mat = Diffuse { mat_color = def_matColor
                      , mat_name  = "!:default:"
                      }

-- | Convert a 'XML_Shape' in an actual 'Shape'.
parseShape :: (OrdRealFloat a, Color c) =>
    Trans a -> Map.Map String (Geometry a) -> Map.Map String (Material a c) -> XML_Shape -> Shape a c
parseShape transf geom_map mat_map
    XML_Shape { xml_sh_geometry = geom_name
              , xml_sh_material = mat_name
              }
        = Shape { sh_geometry  = geom_map Map.! geom_name
                , sh_material  = material
                , sh_invTransf = Maybe.fromJust $ transf_invert transf
                , sh_transf    = transf
                }
            where material = case mat_name of Nothing -> default_mat
                                              Just s  -> mat_map  Map.! s


-- | Convert a 'XML_SceneNode' in an actual 'SceneNode'.
parseSceneNode :: Color c => 
    Trans Float -> Map.Map String (Geometry Float) -> Map.Map String (Material Float c) -> XML_SceneNode -> [Shape Float c]
parseSceneNode affTransf gl ml XML_Translate{xml_aff_transVect=trVect, xml_aff_children=children}
    = concat (map (parseSceneNode new_affTransf gl ml) children)
        where new_affTransf = (affine_translate $ readSpaceVect trVect) `compose` affTransf
parseSceneNode affTransf gl ml XML_Scale {xml_aff_scaleFactor=factor, xml_aff_children=children}
    = concat (map (parseSceneNode new_affTransf gl ml) children)
        where new_affTransf = (affine_scale $ read factor) `compose` affTransf
parseSceneNode affTransf gl ml XML_Rotate {xml_aff_rotAxis=axis, xml_aff_rotAngle=angle, xml_aff_children=children}
    = concat (map (parseSceneNode new_affTransf gl ml) children)
        where new_affTransf = (affine_rotate  (readSpaceVect axis) (toRadian $ read angle)) `compose` affTransf
parseSceneNode affTransf gl ml (XML_Leaf shape) = [parseShape affTransf gl ml shape] -- we only copy the pointer not all the affTransf !


-- | Convert Both 'XML_World' and 'XML_Scene' in a 'Scene'. The result is in the
-- IO monad since we may interact with a @*.obj@ file.
parseScene :: (Integer, Integer) -- ^ Dimension of the screen in pixels. The pixels are square ones.
                                 -- The screen ratio is given by the ratio of
                                 -- the pixel screen dimension.
            -> XML_World -- ^ The world to convert. It already contains a 'XML_Scene'.
            -> IO (Scene Float RGBColor)
parseScene (nx, ny) XML_World
        { xml_world_cameras    = xml_cams
        , xml_world_lights     = xml_lights
        , xml_world_geometries = xml_geoms
        , xml_world_materials  = m_xml_mats
        , xml_world_scene      = XML_Scene
            { xml_sc_camera = xml_main_cam
            , xml_sc_lights = xml_active_lights
            , xml_sc_background= xml_back
            , xml_sc_nodes  = xml_nodes
            }
        }
    = let
        cam_map   = Map.fromList [(xml_cam_name camera,  parseCamera (nx, ny) camera) | camera <- xml_cams]
        
        light_map = Map.fromList [(xml_light_name light, parseLight   light) | light  <- xml_lights]
        active_lightnames :: [String]
        active_lightnames = map removedBorderSpaces $ splitComa xml_active_lights
        lights    = Map.filterWithKey (\k _ -> k `elem` active_lightnames) light_map
        
        xml_mats  =(justList $ m_xml_mats)
        xml_mapWithNames = Map.fromList [(xml_mat_name xmat, xmat) | xmat <- xml_mats]

        mat_map   = Map.map (parseMaterial xml_mapWithNames) xml_mapWithNames
        --mat_map   = Map.map (parseMaterial mat_map) xml_mapWithNames
    in do
        geom_list <- sequence $ map sequenceSnd [(xml_geom_name geom, parseGeometry geom) | geom  <- xml_geoms]
        let geom_map  = Map.fromList geom_list in
            return Scene
                { sc_camera = cam_map Map.! xml_main_cam
                , sc_lights = Map.elems lights
                , sc_background = readIfDefined def_background readRGB xml_back
                , sc_shapes = concat $ map (parseSceneNode affine_id geom_map mat_map) xml_nodes
                }
        
-- | remove the spaces at the beginning and the end of a string.
removedBorderSpaces = (fst).(break (==' ')).snd.(span (==' '))

-- | The signature is self-explanatory : convert a Tuple of an element and a
-- action in an action of a Tuple.
sequenceSnd :: Monad m => (a,m b) -> m (a,b)
sequenceSnd (alpha, beta) = do
    beta' <- beta
    return (alpha, beta')


---- | Get the list of a @Maybe list@. If the argument is set to @Nothing@, return
---- an empty list.
justList :: Maybe [a] -> [a]
justList Nothing = []
justList (Just list) = list





























-- | As simple as :
--
-- @
-- uncurry5 fun (x1,x2,x3,x4,x5) = fun x1 x2 x3 x4 x5
-- @
uncurry5 :: (a->b->c->d->e->sol) -> (a,b,c,d,e) -> sol
uncurry5 fun (x1,x2,x3,x4,x5) = fun x1 x2 x3 x4 x5
-- | As simple as :
--
-- @
-- uncurry6 fun (x1,x2,x3,x4,x5,x6) = fun x1 x2 x3 x4 x5 x6
-- @
uncurry6 :: (a->b->c->d->e->f->sol) -> (a,b,c,d,e,f) -> sol
uncurry6 fun (x1,x2,x3,x4,x5,x6) = fun x1 x2 x3 x4 x5 x6
-- | As simple as :
--
-- @
-- uncurry7 fun (x1,x2,x3,x4,x5,x6,x7) = fun x1 x2 x3 x4 x5 x6 x7
-- @
uncurry7 :: (a->b->c->d->e->f->g->sol) -> (a,b,c,d,e,f,g) -> sol
uncurry7 fun (x1,x2,x3,x4,x5,x6,x7) = fun x1 x2 x3 x4 x5 x6 x7
-- | As simple as :
--
-- @
-- uncurry8 fun (x1,x2,x3,x4,x5,x6,x7,x8) = fun x1 x2 x3 x4 x5 x6 x7 x8
-- @
uncurry8 :: (a->b->c->d->e->f->g->h->sol) -> (a,b,c,d,e,f,g,h) -> sol
uncurry8 fun (x1,x2,x3,x4,x5,x6,x7,x8) = fun x1 x2 x3 x4 x5 x6 x7 x8
uncurry9 :: (a->b->c->d->e->f->g->h->i->sol) -> (a,b,c,d,e,f,g,h,i) -> sol
uncurry9 fun (x1,x2,x3,x4,x5,x6,x7,x8,x9) = fun x1 x2 x3 x4 x5 x6 x7 x8 x9

