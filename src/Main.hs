module Main
    where

import Shortnames
import Util
import Displayer
import Parser
import RayTracer
import Data.Maybe(fromMaybe)
import Data.Char(isDigit)
import System.FilePath.Posix(splitFileName)
import System.Directory(getCurrentDirectory, setCurrentDirectory)
import System.Console.GetOpt
import System.Environment
import System.Exit

-- | Runs the programs. parse arguments, display help if requested, and then
-- process each given file.
main :: IO ()
main = do
    args <- getArgs
    (givenFlags, files) <- compileOpts args
    if HELP `elem` givenFlags
        then do
            putStr helpMessage
            exitSuccess
        else let opts = processOptions givenFlags
            in do
                putStrLn $ "> " ++ (show $ length files) ++ " file(s) to process"
                sequence_ $ map (processFile opts) files

-- | Contains unique data about options that may be given in command-line
data Opts = Opts
    { dim :: (Integer,Integer)
    , useKDTree :: Bool
    , renderKDTree :: Bool
    , m_file    :: Maybe String
    }
-- | Convert the parsed options to 'Opts', last option takes precedence on
-- others
processOptions = processOptions'
    (Opts
    { dim=(64,64)
    , useKDTree=True
    , renderKDTree=False
    , m_file=Nothing
    })
processOptions' cval [] = cval
processOptions' cval ((DIMENSION (nx,ny) ):list) = processOptions' (cval{dim=(nx,ny)})       list
processOptions' cval ((KDTREE bool):list)        = processOptions' (cval{useKDTree=bool})    list
processOptions' cval ((RENDER_KDTREE bool):list) = processOptions' (cval{renderKDTree=bool}) list
processOptions' cval ((FILENAME m_string):list)  = processOptions' (cval{m_file=m_string})   list
processOptions' cval (_:list) = processOptions' cval list



processFile opts filename = 
    let (dir',file) = splitFileName filename 
        dir = if dir'== "" then "." else dir'
    in do
        putStrLn $ "> Rendering \"" ++ filename ++ "\""
        oldDir <- getCurrentDirectory
        setCurrentDirectory dir
        w <- parse file
        if w == [] 
            then error "could not parse scene."
            else return ()
        s <- parseScene (dim opts) $ head w
        img <- (if (renderKDTree opts)
            then do
                s' <- return $ sceneToKDScene s
                return $ traceFullBox s'
            else if (useKDTree opts)
                then do
                    s'<- return $ sceneToKDScene s
                    return $ traceFullScene s'
                else
                    return $ traceFullScene s
            )
        setCurrentDirectory oldDir
        display (m_file opts) img

-- | Data structure that contains every possible flags for the program.
data Flag = HELP
          | DIMENSION     (Integer, Integer)
          | KDTREE        Bool
          | RENDER_KDTREE Bool
          | FILENAME      (Maybe String)
          deriving (Eq, Show)

flags =
    [ Option ['h','?']      ["help"]            (NoArg  HELP)                       "Display this help message."
    , Option ['d']          ["dim"]             (ReqArg getDim "<W>x<H>")           "Set the image dimension in pixels. (default: 64x64 )"
    , Option []             ["with-kdtree"]     (NoArg  (KDTREE True))              "Enable the use of a kdTree. (default)"
    , Option []             ["no-kdtree"]       (NoArg  (KDTREE False))             "Disable the use of a kdTree."
    , Option []             ["render-kdtree"]   (NoArg(RENDER_KDTREE True))
        "Render the external box of the kdTree. Black pixels are missed pixels. Grey and white ones have a ray that transverse the tree."
    , Option ['o']          ["output-file","save-as"]
                                                (ReqArg (FILENAME . Just) "<FILE>")   "Do not display render, instead, save it to the bmp image file <FILE>."
    , Option ['s']          ["output-window","display-on-screen"]
                                                (NoArg  (FILENAME Nothing))         "Render image in a window. (default)"
    ]
getDim string =
    let (xStr, tmp) = span isDigit string
        (_, yStr) = break isDigit tmp
    in DIMENSION (read xStr, read yStr)

-- | Read arguments and output a tuple of flags and actual arguments.
compileOpts :: [String] -> IO ([Flag],[String])
compileOpts args =
    case getOpt Permute flags args of
          (o,no,[]  ) -> return (o,no)
          (_,_,errs)  -> ioError (userError (concat errs ++ "" ++ "Use -h to display help." ++ "\n"))

helpMessage = usageInfo "Usage: ingi2325-nbrack [OPTION...] FILE...    where FILE is the file to process." flags
