{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import Apecs
import Apecs.Gloss
import ApecsHintDemo
import Language.Haskell.Interpreter
--import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import Linear (V2 (..))
import System.Exit


initialize :: System World ()
initialize = do
  newEntity_ (Position 0, Velocity 20)
  newEntity_ (Position (-200), Velocity (V2 100 20))
  newEntity_ (Position (-100), Velocity (V2 80 40), Flying)

translate' :: V2 Float -> Picture -> Picture
translate' (V2 x y) = translate x y

draw :: System World Picture
draw = do
  foldDraw $ \(Position p) -> translate' p $ color white $ circle 10

interpret' :: String -> IO (Either InterpreterError (System World ()))
interpret' code = do
  -- 'runInterpreter' should work just fine with @stack run@.
  -- to use @cabal run@, first run @cabal repl -v@ to see which @-package-db@
  -- flags cabal is sending to ghc, then pass them to hint using
  -- 'unsafeRunInterpreterWithArgs', like this:
  --unsafeRunInterpreterWithArgs ["-package-db", "/home/gelisam/.cabal/store/ghc-8.8.4/package.db", "-package-db", "/home/gelisam/working/haskell/apecs-hint-demo/dist-newstyle/packagedb/ghc-8.8.4", "-package-db", "/home/gelisam/working/haskell/apecs-hint-demo/dist-newstyle/build/x86_64-linux/ghc-8.8.4/apecs-hint-demo-0.1.0.0/package.conf.inplace"] $ do
  runInterpreter $ do
    setImports ["Prelude", "Apecs", "ApecsHintDemo", "Linear"]
    interpret code (as :: System World ())

indentLines :: String -> String
indentLines = unlines . fmap ("  " ++) . lines

handleEvent :: Event -> System World ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = do
  liftIO exitSuccess
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) = do
  allLines <- liftIO $ readFile "new-entity"
  let doBlock = "do\n" ++ indentLines allLines
  liftIO (interpret' doBlock) >>= \case
    Right systemAction -> do
      systemAction
    Left e -> do
      liftIO $ putStrLn $ "while interpreting the following code:"
      liftIO $ putStrLn $ indentLines doBlock
      liftIO $ putStrLn $ "the following error was encountered:"
      liftIO $ putStrLn $ show e
handleEvent _ = do
  pure ()

applyVelocity :: Float -> Velocity -> Position -> Position
applyVelocity dt (Velocity v) (Position p)
  = Position (p + realToFrac dt * v)

gravity :: Float -> Velocity -> Velocity
gravity dt (Velocity v)
  = Velocity (v - realToFrac dt * V2 0 100)

bounce :: Position -> Velocity -> Velocity
bounce (Position (V2 x y)) (Velocity (V2 dx dy))
  = Velocity (V2 (bounce1D (-310) 310 x dx) (bounce1D (-230) 230 y dy))

bounce1D :: Float -> Float -> Float -> Float -> Float
bounce1D minVal maxVal val deltaVal
  | val <= minVal
    = abs deltaVal
  | val >= maxVal
    = negate (abs deltaVal)
  | otherwise
    = deltaVal

step :: Float -> System World ()
step dt = do
  -- 1. Apply gravity to non-flying entities
  -- 2. Add velocity to position
  -- 3. Bounce on window borders
  cmap $ \(v, _ :: Not Flying) -> gravity dt v
  cmap $ \(p, v) -> applyVelocity dt v p
  cmap $ \(p, v) -> bounce p v

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "Bouncing Balls" (640, 480) (10, 10)) black 60 draw handleEvent step
