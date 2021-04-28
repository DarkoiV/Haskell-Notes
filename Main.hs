import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf

type Hz = Float
type Samples = Float
type Octave = Float
type Seconds = Float
type Wave = [Float]
type Beats = Float


data Note
  = C
  | Cs
  | D
  | Ds
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | As
  | B
  deriving (Eq, Show)

-- Default output file path
outputFilePath :: FilePath
outputFilePath = "output"

samplingRate :: Samples
samplingRate = 48000

volume :: Float
volume = 0.2

period :: Float
period = 2 * pi

noteFreq :: Note -> Octave -> Hz
noteFreq n o
  | n == C  = 16.35 * (2 ** o) 
  | n == Cs = 17.32 * (2 ** o)
  | n == D  = 18.35 * (2 ** o)
  | n == Ds = 19.45 * (2 ** o)
  | n == E  = 20.60 * (2 ** o)
  | n == F  = 21.83 * (2 ** o)
  | n == Fs = 23.12 * (2 ** o)
  | n == G  = 24.50 * (2 ** o)
  | n == Gs = 25.96 * (2 ** o)
  | n == A  = 27.50 * (2 ** o)
  | n == As = 29.14 * (2 ** o)
  | n == B  = 30.87 * (2 ** o)

-- Calculate duration of beat from bpm
beatDuration :: Beats -> Seconds
beatDuration bpm = 60.0 / bpm

-- Create sound wave of note in octave, duration in beats
note :: Note -> Octave -> Beats -> Beats -> Wave
note n o b bpm = map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output 
  where 
    step = (( (noteFreq n o) * period) / samplingRate)

    attack :: Wave
    attack = map (min 1.0) [0.0, 0.001 ..]

    release :: Wave
    release = reverse $ take (length output) attack

    output :: Wave
    output = map sin $ map (* step) [0.0 .. samplingRate * (b * beatDuration bpm)]

-- Create melody from list of notes
melody :: Beats -> [(Note, Octave, Beats)] -> Wave
melody bpm noteList = concat $ map (\(x,y,z) -> note x y z bpm) noteList

-- Sand notes ;-)
sand :: [(Note, Octave, Beats)]
sand = 
  [ (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.50)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.50)
  , (D, 5, 0.25)
  , (D, 5, 0.25)
  , (D, 5, 0.25)
  , (D, 5, 0.25)
  , (D, 5, 0.25)
  , (D, 5, 0.25)
  , (D, 5, 0.50)
  , (C, 5, 0.25)
  , (C, 5, 0.25)
  , (C, 5, 0.25)
  , (C, 5, 0.25)
  , (C, 5, 0.25)
  , (C, 5, 0.25)
  , (C, 5, 0.50)
  , (G, 4, 0.50)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.25)
  , (A, 4, 0.50)
  ]

-- Save melody to file
save :: [(Note, Octave, Beats)] -> Beats -> FilePath -> IO ()
save noteList bpm filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE $ (melody bpm noteList)

-- Play sand melody
play :: [(Note, Octave, Beats)] -> Beats -> IO ()
play noteList bpm = do
  save noteList bpm outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" samplingRate outputFilePath
  return ()
