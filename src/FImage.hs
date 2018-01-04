import Data.Foldable as F

import qualified Data.FImage.BMP             as BMP
import qualified Data.FImage.Image           as Image
import qualified Data.FImage.Color           as Color
import qualified Data.FImage.BImage.Simple   as BImage.Simple
import qualified Data.FImage.BImage.Gallery   as BImage.Gallery
import qualified Data.FImage.View            as View
import qualified Data.FImage.Window          as Window
import qualified Data.FImage.Render          as Render


write :: Render.Render a => View.View -> Window.Window -> (String, Image.Image a) -> IO ()
write v w (filename, i) = do
  let bmp = BMP.bmp w v i
  BMP.write filename bmp


main :: IO ()
main = do
  let v   = View.mk0 8 8
  let w   = Window.mk 256 256

  let superCross = Image.toColor (BImage.Simple.cross 2) Color.green Color.yellow
  let superTunnel = Image.toColor BImage.Gallery.timeTunnel Color.pink Color.red

  let gradient = Image.gradient 0 Color.blue Color.white
  let oblivion = Image.gradient 3.0 Color.green Color.black
  let sun = Image.gradient 0.5 Color.red Color.yellow

  F.mapM_ (write v w) [
                        ("colored-cross.bmp", superCross),
                        ("colored-tunnel.bmp", superTunnel),
                        ("sun.bmp", sun),
                        ("gradient.bmp", gradient),
                        ("oblivion.bmp", oblivion)
                        ]
