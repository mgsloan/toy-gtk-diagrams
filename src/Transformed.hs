import Graphics.UI.Gtk.Toy.Prelude
import Graphics.UI.Gtk.Toy.Slider
import Graphics.UI.Gtk.Toy.Transformed
import Data.Dynamic
import Control.Newtype (pack)

sli :: Transformed (Slider Cairo R2 Double)
sli = mkTransformed $ mkSlider (0, 1) (circle 5) (r2 (0, 100))
-- tog = mkTransformed $ mkToggle (circle 5) (0, 10)

main = runToy $ sli ||| sli ||| sli ||| sli ||| sli ||| sli
