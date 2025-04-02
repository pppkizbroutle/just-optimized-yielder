import Data.List
import Test.QuickCheck


mk_arithmetic 0 joyWidth = oneof [return "x"]
mk_arithmetic joyDepth joyWidth = oneof [concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " + ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " - ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " * ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " / ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [return "(", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return ")"]]

mk_boolean 0 joyWidth = oneof [return "b"]
mk_boolean joyDepth joyWidth = oneof [concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth), return " & ", chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth), return " | ", chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " = ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " <= ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return " >= ", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [return "(", chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth), return ")"]]

mk_expression joyDepth joyWidth = oneof [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth)]
