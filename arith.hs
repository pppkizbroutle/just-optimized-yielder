import Data.List
import Test.QuickCheck


mk_var joyDepth joyWidth = concat <$> sequence [(: []) <$> chooseEnum ('a', 'z'), chooseInt (0,2) >>= (concat <$>) . (`vectorOf` (oneof [oneof [(: []) <$> chooseEnum ('a', 'z'),(: []) <$> chooseEnum ('A', 'Z')], show <$> chooseInt (0,9)]))]

mk_lambda joyDepth joyWidth = concat <$> sequence [return "\\", chooseInt (1,joyWidth) >>= (concat <$>) . (`vectorOf` (concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_var` joyWidth), return " "])), return "-> ", chooseInt (0,joyDepth - 1) >>= (`mk_expression` joyWidth)]

mk_arithmetic joyDepth joyWidth | joyDepth <= 0 = oneof [(show . abs) <$> (arbitrary :: Gen Float)]
mk_arithmetic joyDepth joyWidth = oneof [concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic_operator` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [return "(", chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), return ")"]]

mk_arithmetic_operator joyDepth joyWidth = oneof [return " + ", return " - ", return " * ", return " / "]

mk_boolean joyDepth joyWidth | joyDepth <= 0 = oneof [return "True", return "False"]
mk_boolean joyDepth joyWidth = oneof [concat <$> sequence [return "not ", chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_boolean_operator` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth)], concat <$> sequence [chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_comparison_operator` joyWidth), chooseInt (0,joyDepth - 1) >>= (`mk_arithmetic` joyWidth)], concat <$> sequence [return "(", chooseInt (0,joyDepth - 1) >>= (`mk_boolean` joyWidth), return ")"]]

mk_boolean_operator joyDepth joyWidth = oneof [return " && ", return " || "]

mk_comparison_operator joyDepth joyWidth = oneof [return " == ", return " <= ", return " >= ", return " < ", return " > "]

mk_expression joyDepth joyWidth = oneof [chooseInt (0,joyDepth) >>= (`mk_boolean` joyWidth), chooseInt (0,joyDepth) >>= (`mk_arithmetic` joyWidth), chooseInt (0,joyDepth) >>= (`mk_lambda` joyWidth)]
