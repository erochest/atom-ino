
module Main where

import Data.Word
import Language.Atom
import System.Directory

ledPin :: Word8
ledPin = 13

motorPin :: Word8
motorPin = 9

high :: Word8
high = 255

low :: Word8
low = 0

digitalWrite :: UE -> UE -> Atom ()
digitalWrite pin level = action dw [pin, level]
    where dw [l, p] = "digitalWrite(" ++ p ++ "," ++ l ++ ")"
          dw _      = ""

vue :: V a -> UE
vue = UVRef . uv

blink :: Atom ()
blink = do
        pin        <- word8 "pin"        ledPin
        level_low  <- word8 "level_low"  low
        level_high <- word8 "level_high" high

        period ph $ phase 0 $ atom "lp" $ do
            phase 0 $ atom "blink_on" $
                digitalWrite (vue pin) (vue level_high)
            phase (ph `quot` 8) $ atom "blink_off" $
                digitalWrite (vue pin) (vue level_low)

        where ph = 80000

motor :: Atom ()
motor = do
        pin        <- word8 "pin"        motorPin
        level_low  <- word8 "level_low"  low
        level_high <- word8 "level_high" high
        let writePin = digitalWrite (vue pin)

        period ph $ phase 0 $ atom "motor" $ do
            phase 0 $ atom "motor_on" $
                writePin $ vue level_high
            phase onTime $ atom "motor_off" $
                writePin $ vue level_low

        where onTime  = 2500
              offTime = 100000
              ph      = onTime + offTime

main :: IO ()
main = do
    (sch, _, _, _, _) <- compile atomName defaults { cCode = prePostCode } motor
    putStrLn $ reportSchedule sch
    renameFile (atomName ++ ".c") (atomName ++ ".pde")
    where
        atomName = "AtomIno"
        varInit t vr val = cType t ++ " " ++ vr ++ " = " ++ val ++ ";"
        prePostCode _ _ _ =
            ( varInit Int16 "ledPin" $ show ledPin
            , unlines [ "void setup() {"
                      , " pinMode(ledPin, OUTPUT);"
                      , "}"
                      , ""
                      , "void loop() {"
                      , "  " ++ atomName ++ "();"
                      , "}"
                      ]
            )


