data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = div alphabetSize 2
          offset = fromEnum c + halfAlphabet
          rotation = mod offset alphabetSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder alphabetSize c = toEnum rotation
    where halfN = div alphabetSize 2
          offset = if even alphabetSize
                    then fromEnum c + halfN
                    else 1 + fromEnum c + halfN
          rotation = mod offset alphabetSize


largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar char = rotN sizeOfAlphabet char
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

rotCharDecoder char = rotNdecoder sizeOfAlphabet char
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

rotEncoder :: String -> String
rotEncoder text = map rotChar text

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text

type Bits = [Bool]

intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
                then False : intToBits' nextVal
                else True : intToBits' nextVal
    where remainder = mod n 2
          nextVal = div n 2

maxBits = length (intToBits' (maxBound :: Int))

intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])

charToBits char = intToBits (fromEnum char)

bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
    where size = length bits
          indices = [size-1, size-2 .. 0]
          trueLocations = filter (\x -> fst x == True)
                            (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))
xorPair (v1,v2) = xorBool v1 v2
xor list1 list2 = map xorPair (zip list1 list2)

myPad = "Shhhhhh"
myPlainText = "Haskell"

applyOTP' pad plainText = map (\pair -> xor (fst pair) (snd pair))
                            (zip padBits plaintextBits)
    where padBits = map charToBits pad
          plaintextBits = map charToBits plainText


applyOTP pad plaintext = map bitsToChar bitList
    where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String
instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text