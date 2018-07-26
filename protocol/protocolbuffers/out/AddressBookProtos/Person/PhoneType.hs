module AddressBookProtos.Person.PhoneType (PhoneType(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data PhoneType = MOBILE
               | HOME
               | WORK
               deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable PhoneType
 
instance P'.Bounded PhoneType where
  minBound = MOBILE
  maxBound = WORK
 
instance P'.Default PhoneType where
  defaultValue = MOBILE
 
toMaybe'Enum :: P'.Int -> P'.Maybe PhoneType
toMaybe'Enum 0 = P'.Just MOBILE
toMaybe'Enum 1 = P'.Just HOME
toMaybe'Enum 2 = P'.Just WORK
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum PhoneType where
  fromEnum (MOBILE) = 0
  fromEnum (HOME) = 1
  fromEnum (WORK) = 2
  toEnum
   = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type AddressBookProtos.Person.PhoneType") . toMaybe'Enum
  succ (MOBILE) = HOME
  succ (HOME) = WORK
  succ _ = P'.error "hprotoc generated code: succ failure for type AddressBookProtos.Person.PhoneType"
  pred (HOME) = MOBILE
  pred (WORK) = HOME
  pred _ = P'.error "hprotoc generated code: pred failure for type AddressBookProtos.Person.PhoneType"
 
instance P'.Wire PhoneType where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB PhoneType
 
instance P'.MessageAPI msg' (msg' -> PhoneType) PhoneType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum PhoneType where
  reflectEnum = [(0, "MOBILE", MOBILE), (1, "HOME", HOME), (2, "WORK", WORK)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".tutorial.Person.PhoneType") [] ["AddressBookProtos", "Person"] "PhoneType")
      ["AddressBookProtos", "Person", "PhoneType.hs"]
      [(0, "MOBILE"), (1, "HOME"), (2, "WORK")]