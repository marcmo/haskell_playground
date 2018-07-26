module AddressBookProtos.Person (Person(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified AddressBookProtos.Person.PhoneNumber as AddressBookProtos.Person (PhoneNumber)
 
data Person = Person{name :: P'.Utf8, id :: P'.Int32, email :: P'.Maybe P'.Utf8,
                     phone :: P'.Seq AddressBookProtos.Person.PhoneNumber}
            deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable Person where
  mergeEmpty = Person P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (Person x'1 x'2 x'3 x'4) (Person y'1 y'2 y'3 y'4)
   = Person (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default Person where
  defaultValue = Person P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Person where
  wireSize ft' self'@(Person x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeRep 1 11 x'4)
  wirePut ft' self'@(Person x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'1
             P'.wirePutReq 16 5 x'2
             P'.wirePutOpt 26 9 x'3
             P'.wirePutRep 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> P'.fmap (\ new'Field -> old'Self{name = new'Field}) (P'.wireGet 9)
             16 -> P'.fmap (\ new'Field -> old'Self{id = new'Field}) (P'.wireGet 5)
             26 -> P'.fmap (\ new'Field -> old'Self{email = P'.Just new'Field}) (P'.wireGet 9)
             34 -> P'.fmap (\ new'Field -> old'Self{phone = P'.append (phone old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Person) Person where
  getVal m' f' = f' m'
 
instance P'.GPB Person
 
instance P'.ReflectDescriptor Person where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16, 26, 34])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".tutorial.Person\", haskellPrefix = [], parentModule = [MName \"AddressBookProtos\"], baseName = MName \"Person\"}, descFilePath = [\"AddressBookProtos\",\"Person.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".tutorial.Person.name\", haskellPrefix' = [], parentModule' = [MName \"AddressBookProtos\",MName \"Person\"], baseName' = FName \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".tutorial.Person.id\", haskellPrefix' = [], parentModule' = [MName \"AddressBookProtos\",MName \"Person\"], baseName' = FName \"id\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".tutorial.Person.email\", haskellPrefix' = [], parentModule' = [MName \"AddressBookProtos\",MName \"Person\"], baseName' = FName \"email\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".tutorial.Person.phone\", haskellPrefix' = [], parentModule' = [MName \"AddressBookProtos\",MName \"Person\"], baseName' = FName \"phone\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".tutorial.Person.PhoneNumber\", haskellPrefix = [], parentModule = [MName \"AddressBookProtos\",MName \"Person\"], baseName = MName \"PhoneNumber\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"