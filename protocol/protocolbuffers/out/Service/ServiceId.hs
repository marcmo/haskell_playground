module Service.ServiceId (ServiceId(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ServiceId = ServiceId{id :: P'.Int32, instId :: P'.Maybe P'.Int32}
               deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable ServiceId where
  mergeEmpty = ServiceId P'.mergeEmpty P'.mergeEmpty
  mergeAppend (ServiceId x'1 x'2) (ServiceId y'1 y'2) = ServiceId (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ServiceId where
  defaultValue = ServiceId P'.defaultValue P'.defaultValue
 
instance P'.Wire ServiceId where
  wireSize ft' self'@(ServiceId x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeOpt 1 5 x'2)
  wirePut ft' self'@(ServiceId x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 5 x'1
             P'.wirePutOpt 16 5 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> P'.fmap (\ new'Field -> old'Self{id = new'Field}) (P'.wireGet 5)
             16 -> P'.fmap (\ new'Field -> old'Self{instId = P'.Just new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ServiceId) ServiceId where
  getVal m' f' = f' m'
 
instance P'.GPB ServiceId
 
instance P'.ReflectDescriptor ServiceId where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".service.ServiceId\", haskellPrefix = [], parentModule = [MName \"Service\"], baseName = MName \"ServiceId\"}, descFilePath = [\"Service\",\"ServiceId.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".service.ServiceId.id\", haskellPrefix' = [], parentModule' = [MName \"Service\",MName \"ServiceId\"], baseName' = FName \"id\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".service.ServiceId.instId\", haskellPrefix' = [], parentModule' = [MName \"Service\",MName \"ServiceId\"], baseName' = FName \"instId\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"