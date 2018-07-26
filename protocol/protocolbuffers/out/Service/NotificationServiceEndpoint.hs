module Service.NotificationServiceEndpoint (NotificationServiceEndpoint(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Service.ServiceId as Service (ServiceId)
 
data NotificationServiceEndpoint = NotificationServiceEndpoint{service :: Service.ServiceId}
                                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable NotificationServiceEndpoint where
  mergeEmpty = NotificationServiceEndpoint P'.mergeEmpty
  mergeAppend (NotificationServiceEndpoint x'1) (NotificationServiceEndpoint y'1)
   = NotificationServiceEndpoint (P'.mergeAppend x'1 y'1)
 
instance P'.Default NotificationServiceEndpoint where
  defaultValue = NotificationServiceEndpoint P'.defaultValue
 
instance P'.Wire NotificationServiceEndpoint where
  wireSize ft' self'@(NotificationServiceEndpoint x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 11 x'1)
  wirePut ft' self'@(NotificationServiceEndpoint x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> P'.fmap (\ new'Field -> old'Self{service = P'.mergeAppend (service old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> NotificationServiceEndpoint) NotificationServiceEndpoint where
  getVal m' f' = f' m'
 
instance P'.GPB NotificationServiceEndpoint
 
instance P'.ReflectDescriptor NotificationServiceEndpoint where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".service.NotificationServiceEndpoint\", haskellPrefix = [], parentModule = [MName \"Service\"], baseName = MName \"NotificationServiceEndpoint\"}, descFilePath = [\"Service\",\"NotificationServiceEndpoint.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".service.NotificationServiceEndpoint.service\", haskellPrefix' = [], parentModule' = [MName \"Service\",MName \"NotificationServiceEndpoint\"], baseName' = FName \"service\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".service.ServiceId\", haskellPrefix = [], parentModule = [MName \"Service\"], baseName = MName \"ServiceId\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"