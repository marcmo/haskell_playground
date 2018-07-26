module Service.AckVoid (AckVoid(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data AckVoid = AckVoid{}
             deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable AckVoid where
  mergeEmpty = AckVoid
  mergeAppend (AckVoid) (AckVoid) = AckVoid
 
instance P'.Default AckVoid where
  defaultValue = AckVoid
 
instance P'.Wire AckVoid where
  wireSize ft' self'@(AckVoid)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = 0
  wirePut ft' self'@(AckVoid)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.return ()
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> AckVoid) AckVoid where
  getVal m' f' = f' m'
 
instance P'.GPB AckVoid
 
instance P'.ReflectDescriptor AckVoid where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".service.AckVoid\", haskellPrefix = [], parentModule = [MName \"Service\"], baseName = MName \"AckVoid\"}, descFilePath = [\"Service\",\"AckVoid.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"