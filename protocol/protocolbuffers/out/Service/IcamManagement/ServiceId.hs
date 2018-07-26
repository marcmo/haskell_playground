module Service.IcamManagement.ServiceId (ServiceId(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ServiceId = SERVICE_ID_ICAMMANAGEMENT
               deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable ServiceId
 
instance P'.Bounded ServiceId where
  minBound = SERVICE_ID_ICAMMANAGEMENT
  maxBound = SERVICE_ID_ICAMMANAGEMENT
 
instance P'.Default ServiceId where
  defaultValue = SERVICE_ID_ICAMMANAGEMENT
 
toMaybe'Enum :: P'.Int -> P'.Maybe ServiceId
toMaybe'Enum 50 = P'.Just SERVICE_ID_ICAMMANAGEMENT
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum ServiceId where
  fromEnum (SERVICE_ID_ICAMMANAGEMENT) = 50
  toEnum = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type Service.IcamManagement.ServiceId") . toMaybe'Enum
  succ _ = P'.error "hprotoc generated code: succ failure for type Service.IcamManagement.ServiceId"
  pred _ = P'.error "hprotoc generated code: pred failure for type Service.IcamManagement.ServiceId"
 
instance P'.Wire ServiceId where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB ServiceId
 
instance P'.MessageAPI msg' (msg' -> ServiceId) ServiceId where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum ServiceId where
  reflectEnum = [(50, "SERVICE_ID_ICAMMANAGEMENT", SERVICE_ID_ICAMMANAGEMENT)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".service.icamManagement.ServiceId") [] ["Service", "IcamManagement"] "ServiceId")
      ["Service", "IcamManagement", "ServiceId.hs"]
      [(50, "SERVICE_ID_ICAMMANAGEMENT")]