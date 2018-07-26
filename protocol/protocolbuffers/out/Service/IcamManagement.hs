module Service.IcamManagement (protoInfo, fileDescriptorProto) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto)
import Text.ProtocolBuffers.Reflections (ProtoInfo)
import qualified Text.ProtocolBuffers.WireMessage as P' (wireGet,getFromBS)
 
protoInfo :: ProtoInfo
protoInfo
 = P'.read
    "ProtoInfo {protoMod = ProtoName {protobufName = FIName \".service.icamManagement\", haskellPrefix = [], parentModule = [MName \"Service\"], baseName = MName \"IcamManagement\"}, protoFilePath = [\"Service\",\"IcamManagement.hs\"], protoSource = \"icamManagement.proto\", extensionKeys = fromList [], messages = [], enums = [EnumInfo {enumName = ProtoName {protobufName = FIName \".service.icamManagement.ServiceId\", haskellPrefix = [], parentModule = [MName \"Service\",MName \"IcamManagement\"], baseName = MName \"ServiceId\"}, enumFilePath = [\"Service\",\"IcamManagement\",\"ServiceId.hs\"], enumValues = [(EnumCode {getEnumCode = 50},\"SERVICE_ID_ICAMMANAGEMENT\")]}], knownKeyMap = fromList []}"
 
fileDescriptorProto :: FileDescriptorProto
fileDescriptorProto
 = P'.getFromBS (P'.wireGet 11)
    (P'.pack
      "h\n\DC4icamManagement.proto\DC2\SYNservice.icamManagement\SUB\fcommon.proto**\n\tServiceId\DC2\GS\n\EMSERVICE_ID_ICAMMANAGEMENT\DLE2")